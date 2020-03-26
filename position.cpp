/*
  Stockfish, a UCI chess playing engine derived from Glaurung 2.1
  Copyright (C) 2004-2008 Tord Romstad (Glaurung author)
  Copyright (C) 2008-2015 Marco Costalba, Joona Kiiski, Tord Romstad
  Copyright (C) 2015-2016 Marco Costalba, Joona Kiiski, Gary Linscott, Tord Romstad

  Stockfish is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  Stockfish is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#include <algorithm>
#include <cassert>
#include <cstring>   // For std::memset, std::memcmp
#include <iomanip>
#include <sstream>
#include <iostream>

#include "bitboard.h"
#include "misc.h"
#include "movegen.h"
#include "position.h"
#include "thread.h"
#include "tt.h"
#include "uci.h"

using std::string;

namespace Zobrist {

  Sleutel64 psq[STUK_N][VELD_N];
  Sleutel64 enpassant[LIJN_N];
  Sleutel64 rokade[ROKADE_MOGELIJK_N];
  Sleutel64 aanZet;
  Sleutel64 hash_50move[32];
}

Sleutel64 Stelling::uitzondering_sleutel(Zet zet) const { 
	return Zobrist::psq[W_KONING][van_veld(zet)] ^ Zobrist::psq[Z_KONING][naar_veld(zet)];
	//return zet;
}

Sleutel64 Stelling::remise50_sleutel() const {
	return Zobrist::hash_50move[st->remise50Zetten >> 2];
}


namespace {

const string PieceToChar(" KPNBRQ  kpnbrq");

} // namespace


void Stelling::bereken_schaak_penningen() const {

	bereken_penningen<WIT>();
	bereken_penningen<ZWART>();
	Veld veldK = veld<KONING>(~aanZet);
	st->schaakVelden[PION]  = aanval_van<PION>(veldK, ~aanZet);
	st->schaakVelden[PAARD] = aanval_van<PAARD>(veldK);
	st->schaakVelden[LOPER] = aanval_van<LOPER>(veldK);
	st->schaakVelden[TOREN] = aanval_van<TOREN>(veldK);
	st->schaakVelden[DAME]  = st->schaakVelden[LOPER] | st->schaakVelden[TOREN];
	st->schaakVelden[KONING] = 0;
}

template <Kleur c>
void Stelling::bereken_penningen() const {

	Bitboard penningGevers, result = 0;
	Veld veldK = veld<KONING>(c);

	penningGevers = (LegeAanval[TOREN][veldK] & stukken(~c, DAME, TOREN))
		          | (LegeAanval[LOPER][veldK] & stukken(~c, DAME, LOPER));

	while (penningGevers)
	{
		Veld veld = pop_lsb(&penningGevers);
		Bitboard b = bb_tussen(veldK, veld) & stukken();

		if (b && !meer_dan_een(b)) {
			result |= b;
			st->penning_door[lsb(b)] = veld;
		}
	}
	st->xRay[c] = result;
}


/// operator<<(Position) returns an ASCII representation of the position

std::ostream& operator<<(std::ostream& os, const Stelling& pos) {

  os << "\n +---+---+---+---+---+---+---+---+\n";

  for (Rij r = RIJ_8; r >= RIJ_1; --r)
  {
      for (Lijn f = LIJN_A; f <= LIJN_H; ++f)
          os << " | " << PieceToChar[pos.stuk_op_veld(maak_veld(f, r))];

      os << " |\n +---+---+---+---+---+---+---+---+\n";
  }

  os << "\nFen: " << pos.fen() << "\n";

  return os;
}


/// Position::init() initializes at startup the various arrays used to compute
/// hash keys.

void Stelling::init() {

  PRNG rng(19680708);

  for (Kleur c = WIT; c <= ZWART; ++c)
      for (StukType pt = KONING; pt <= DAME; ++pt)
          for (Veld s = SQ_A1; s <= SQ_H8; ++s)
              Zobrist::psq[maak_stuk(c, pt)][s] = rng.rand<Sleutel64>();

  for (Lijn f = LIJN_A; f <= LIJN_H; ++f)
      Zobrist::enpassant[f] = rng.rand<Sleutel64>();

  for (int rokade = GEEN_ROKADE; rokade <= ALLE_ROKADE; ++rokade)
  {
      Zobrist::rokade[rokade] = 0;
      Bitboard b = rokade;
      while (b)
      {
          Sleutel64 k = Zobrist::rokade[1ULL << pop_lsb(&b)];
          Zobrist::rokade[rokade] ^= k ? k : rng.rand<Sleutel64>();
      }
  }

  Zobrist::aanZet = rng.rand<Sleutel64>();
  init_hash_move50(50);
}

void Stelling::init_hash_move50(int FiftyMoveDistance) {

	for (int i = 0; i < 32; i++)
		if (4 * i + 50 < 2 * FiftyMoveDistance)
			Zobrist::hash_50move[i] = 0;
		else
			Zobrist::hash_50move[i] = 0x0001000100010001 << i;
}


/// Position::set() initializes the position object with the given FEN string.
/// This function is not very robust - make sure that input FENs are correct,
/// this is assumed to be the responsibility of the GUI.

Stelling& Stelling::set(const string& fenStr, bool isChess960, Thread* th) {
/*
   A FEN string defines a particular position using only the ASCII character set.

   A FEN string contains six fields separated by a space. The fields are:

   1) Piece placement (from white's perspective). Each rank is described, starting
      with rank 8 and ending with rank 1. Within each rank, the contents of each
      square are described from file A through file H. Following the Standard
      Algebraic Notation (SAN), each piece is identified by a single letter taken
      from the standard English names. White pieces are designated using upper-case
      letters ("PNBRQK") whilst Black uses lowercase ("pnbrqk"). Blank squares are
      noted using digits 1 through 8 (the number of blank squares), and "/"
      separates ranks.

   2) Active color. "w" means white moves next, "b" means black.

   3) Castling availability. If neither side can castle, this is "-". Otherwise,
      this has one or more letters: "K" (White can castle kingside), "Q" (White
      can castle queenside), "k" (Black can castle kingside), and/or "q" (Black
      can castle queenside).

   4) En passant target square (in algebraic notation). If there's no en passant
      target square, this is "-". If a pawn has just made a 2-square move, this
      is the position "behind" the pawn. This is recorded regardless of whether
      there is a pawn in position to make an en passant capture.

   5) Halfmove clock. This is the number of halfmoves since the last pawn advance
      or capture. This is used to determine if a draw can be claimed under the
      fifty-move rule.

   6) Fullmove number. The number of the full move. It starts at 1, and is
      incremented after Black's move.
*/
  assert(th != nullptr);

  unsigned char col, row, token;
  size_t idx;
  Veld sq = SQ_A8;
  std::istringstream ss(fenStr);

  std::memset(this, 0, sizeof(Stelling));
  std::fill_n(&stukLijst[0][0], sizeof(stukLijst) / sizeof(Veld), SQ_NONE);
  st = th->ti->stellingInfo + 5;
  std::memset(st, 0, sizeof(StellingInfo));

  ss >> std::noskipws;

  // 1. Piece placement
  while ((ss >> token) && !isspace(token))
  {
      if (isdigit(token))
          sq += Veld(token - '0'); // Advance the given number of files

      else if (token == '/')
          sq -= Veld(16);

      else if ((idx = PieceToChar.find(token)) != string::npos)
      {
          zet_stuk(stuk_kleur(Stuk(idx)), Stuk(idx), sq);
          ++sq;
      }
  }
  stukBB[ALLE_STUKKEN] = kleurBB[WIT] | kleurBB[ZWART];

  // 2. Active color
  ss >> token;
  aanZet = (token == 'w' ? WIT : ZWART);
  ss >> token;

  // 3. Castling availability. Compatible with 3 standards: Normal FEN standard,
  // Shredder-FEN that uses the letters of the columns on which the rooks began
  // the game instead of KQkq and also X-FEN standard that, in case of Chess960,
  // if an inner rook is associated with the castling right, the castling tag is
  // replaced by the file letter of the involved rook, as for the Shredder-FEN.
  while ((ss >> token) && !isspace(token))
  {
      Veld rsq;
      Kleur c = islower(token) ? ZWART : WIT;
      Stuk toren = maak_stuk(c, TOREN);

      token = char(toupper(token));

      if (token == 'K')
          for (rsq = relatief_veld(c, SQ_H1); stuk_op_veld(rsq) != toren; --rsq) {}

      else if (token == 'Q')
          for (rsq = relatief_veld(c, SQ_A1); stuk_op_veld(rsq) != toren; ++rsq) {}

      else if (token >= 'A' && token <= 'H')
          rsq = maak_veld(Lijn(token - 'A'), relatieve_rij(c, RIJ_1));

      else
          continue;

      set_rokade_mogelijkheid(c, rsq);
  }

  // 4. En passant square. Ignore if no pawn capture is possible
  if (   ((ss >> col) && (col >= 'a' && col <= 'h'))
      && ((ss >> row) && (row == '3' || row == '6')))
  {
      st->enpassantVeld = maak_veld(Lijn(col - 'a'), Rij(row - '1'));

      if (!(aanvallers_naar(st->enpassantVeld) & stukken(aanZet, PION)))
          st->enpassantVeld = SQ_NONE;
  }
  else
      st->enpassantVeld = SQ_NONE;

  // 5-6. Halfmove clock and fullmove number
  ss >> std::skipws >> st->remise50Zetten >> partijPly;

  // Convert from fullmove starting from 1 to ply starting from 0,
  // handle also common incorrect FEN with fullmove = 0.
  partijPly = std::max(2 * (partijPly - 1), 0) + (aanZet == ZWART);

  chess960 = isChess960;
  mijnThread = th;
  threadInfo = th->ti;
  numaInfo = th->ni;
  set_stelling_info(st);
  bereken_schaak_penningen();

  assert(stelling_is_ok());

  return *this;
}


void Stelling::kopieer_stelling(const Stelling* pos, Thread* th, StellingInfo* copyState)
{
	std::memcpy(this, pos, sizeof(Stelling));
	if (th)
	{
		mijnThread = th;
		threadInfo = th->ti;
		numaInfo = th->ni;
		st = th->ti->stellingInfo + 5;

		StellingInfo* origSt = pos->threadInfo->stellingInfo + 5;
		// copy key of ancient States
		while (origSt < copyState - 4)
		{
			st->sleutel = origSt->sleutel;
			st++;
			origSt++;
		}
		// copy last 5 StellingInfo records
		while (origSt <= copyState)
		{
			*st = *origSt;
			st++;
			origSt++;
		}
		st--;
	}
	assert(stelling_is_ok());
}


/// Position::set_rokade_mogelijkheid() is a helper function used to set castling
/// rights given the corresponding color and the rook starting square.

void Stelling::set_rokade_mogelijkheid(Kleur c, Veld vanT) {

  Veld vanK = veld<KONING>(c);
  RokadeMogelijkheid rokade = RokadeMogelijkheid(WIT_KORT << ((vanK >= vanT) + 2 * c));

  Veld naarK = relatief_veld(c, vanK < vanT ? SQ_G1 : SQ_C1);
  Veld naarT = relatief_veld(c, vanK < vanT ? SQ_F1 : SQ_D1);

  st->rokadeMogelijkheden |= rokade;
  rokadeMask[vanK] |= rokade;
  rokadeMask[vanT] |= rokade;
  rokadeTorenVeld[naarK] = vanT;

  Bitboard pad = 0;
  Veld richting = naarT > vanT ? RECHTS : LINKS;
  for (Veld s = vanT + richting; s <= naarT; s += richting)
	  pad |= s;

  richting = naarK > vanK ? RECHTS : LINKS;
  for (Veld s = vanK + richting; s <= naarK; s += richting)
	  pad |= s;

  rokadePad[rokade] = pad & ~(bbVeld[vanK] | bbVeld[vanT]);
}


void Stelling::bereken_loper_kleur_sleutel() const {
	Sleutel64 key = 0;
	if (stukken(WIT, LOPER) & DonkereVelden)
		key ^= 0xF3094B57AC4789A2;
	if (stukken(WIT, LOPER) & ~DonkereVelden)
		key ^= 0x89A2F3094B57AC47;
	if (stukken(ZWART, LOPER) & DonkereVelden)
		key ^= 0xAC4789A2F3094B57;
	if (stukken(ZWART, LOPER) & ~DonkereVelden)
		key ^= 0x4B57AC4789A2F309;
	st->loperKleurSleutel = key;
}


/// Position::set_state() computes the hash keys of the position, and other
/// data that once computed is updated incrementally as moves are made.
/// The function is only used when a new position is set up, and to verify
/// the correctness of the StellingInfo data when running in debug mode.

void Stelling::set_stelling_info(StellingInfo* si) const {

  si->sleutel = si->materiaalSleutel = 0;
  si->nietPionMateriaal[WIT] = si->nietPionMateriaal[ZWART] = MATL_0;
  si->psq = SCORE_ZERO;
  si->fase = si->aantalContempt[WIT] = si->aantalContempt[ZWART] = 0;

  si->schaakGevers = aanvallers_naar(veld<KONING>(aanZet)) & stukken(~aanZet);

  for (Bitboard b = stukken(); b; )
  {
      Veld s = pop_lsb(&b);
      Stuk pc = stuk_op_veld(s);
      si->sleutel ^= Zobrist::psq[pc][s];
      si->psq += PSQT::psq[pc][s];
	  si->fase += StukFase[stuk_type(pc)];
	  si->aantalContempt[stuk_kleur(pc)] += StukContempt[stuk_type(pc)];
  }

  if (si->enpassantVeld != SQ_NONE)
      si->sleutel ^= Zobrist::enpassant[lijn(si->enpassantVeld)];

  if (aanZet == ZWART)
      si->sleutel ^= Zobrist::aanZet;

  si->sleutel ^= Zobrist::rokade[si->rokadeMogelijkheden];

  si->pionSleutel = 0x1234567890ABCDEF;
  for (Bitboard b = stukken(PION); b; )
  {
      Veld s = pop_lsb(&b);
      si->pionSleutel ^= Zobrist::psq[stuk_op_veld(s)][s];
  }

  for (Kleur c = WIT; c <= ZWART; ++c)
	  for (StukType pt = KONING; pt <= DAME; ++pt)
		  for (int cnt = 0; cnt < stukAantal[maak_stuk(c, pt)]; ++cnt)
		      si->materiaalSleutel ^= Zobrist::psq[maak_stuk(c, pt)][cnt];

  bereken_loper_kleur_sleutel();

  for (Kleur c = WIT; c <= ZWART; ++c)
      for (StukType pt = PAARD; pt <= DAME; ++pt)
          si->nietPionMateriaal[c] += MateriaalWaarden[pt] * stukAantal[maak_stuk(c, pt)];
}


/// Position::fen() returns a FEN representation of the position. In case of
/// Chess960 the Shredder-FEN notation is used. This is mainly a debugging function.

const string Stelling::fen() const {

  int emptyCnt;
  std::ostringstream ss;

  for (Rij r = RIJ_8; r >= RIJ_1; --r)
  {
      for (Lijn f = LIJN_A; f <= LIJN_H; ++f)
      {
          for (emptyCnt = 0; f <= LIJN_H && leeg_veld(maak_veld(f, r)); ++f)
              ++emptyCnt;

          if (emptyCnt)
              ss << emptyCnt;

          if (f <= LIJN_H)
              ss << PieceToChar[stuk_op_veld(maak_veld(f, r))];
      }

      if (r > RIJ_1)
          ss << '/';
  }

  ss << (aanZet == WIT ? " w " : " b ");

  if (rokade_mogelijk(WIT_KORT))
      ss << (chess960 ? char('A' + lijn(rokade_toren_veld(SQ_G1))) : 'K');

  if (rokade_mogelijk(WIT_LANG))
      ss << (chess960 ? char('A' + lijn(rokade_toren_veld(SQ_C1))) : 'Q');

  if (rokade_mogelijk(ZWART_KORT))
      ss << (chess960 ? char('a' + lijn(rokade_toren_veld(SQ_G8))) : 'k');

  if (rokade_mogelijk(ZWART_LANG))
      ss << (chess960 ? char('a' + lijn(rokade_toren_veld(SQ_C8))) : 'q');

  if (!rokade_mogelijkheden(WIT) && !rokade_mogelijkheden(ZWART))
      ss << '-';

  ss << (enpassant_veld() == SQ_NONE ? " - " : " " + UCI::veld(enpassant_veld()) + " ")
     << st->remise50Zetten << " " << 1 + (partijPly - (aanZet == ZWART)) / 2;

  return ss.str();
}


/// Position::partij_fase() calculates the game phase interpolating total non-pawn
/// material between endgame and midgame limits.

PartijFase Stelling::partij_fase() const {

	return PartijFase(std::max(std::min(st->fase - 6, (int)MIDDENSPEL_FASE), 0));
}


/// Position::slider_blockers() returns a bitboard of all the pieces in 'target' that
/// are blocking attacks on the square 's' from 'sliders'. A piece blocks a slider
/// if removing that piece from the board would result in a position where square 's'
/// is attacked. For example, a king-attack blocking piece can be either a pinned or
/// a discovered check piece, according if its color is the opposite or the same of
/// the color of the slider.

//Bitboard Stelling::slider_blockers(Bitboard target, Bitboard sliders, Veld s) const {
//
//  Bitboard b, pinners, result = 0;
//
//  // Pinners are sliders that attack 's' when a pinned piece is removed
//  pinners = (  (LegeAanval[TOREN  ][s] & stukken(DAME, TOREN))
//             | (LegeAanval[LOPER][s] & stukken(DAME, LOPER))) & sliders;
//
//  while (pinners)
//  {
//      b = bb_tussen(s, pop_lsb(&pinners)) & stukken();
//
//      if (!meer_dan_een(b))
//          result |= b & target;
//  }
//  return result;
//}


/// Position::aanvallers_naar() computes a bitboard of all pieces which attack a
/// given square. Slider attacks use the occupied bitboard to indicate occupancy.

Bitboard Stelling::aanvallers_naar(Veld s, Bitboard occupied) const {

  return  (aanval_van<PION>(s, ZWART)    & stukken(WIT, PION))
        | (aanval_van<PION>(s, WIT)      & stukken(ZWART, PION))
        | (aanval_van<PAARD>(s)          & stukken(PAARD))
        | (aanval_bb<TOREN>(s, occupied) & stukken(TOREN, DAME))
        | (aanval_bb<LOPER>(s, occupied) & stukken(LOPER, DAME))
        | (aanval_van<KONING>(s)         & stukken(KONING));
}


/// Position::legal() tests whether a pseudo-legal move is legal

bool Stelling::legale_zet(Zet zet) const {

  assert(is_ok(zet));

  Kleur ik = aanZet;
  Veld van = van_veld(zet);

  assert(stuk_kleur(bewogen_stuk(zet)) == ik);
  assert(stuk_op_veld(veld<KONING>(ik)) == maak_stuk(ik, KONING));

  // En passant captures are a tricky special case. Because they are rather
  // uncommon, we do it simply by testing whether the king is attacked after
  // the move is made.
  if (zet_type(zet) == ENPASSANT)
  {
      Veld veldK = veld<KONING>(ik);
      Veld naar = naar_veld(zet);
      Veld slagveld = naar - pion_vooruit(ik);
      Bitboard bezet = (stukken() ^ van ^ slagveld) | naar;

      assert(naar == enpassant_veld());
      assert(bewogen_stuk(zet) == maak_stuk(ik, PION));
      assert(stuk_op_veld(slagveld) == maak_stuk(~ik, PION));
      assert(stuk_op_veld(naar) == GEEN_STUK);

      return   !(aanval_bb<TOREN>(veldK, bezet) & stukken(~ik, DAME, TOREN))
            && !(aanval_bb<LOPER>(veldK, bezet) & stukken(~ik, DAME, LOPER));
  }

  // If the moving piece is a king, check whether the destination
  // square is attacked by the opponent. Castling moves are checked
  // for legality during move generation.
  if (stuk_type(stuk_op_veld(van)) == KONING)
      return zet_type(zet) == ROKADE || !(aanvallers_naar(naar_veld(zet)) & stukken(~ik));

  // A non-king move is legal if and only if it is not pinned or it
  // is moving along the ray towards or away from the king.
  return   !(st->xRay[aanZet] & van)
        ||  aligned(van, naar_veld(zet), veld<KONING>(ik));
}


/// Position::pseudo_legal() takes a random move and tests whether the move is
/// pseudo legal. It is used to validate moves from TT that can be corrupted
/// due to SMP concurrent access or hash position key aliasing.

bool Stelling::geldige_zet(const Zet zet) const {

  Kleur ik = aanZet;
  Veld van = van_veld(zet);
  Veld naar = naar_veld(zet);
  Stuk pc = bewogen_stuk(zet);

  // Use a slower but simpler function for uncommon cases
  // rokade 0,8%  enpassant 0,03%  promotie 0,6%
  if (zet_type(zet) != NORMAAL)
      return ZettenLijst<GEN_LEGALE_ZETTEN>(*this).contains(zet);

  // Is not a promotion, so promotion piece must be empty
  //if (promotie_stuk(zet) - PAARD != GEEN_STUKTYPE)
  //    return false;

  // If the 'from' square is not occupied by a piece belonging to the side to
  // move, the move is obviously not legal.
  if (pc == GEEN_STUK || stuk_kleur(pc) != ik)
      return false;

  // The destination square cannot be occupied by a friendly piece
  if (stukken(ik) & naar)
      return false;

  // Handle the special case of a pawn move
  if (stuk_type(pc) == PION)
  {
      // We have already handled promotion moves, so destination
      // cannot be on the 8th/1st rank.
      if (rij(naar) == relatieve_rij(ik, RIJ_8))
          return false;

      if (   !(aanval_van<PION>(van, ik) & stukken(~ik) & naar) // Not a capture
          && !((van + pion_vooruit(ik) == naar) && leeg_veld(naar))       // Not a single push
          && !(   (van + 2 * pion_vooruit(ik) == naar)              // Not a double push
               && (rij(van) == relatieve_rij(ik, RIJ_2))
               && leeg_veld(naar)
               && leeg_veld(naar - pion_vooruit(ik))))
          return false;
  }
  else if (!(aanval_van(pc, van) & naar))
      return false;

  // Evasions generator already takes care to avoid some kind of illegal moves
  // and legal() relies on this. We therefore have to take care that the same
  // kind of moves are filtered out here.
  if (schaak_gevers())
  {
      if (stuk_type(pc) != KONING)
      {
          // Double check? In this case a king move is required
          if (meer_dan_een(schaak_gevers()))
              return false;

          // Our move must be a blocking evasion or a capture of the checking piece
          if (!((bb_tussen(lsb(schaak_gevers()), veld<KONING>(ik)) | schaak_gevers()) & naar))
              return false;
      }
      // In case of king moves under check we have to remove king so as to catch
      // invalid moves like b1a1 when opposite queen is on c1.
      else if (aanvallers_naar(naar, stukken() ^ van) & stukken(~ik))
          return false;
  }

  return true;
}


/// Position::geeft_schaak() tests whether a pseudo-legal move gives a check

bool Stelling::geeft_schaak(Zet zet) const {

  assert(is_ok(zet));
  assert(stuk_kleur(bewogen_stuk(zet)) == aanZet);

  Veld van = van_veld(zet);
  Veld naar = naar_veld(zet);
  Veld veldK = veld<KONING>(~aanZet);

  // Is there a direct check?
  if (st->schaakVelden[stuk_type(stuk_op_veld(van))] & naar)
      return true;

  // Is there a discovered check?
  if (   (st->xRay[~aanZet] & van)
      && !aligned(van, naar, veldK))
      return true;

  switch (zet_type(zet))
  {
  case NORMAAL:
      return false;

  case PROMOTIE:
      return aanval_bb(Stuk(promotie_stuk(zet)), naar, stukken() ^ van) & veldK;

  // En passant capture with check? We have already handled the case
  // of direct checks and ordinary discovered check, so the only case we
  // need to handle is the unusual case of a discovered check through
  // the captured pawn.
  case ENPASSANT:
  {
      Veld capsq = maak_veld(lijn(naar), rij(van));
      Bitboard b = (stukken() ^ van ^ capsq) | naar;

      return  (aanval_bb<TOREN>(veldK, b) & stukken(aanZet, DAME, TOREN))
            | (aanval_bb<LOPER>(veldK, b) & stukken(aanZet, DAME, LOPER));
  }
  case ROKADE:
  {
      Veld vanT = rokade_toren_veld(naar);
      Veld naarT = relatief_veld(aanZet, vanT > van ? SQ_F1 : SQ_D1);

      return   (LegeAanval[TOREN][naarT] & veldK)
            && (aanval_bb<TOREN>(naarT, (stukken() ^ van ^ vanT) | naarT | naar) & veldK);
  }
  default:
      assert(false);
      return false;
  }
}


/// Position::speel_zet() makes a move, and saves all information necessary
/// to a StellingInfo object. The move is assumed to be legal. Pseudo-legal
/// moves should be filtered out before this function is called.

void Stelling::speel_zet(Zet zet)
{
	bool geeftSchaak = zet_type(zet) == NORMAAL && !aftrek_schaak_mogelijk()
		? st->schaakVelden[stuk_type(stuk_op_veld(van_veld(zet)))] & naar_veld(zet)
		: geeft_schaak(zet);

	speel_zet(zet, geeftSchaak);
}

void Stelling::speel_zet(Zet zet, bool geeftSchaak) {

  assert(is_ok(zet));

  ++knopen;
#ifdef TRACE_LOG
  trace_do_move(zet);
#endif
  Sleutel64 sleutel64 = st->sleutel ^ Zobrist::aanZet;

  // Copy some fields of the old state to our new StellingInfo object except the
  // ones which are going to be recalculated from scratch anyway and then switch
  // our state pointer to point to the new (ready to be updated) state.
  std::memcpy(st + 1, st, offsetof(StellingInfo, sleutel));
  st++;

  // Increment ply counters. In particular, rule50 will be reset to zero later on
  // in case of a capture or a pawn move.
  st->remise50Zetten = (st - 1)->remise50Zetten + 1;
  st->afstandTotNullzet = (st - 1)->afstandTotNullzet + 1;

  Kleur ik = aanZet;
  Kleur jij = ~ik;
  Veld van = van_veld(zet);
  Veld naar = naar_veld(zet);
  Stuk pc = stuk_op_veld(van);
  Stuk slagstuk;

  assert(stuk_kleur(pc) == ik);

  if (zet_type(zet) == ROKADE)
  {
      assert(stuk_type(pc) == KONING);

      Veld vanT, naarT;
      speel_rokade<true>(ik, van, naar, vanT, naarT);

      slagstuk = GEEN_STUK;
	  Stuk mijnToren = maak_stuk(ik, TOREN);
      st->psq += PSQT::psq[mijnToren][naarT] - PSQT::psq[mijnToren][vanT];
      sleutel64 ^= Zobrist::psq[mijnToren][vanT] ^ Zobrist::psq[mijnToren][naarT];
  }
  else
	  slagstuk = zet_type(zet) == ENPASSANT ? maak_stuk(jij, PION) : stuk_op_veld(naar);

  if (slagstuk)
  {
	  assert(stuk_type(slagstuk) != KONING);
	  assert(stuk_kleur(slagstuk) == jij);
	  Veld slagveld = naar;

      // If the captured piece is a pawn, update pawn hash key, otherwise
      // update non-pawn material.
      if (stuk_type(slagstuk) == PION)
      {
          if (zet_type(zet) == ENPASSANT)
          {
              slagveld -= pion_vooruit(ik);

              assert(stuk_type(pc) == PION);
              assert(naar == st->enpassantVeld);
              assert(relatieve_rij(ik, naar) == RIJ_6);
              assert(stuk_op_veld(naar) == GEEN_STUK);
              assert(stuk_op_veld(slagveld) == maak_stuk(jij, PION));

              bord[slagveld] = GEEN_STUK; // Not done by verwijder_stuk()
          }

          st->pionSleutel ^= Zobrist::psq[slagstuk][slagveld];
      }
      else
          st->nietPionMateriaal[jij] -= MateriaalWaarden[slagstuk];

	  st->fase -= StukFase[slagstuk];
	  st->aantalContempt[jij] -= StukContempt[slagstuk];

      // Update board and piece lists
      verwijder_stuk(jij, slagstuk, slagveld);

      // Update material hash key and prefetch access to materialTable
      sleutel64 ^= Zobrist::psq[slagstuk][slagveld];
	  st->materiaalSleutel ^= Zobrist::psq[slagstuk][stukAantal[slagstuk]];
      prefetch(threadInfo->materiaalTabel[st->materiaalSleutel ^ st->loperKleurSleutel]);

	  if (stuk_type(slagstuk) == LOPER)
		  bereken_loper_kleur_sleutel();

      // Update incremental scores
      st->psq -= PSQT::psq[slagstuk][slagveld];

      // Reset rule 50 counter
      st->remise50Zetten = 0;
  }

  // Update hash key
  sleutel64 ^= Zobrist::psq[pc][van] ^ Zobrist::psq[pc][naar];

  // Reset en passant square
  if (st->enpassantVeld != SQ_NONE)
  {
      sleutel64 ^= Zobrist::enpassant[lijn(st->enpassantVeld)];
      st->enpassantVeld = SQ_NONE;
  }

  // Update castling rights if needed
  if (st->rokadeMogelijkheden && (rokadeMask[van] | rokadeMask[naar]))
  {
      int rokade = rokadeMask[van] | rokadeMask[naar];
      sleutel64 ^= Zobrist::rokade[st->rokadeMogelijkheden & rokade];
      st->rokadeMogelijkheden &= ~rokade;
  }

  // Move the piece. The tricky Chess960 castling is handled earlier
  if (zet_type(zet) != ROKADE)
      verplaats_stuk(ik, pc, van, naar);

  // If the moving piece is a pawn do some special extra work
  if (stuk_type(pc) == PION)
  {
      // Set en-passant square if the moved pawn can be captured
      if (   (int(naar) ^ int(van)) == 16
          && (aanval_van<PION>(naar - pion_vooruit(ik), ik) & stukken(jij, PION)))
      {
          st->enpassantVeld = (van + naar) / 2;
          sleutel64 ^= Zobrist::enpassant[lijn(st->enpassantVeld)];
      }

      else if (zet_type(zet) == PROMOTIE)
      {
          Stuk promotion = maak_stuk(ik, promotie_stuk(zet));

          assert(relatieve_rij(ik, naar) == RIJ_8);
          assert(stuk_type(promotion) >= PAARD && stuk_type(promotion) <= DAME);

          verwijder_stuk(ik, pc, naar);
          zet_stuk(ik, promotion, naar);

          // Update hash keys
          sleutel64 ^= Zobrist::psq[pc][naar] ^ Zobrist::psq[promotion][naar];
          st->pionSleutel ^= Zobrist::psq[pc][naar];
          st->materiaalSleutel ^= Zobrist::psq[promotion][stukAantal[promotion] - 1]
								^ Zobrist::psq[pc][stukAantal[pc]];

		  if (stuk_type(promotion) == LOPER)
			  bereken_loper_kleur_sleutel();

          // Update incremental score
          st->psq += PSQT::psq[promotion][naar] - PSQT::psq[pc][naar];

          // Update material
          st->nietPionMateriaal[ik] += MateriaalWaarden[promotion];
		  st->fase += StukFase[promotion];
		  st->aantalContempt[ik] -= StukContempt[promotion];
	  }

      // Update pawn hash key and prefetch access to pawnsTable
      st->pionSleutel ^= Zobrist::psq[pc][van] ^ Zobrist::psq[pc][naar];
      prefetch2(threadInfo->pionnenTabel[st->pionSleutel]);

      // Reset rule 50 draw counter
      st->remise50Zetten = 0;
  }

  stukBB[ALLE_STUKKEN] = kleurBB[WIT] | kleurBB[ZWART];

  // Update incremental scores
  st->psq += PSQT::psq[pc][naar] - PSQT::psq[pc][van];

  // Set capture piece
  st->geslagenStuk = slagstuk;
  st->vorigeZet = zet;
  st->counterZetWaarden = &numaInfo->counterZetStatistieken[pc][naar_veld(zet)];
  st->evalPositioneel = GEEN_EVAL;

  // Update the key with the final value
  st->sleutel = sleutel64;

  // Calculate schaak_gevers bitboard (if move gives check)
  st->schaakGevers = geeftSchaak ? aanvallers_naar(veld<KONING>(jij)) & stukken(ik) : 0;
  st->zetHerhaling = is_remise();

  aanZet = ~aanZet;
  bereken_schaak_penningen();

  assert(stelling_is_ok());
}


/// Position::neem_zet_terug() unmakes a move. When it returns, the position should
/// be restored to exactly the same state as before the move was made.

void Stelling::neem_zet_terug(Zet zet) {

  assert(is_ok(zet));
#ifdef TRACE_LOG
  trace_cancel_move();
#endif

  aanZet = ~aanZet;

  Kleur ik = aanZet;
  Veld van = van_veld(zet);
  Veld naar = naar_veld(zet);
  Stuk pc = stuk_op_veld(naar);

  assert(leeg_veld(van) || zet_type(zet) == ROKADE);
  assert(stuk_type(st->geslagenStuk) != KONING);

  if (zet_type(zet) == PROMOTIE)
  {
      assert(relatieve_rij(ik, naar) == RIJ_8);
      assert(stuk_type(pc) == promotie_stuk(zet));
      assert(stuk_type(pc) >= PAARD && stuk_type(pc) <= DAME);

      verwijder_stuk(ik, pc, naar);
	  pc = maak_stuk(ik, PION);
      zet_stuk(ik, pc, naar);
  }

  if (zet_type(zet) == ROKADE)
  {
      Veld vanT, naarT;
      speel_rokade<false>(ik, van, naar, vanT, naarT);
  }
  else
  {
      verplaats_stuk(ik, pc, naar, van); // Put the piece back at the source square

      if (st->geslagenStuk)
      {
          Veld slagveld = naar;

          if (zet_type(zet) == ENPASSANT)
          {
              slagveld -= pion_vooruit(ik);

              assert(stuk_type(pc) == PION);
              assert(naar == (st - 1)->enpassantVeld);
              assert(relatieve_rij(ik, naar) == RIJ_6);
              assert(stuk_op_veld(slagveld) == GEEN_STUK);
              assert(st->geslagenStuk == maak_stuk(~ik, PION));
          }

          zet_stuk(~ik, st->geslagenStuk, slagveld); // Restore the captured piece
      }
  }
  stukBB[ALLE_STUKKEN] = kleurBB[WIT] | kleurBB[ZWART];

  // Finally point our state pointer back to the previous state
  st--;
  //--partijPly;  // only rootStelling->partijPly is ever used, no need to increment it at every move 

  assert(stelling_is_ok());
}


/// Position::speel_rokade() is a helper used to do/undo a castling move. This
/// is a bit tricky, especially in Chess960.
template<bool Do>
void Stelling::speel_rokade(Kleur ik, Veld van, Veld naar, Veld& vanT, Veld& naarT) {

  vanT = rokade_toren_veld(naar);
  naarT = relatief_veld(ik, vanT > van ? SQ_F1 : SQ_D1);

  if (!chess960)
  {
	  verplaats_stuk(ik, maak_stuk(ik, KONING), Do ? van : naar, Do ? naar : van);
	  verplaats_stuk(ik, maak_stuk(ik, TOREN), Do ? vanT : naarT, Do ? naarT : vanT);
  }
  else
  {
	  verwijder_stuk(ik, maak_stuk(ik, KONING), Do ? van : naar);
	  verwijder_stuk(ik, maak_stuk(ik, TOREN), Do ? vanT : naarT);
	  bord[Do ? van : naar] = bord[Do ? vanT : naarT] = GEEN_STUK;
	  zet_stuk(ik, maak_stuk(ik, KONING), Do ? naar : van);
	  zet_stuk(ik, maak_stuk(ik, TOREN), Do ? naarT : vanT);
  }
}


/// Position::do(undo)_null_move() is used to do(undo) a "null move": It flips
/// the side to move without executing any move on the board.

void Stelling::speel_null_zet() {

  assert(!schaak_gevers());

  ++knopen;
#ifdef TRACE_LOG
  trace_do_move(0);
#endif

  Sleutel64 sleutel64 = st->sleutel ^ Zobrist::aanZet;
  if (st->enpassantVeld != SQ_NONE)
	  sleutel64 ^= Zobrist::enpassant[lijn(st->enpassantVeld)];

  TT.prefetch_tuple(sleutel64);

  std::memcpy(st + 1, st, offsetof(StellingInfo, sleutel));
  st++;

  st->sleutel = sleutel64;
  st->remise50Zetten = (st - 1)->remise50Zetten + 1;
  st->afstandTotNullzet = 0;

  st->enpassantVeld = SQ_NONE;
  st->schaakGevers = 0;
  //st->capturedPiece = GEEN_STUK; // niet echt nodig, overal waar geslagen_stuk() gebruikt wordt 
                                  // is er een bescherming tegen voorafgaande null move
  st->vorigeZet = NULL_ZET;
  st->counterZetWaarden = nullptr;
  st->evalPositioneel = (st - 1)->evalPositioneel;
  st->evalFactor = (st - 1)->evalFactor;

  st->zetHerhaling = is_remise();

  aanZet = ~aanZet;
  bereken_schaak_penningen();

  assert(stelling_is_ok());
}

void Stelling::neem_null_terug() {

  assert(!schaak_gevers());
#ifdef TRACE_LOG
  trace_cancel_move();
#endif

  st--;
  aanZet = ~aanZet;
}


/// Position::key_after() computes the new hash key after the given move. Needed
/// for speculative prefetch. It doesn't recognize special moves like castling,
/// en-passant and promotions.

Sleutel64 Stelling::sleutel_na_zet(Zet zet) const {

  Veld van = van_veld(zet);
  Veld naar = naar_veld(zet);
  Stuk pc = stuk_op_veld(van);
  Stuk slagstuk = stuk_op_veld(naar);
  Sleutel64 k = st->sleutel ^ Zobrist::aanZet;

  if (slagstuk)
      k ^= Zobrist::psq[slagstuk][naar];

  return k ^ Zobrist::psq[pc][naar] ^ Zobrist::psq[pc][van];
}


/// Position::see() is a static exchange evaluator: It tries to estimate the
/// material gain or loss resulting from a move.

const SeeWaarde SeeValueSimple[STUK_N] =
{
	SEE_0, SEE_0, SEE_PION, SEE_PAARD, SEE_LOPER, SEE_TOREN, SEE_DAME, SEE_0,
	SEE_0, SEE_0, SEE_PION, SEE_PAARD, SEE_LOPER, SEE_TOREN, SEE_DAME, SEE_0
};

const SeeWaarde* Stelling::see_waarden() const {
	return SeeValueSimple;
}

// see_sign geeft >= 0 (geen exacte waarde) of < 0 (wel een exacte waarde!)
SeeWaarde Stelling::see(Zet zet) const {
	
  SeeWaarde waarde, best, slechtst;
  const SeeWaarde* see_waarde = see_waarden();

  assert(is_ok(zet));

  Veld van = van_veld(zet);
  Veld naar = naar_veld(zet);
  waarde = see_waarde[stuk_op_veld(naar)];
  if (see_waarde[stuk_op_veld(van)] <= waarde)
	  return SEE_0;

  // Castling moves are implemented as king capturing the rook so cannot
  // be handled correctly. Simply return WAARDE_0 that is always correct
  // unless in the rare case the rook ends up under attack.
  if (zet_type(zet) == ROKADE || zet_type(zet) == ENPASSANT)
      return SEE_0;

  best = waarde;
  waarde -= see_waarde[stuk_op_veld(van)];
  slechtst = waarde;

  // Find all attackers to the destination square, with the moving piece
  // removed, but possibly an X-ray attacker added behind it.
  const Kleur ik = stuk_kleur(stuk_op_veld(van));
  Bitboard bezet = stukken() ^ van;
  Bitboard aanvallers = aanvallers_naar(naar, bezet) & bezet;

  do {
	  Bitboard mijnAanvallers = aanvallers & stukken(~ik);
	  if (!mijnAanvallers)
		  return best;
	  //if (usePins) 
	  //{
		 // Bitboard pinned = stmAttackers & st->xRay[~stm];
		 // while (pinned) {
			//  Square veld = pop_lsb(&pinned);
			//  if (occupied & st->pinner[veld]) {
			//	  stmAttackers ^= veld;
			//	  if (!stmAttackers)
			//		  return best;
			//  }
		 // }
	  //}

	  // Locate and remove the next least valuable attacker
	  Bitboard bb;
	  StukType jouwstuk;
	  if (bb = mijnAanvallers & stukken(PION))
		  jouwstuk = PION;
	  else if (bb = mijnAanvallers & stukken(PAARD))
		  jouwstuk = PAARD;
	  else if (bb = mijnAanvallers & stukken(LOPER))
		  jouwstuk = LOPER;
	  else if (bb = mijnAanvallers & stukken(TOREN))
		  jouwstuk = TOREN;
	  else if (bb = mijnAanvallers & stukken(DAME))
		  jouwstuk = DAME;
	  else
		  return aanvallers & stukken(ik) ? best : slechtst;

	  // Add the new entry to the swap list
	  waarde += see_waarde[jouwstuk];
	  best = std::min(best, waarde);
	  if (slechtst >= best)
		  return slechtst;

	  bezet ^= (bb & -bb);
	  if (!(jouwstuk & 1)) // PION, LOPER, DAME
		  aanvallers |= aanval_bb<LOPER>(naar, bezet) & (stukken(LOPER) | stukken(DAME));
	  if (jouwstuk >= TOREN)
		  aanvallers |= aanval_bb<TOREN>(naar, bezet) & (stukken(TOREN) | stukken(DAME));
	  aanvallers &= bezet;

	  mijnAanvallers = aanvallers & stukken(ik);
	  if (!mijnAanvallers)
		  return slechtst;
	  //if (usePins)
	  //{
		 // Bitboard pinned = stmAttackers & st->xRay[stm];
		 // while (pinned) {
			//  Square veld = pop_lsb(&pinned);
			//  if (occupied & st->pinner[veld]) {
			//	  stmAttackers ^= veld;
			//	  if (!stmAttackers)
			//		  return worst;
			//  }
		 // }
	  //}

	  // Locate and remove the next least valuable attacker
	  if (bb = mijnAanvallers & stukken(PION))
		  jouwstuk = PION;
	  else if (bb = mijnAanvallers & stukken(PAARD))
		  jouwstuk = PAARD;
	  else if (bb = mijnAanvallers & stukken(LOPER))
		  jouwstuk = LOPER;
	  else if (bb = mijnAanvallers & stukken(TOREN))
		  jouwstuk = TOREN;
	  else if (bb = mijnAanvallers & stukken(DAME))
		  jouwstuk = DAME;
	  else
		  return aanvallers & stukken(~ik) ? slechtst : best;

	  // Add the new entry to the swap list
	  waarde -= see_waarde[jouwstuk];
	  slechtst = std::max(slechtst, waarde);
	  if (slechtst >= best)
		  return best;
	  if (slechtst >= 0)
		  return SEE_0;

	  bezet ^= (bb & -bb);
	  if (!(jouwstuk & 1)) // PION, LOPER, DAME
		  aanvallers |= aanval_bb<LOPER>(naar, bezet) & (stukken(LOPER) | stukken(DAME));
	  if (jouwstuk >= TOREN)
		  aanvallers |= aanval_bb<TOREN>(naar, bezet) & (stukken(TOREN) | stukken(DAME));
	  aanvallers &= bezet;
  } while (true);
}

// Test whether see(zet) >= value
//template <bool UsePins>
bool Stelling::see_test(Zet zet, SeeWaarde limiet) const {

	if (zet_type(zet) == ROKADE)
		return 0 >= limiet;

	const SeeWaarde* see_waarde = see_waarden();
	Veld van = van_veld(zet);
	Veld naar = naar_veld(zet);
	Bitboard bezet = stukken();
	const Kleur ik = stuk_kleur(stuk_op_veld(van));

	int waarde = see_waarde[stuk_op_veld(naar)] - limiet;
	if (zet_type(zet) == ENPASSANT) {
		bezet ^= naar - pion_vooruit(ik); // Remove the captured pawn
		waarde += see_waarde[PION];
	}
	if (waarde < 0)
		return false;

	waarde -= see_waarde[stuk_op_veld(van)];
	if (waarde >= 0)
		return true;

	bezet ^= van;
	Bitboard aanvallers = aanvallers_naar(naar, bezet) & bezet;

	do {
		Bitboard mijnAanvallers = aanvallers & stukken(~ik);
		if (!mijnAanvallers)
			return true;
		//if (UsePins)
		{
			Bitboard gepend = mijnAanvallers & st->xRay[~ik];
			while (gepend) {
				Veld veld = pop_lsb(&gepend);
				if (bezet & st->penning_door[veld]) {
					mijnAanvallers ^= veld;
					if (!mijnAanvallers)
						return true;
				}
			}
		}

		Bitboard bb;
		StukType slagstuk;
		if (bb = mijnAanvallers & stukken(PION))
			slagstuk = PION;
		else if (bb = mijnAanvallers & stukken(PAARD))
			slagstuk = PAARD;
		else if (bb = mijnAanvallers & stukken(LOPER))
			slagstuk = LOPER;
		else if (bb = mijnAanvallers & stukken(TOREN))
			slagstuk = TOREN;
		else if (bb = mijnAanvallers & stukken(DAME))
			slagstuk = DAME;
		else
			return aanvallers & stukken(ik);

		waarde += see_waarde[slagstuk];
		if (waarde < 0) 
			return false;
		bezet ^= (bb & -bb);
		if (!(slagstuk & 1)) // PION, LOPER, DAME
			aanvallers |= aanval_bb<LOPER>(naar, bezet) & (stukken(LOPER) | stukken(DAME));
		if (slagstuk >= TOREN)
			aanvallers |= aanval_bb<TOREN>(naar, bezet) & (stukken(TOREN) | stukken(DAME));
		aanvallers &= bezet;

		mijnAanvallers = aanvallers & stukken(ik);
		if (!mijnAanvallers)
			return false;
		//if (UsePins)
		{
			Bitboard gepend = mijnAanvallers & st->xRay[ik];
			while (gepend) {
				Veld veld = pop_lsb(&gepend);
				if (bezet & st->penning_door[veld]) {
					mijnAanvallers ^= veld;
					if (!mijnAanvallers)
						return false;
				}
			}
		}
		
		if (bb = mijnAanvallers & stukken(PION))
			slagstuk = PION;
		else if (bb = mijnAanvallers & stukken(PAARD))
			slagstuk = PAARD;
		else if (bb = mijnAanvallers & stukken(LOPER))
			slagstuk = LOPER;
		else if (bb = mijnAanvallers & stukken(TOREN))
			slagstuk = TOREN;
		else if (bb = mijnAanvallers & stukken(DAME))
			slagstuk = DAME;
		else
			return !(aanvallers & stukken(~ik));

		waarde -= see_waarde[slagstuk];
		if (waarde >= 0)
			return true;
		bezet ^= (bb & -bb);
		if (!(slagstuk & 1)) // PION, LOPER, DAME
			aanvallers |= aanval_bb<LOPER>(naar, bezet) & (stukken(LOPER) | stukken(DAME));
		if (slagstuk >= TOREN)
			aanvallers |= aanval_bb<TOREN>(naar, bezet) & (stukken(TOREN) | stukken(DAME));
		aanvallers &= bezet;
	} while (true);
}


/// Position::is_draw() tests whether the position is drawn by 50-move rule
/// or by repetition. It does not detect stalemates.

bool Stelling::is_remise() const {

  if (st->remise50Zetten >= 2 * Threads.vijftigZettenAfstand)
  {
	  if (st->remise50Zetten == 100)
		  return !st->schaakGevers || ZettenLijst<GEN_LEGALE_ZETTEN>(*this).size();
	  return true;
  }

  int n = std::min(st->remise50Zetten, st->afstandTotNullzet) - 4;
  if (n < 0)
	  return false;

  StellingInfo* stp = st - 4;
  do
  {
      if (stp->sleutel == st->sleutel)
          return true; // Draw at first repetition

	  stp -= 2;
	  n -= 2;
  } while (n >= 0);

  return false;
}


/// Position::flip() flips position with the white and black sides reversed. This
/// is only useful for debugging e.g. for finding evaluation symmetry bugs.

void Stelling::draaiBord() {

  string f, token;
  std::stringstream ss(fen());

  for (Rij r = RIJ_8; r >= RIJ_1; --r) // Piece placement
  {
      std::getline(ss, token, r > RIJ_1 ? '/' : ' ');
      f.insert(0, token + (f.empty() ? " " : "/"));
  }

  ss >> token; // Active color
  f += (token == "w" ? "B " : "W "); // Will be lowercased later

  ss >> token; // Castling availability
  f += token + " ";

  std::transform(f.begin(), f.end(), f.begin(),
                 [](char c) { return char(islower(c) ? toupper(c) : tolower(c)); });

  ss >> token; // En passant square
  f += (token == "-" ? token : token.replace(1, 1, token[1] == '3' ? "6" : "3"));

  std::getline(ss, token); // Half and full moves
  f += token;

  set(f, is_chess960(), mijn_thread());

  assert(stelling_is_ok());
}


/// Position::stelling_is_ok() performs some consistency checks for the position object.
/// This is meant to be helpful when debugging.

bool Stelling::stelling_is_ok(int* failedStep) const {

  const bool Fast = true; // Quick (default) or full check?

  enum { Default, King, Bitboards, State, Lists, Castling };

  for (int step = Default; step <= (Fast ? Default : Castling); step++)
  {
      if (failedStep)
          *failedStep = step;

      if (step == Default)
          if (   (aanZet != WIT && aanZet != ZWART)
              || stuk_op_veld(veld<KONING>(WIT)) != W_KONING
              || stuk_op_veld(veld<KONING>(ZWART)) != Z_KONING
              || (   enpassant_veld() != SQ_NONE
                  && relatieve_rij(aanZet, enpassant_veld()) != RIJ_6))
              return false;

      if (step == King)
          if (   std::count(bord, bord + VELD_N, W_KONING) != 1
              || std::count(bord, bord + VELD_N, Z_KONING) != 1
              || aanvallers_naar(veld<KONING>(~aanZet)) & stukken(aanZet))
              return false;

      if (step == Bitboards)
      {
          if (  (stukken(WIT) & stukken(ZWART))
              ||(stukken(WIT) | stukken(ZWART)) != stukken())
              return false;

          for (StukType p1 = KONING; p1 <= DAME; ++p1)
              for (StukType p2 = KONING; p2 <= DAME; ++p2)
                  if (p1 != p2 && (stukken(p1) & stukken(p2)))
                      return false;
      }

      if (step == State)
      {
          StellingInfo si = *st;
          set_stelling_info(&si);
          if (std::memcmp(&si, st, sizeof(StellingInfo)))
              return false;
      }

      if (step == Lists)
          for (Kleur c = WIT; c <= ZWART; ++c)
              for (StukType pt = KONING; pt <= DAME; ++pt)
              {
                  if (stukAantal[maak_stuk(c, pt)] != popcount(stukken(c, pt)))
                      return false;

                  for (int i = 0; i < stukAantal[maak_stuk(c, pt)];  ++i)
                      if (   bord[stukLijst[maak_stuk(c, pt)][i]] != maak_stuk(c, pt)
                          || index[stukLijst[maak_stuk(c, pt)][i]] != i)
                          return false;
              }

	  if (step == Castling)
		  for (Kleur c = WIT; c <= ZWART; ++c)
			  for (int n = 0; n < 2; n++)
              {
				  RokadeMogelijkheid rokade = RokadeMogelijkheid(WIT_KORT << (n + 2 * c));

                  if (!rokade_mogelijk(rokade))
                      continue;

                  if (   stuk_op_veld(rokadeTorenVeld[rokade]) != maak_stuk(c, TOREN)
                      || rokadeMask[rokadeTorenVeld[rokade]] != rokade
                      ||(rokadeMask[veld<KONING>(c)] & (rokade)) != rokade)
                      return false;
              }
  }

  return true;
}

std::string Stelling::valideer_stelling() const {

	if (aantal<KONING>(WIT) != 1)
		return "Exactly one white king required";
	if (aantal<KONING>(ZWART) != 1)
		return "Exactly one black king required";

	if (popcount(stukken(WIT)) > 16)
		return "Too many white pieces";
	if (popcount(stukken(ZWART)) > 16)
		return "Too many black pieces";

	if (aantal<PION>(WIT) > 8)
		return "Too many white pawns";
	if (aantal<PION>(ZWART) > 8)
		return "Too many black pawns";

	if (aantal<DAME>(WIT) + aantal<PION>(WIT) > 9)
		return "Too many white queens";
	if (aantal<DAME>(ZWART) + aantal<PION>(ZWART) > 9)
		return "Too many black queens";

	if (aantal<TOREN>(WIT) + aantal<PION>(WIT) > 10)
		return "Too many white rooks";
	if (aantal<TOREN>(ZWART) + aantal<PION>(ZWART) > 10)
		return "Too many black rooks";

	if (aantal<LOPER>(WIT) + aantal<PION>(WIT) > 10)
		return "Too many white bishops";
	if (aantal<LOPER>(ZWART) + aantal<PION>(ZWART) > 10)
		return "Too many black bishops";

	if (aantal<PAARD>(WIT) + aantal<PION>(WIT) > 10)
		return "Too many white knights";
	if (aantal<PAARD>(ZWART) + aantal<PION>(ZWART) > 10)
		return "Too many black knights";

	if (stukken(PION) & (Rank1BB | Rank8BB))
		return "Pawn at rank 1 or 8";

	return "";
}
