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

#include "bitboard.h"
#include "endgame.h"
#include "movegen.h"
#include "thread.h"

using std::string;

namespace Zobrist {
	extern Sleutel64 psq[STUK_N][VELD_N];
}

namespace {

#define V(x) maak_waarde(x)

  // Table used to drive the king towards the edge of the board
  // in KX vs K and KQ vs KR endgames.
  const Waarde DrijfNaarKant[VELD_N] = {
	  V(100), V(90), V(80), V(70), V(70), V(80), V(90), V(100),
	  V(90), V(70), V(60), V(50), V(50), V(60), V(70),  V(90),
	  V(80), V(60), V(40), V(30), V(30), V(40), V(60),  V(80),
	  V(70), V(50), V(30), V(20), V(20), V(30), V(50),  V(70),
	  V(70), V(50), V(30), V(20), V(20), V(30), V(50),  V(70),
	  V(80), V(60), V(40), V(30), V(30), V(40), V(60),  V(80),
	  V(90), V(70), V(60), V(50), V(50), V(60), V(70),  V(90),
	  V(100), V(90), V(80), V(70), V(70), V(80), V(90), V(100)
  };

  // Table used to drive the king towards a corner square of the
  // right color in KBN vs K endgames.
  const Waarde DrijfNaarHoek[VELD_N] = {
	  V(100),  V(90),  V(80),  V(70),  V(60),  V(50),  V(40),  V(30),
	  V(90),  V(80),  V(70),  V(60),  V(50),  V(40),  V(30),  V(40),
	  V(80),  V(70),  V(55),  V(40),  V(40),  V(25),  V(40),  V(50),
	  V(70),  V(60),  V(40),  V(20),  V(10),  V(40),  V(50),  V(60),
	  V(60),  V(50),  V(40),  V(10),  V(20),  V(40),  V(60),  V(70),
	  V(50),  V(40),  V(25),  V(40),  V(40),  V(55),  V(70),  V(80),
	  V(40),  V(30),  V(40),  V(50),  V(60),  V(70),  V(80),  V(90),
	  V(30),  V(40),  V(50),  V(60),  V(70),  V(80),  V(90), V(100)
  };

  // Tables used to drive a piece towards or away from another piece
  const Waarde DrijfDichter[8] = { V(0), V(0), V(100), V(80), V(60), V(40), V(20), V(10) };
  const Waarde DrijfVerder [8] = { V(0), V(5), V(20), V(40), V(60), V(80), V(90), V(100) };

#undef V

  // Pawn Rank based scaling factors used in KRPPKRP endgame
  const int KRPPKRPScaleFactors[RIJ_N] = { 0, 14, 16, 22, 33, 69, 0, 0 };

#ifndef NDEBUG
  bool verify_material(const Stelling& pos, Kleur c, MateriaalWaarde npm, int pawnsCnt) {
    return pos.niet_pion_materiaal(c) == npm && pos.aantal<PION>(c) == pawnsCnt;
  }
#endif

  // Map the square as if sterkeZijde is white and sterkeZijde's only pawn
  // is on the left half of the board.
  Veld normalize(const Stelling& pos, Kleur sterkeZijde, Veld sq) {

    assert(pos.aantal<PION>(sterkeZijde) == 1);

    if (lijn(pos.veld<PION>(sterkeZijde)) >= LIJN_E)
        sq = Veld(sq ^ 7); // Mirror SQ_H1 -> SQ_A1

    if (sterkeZijde == ZWART)
        sq = ~sq;

    return sq;
  }

  // Get the material key of Position
  Sleutel64 key(Kleur c, char* pieces) {

	Sleutel64 materiaalSleutel = 0;
	for (StukType pt = KONING; pt <= DAME; ++pt)
	{
		int pc = int(pieces[pt] - '0');
		for (int cnt = 0; cnt < pc; ++cnt)
			materiaalSleutel ^= Zobrist::psq[maak_stuk(c, pt)][cnt];
		pc = int(pieces[pt + 8] - '0');
		for (int cnt = 0; cnt < pc; ++cnt)
			materiaalSleutel ^= Zobrist::psq[maak_stuk(~c, pt)][cnt];
	}

	return materiaalSleutel;
  }

  inline Bitboard bb(Veld s) { return (1ULL << s); }
  inline Bitboard bb2(Veld s1, Veld s2) { return (1ULL << s1) | (1ULL << s2); }
  inline Bitboard bb3(Veld s1, Veld s2, Veld s3) { return (1ULL << s1) | (1ULL << s2) | (1ULL << s3); }
  inline Bitboard bb4(Veld s1, Veld s2, Veld s3, Veld s4) { return (1ULL << s1) | (1ULL << s2) | (1ULL << s3) | (1ULL << s4); }
  Bitboard aanval_koning_inc(Veld s) { return KorteAanval[KONING][s] | s; }

} // namespace


/// Endgames members definitions

Eindspelen::Eindspelen() {};

const Bitboard BlackModifier = 0xa4489c56;

void Eindspelen::add_waarde(char* stukken, eindspel_waarde_fx fW, eindspel_waarde_fx fB) {
  mapWaarde[key(WIT, stukken)] = waarde_aantal;
  waarde_functies[waarde_aantal++] = fW;

  mapWaarde[key(ZWART, stukken)] = waarde_aantal;
  waarde_functies[waarde_aantal++] = fB;
}

void Eindspelen::add_schaalfactor(char* stukken, eindspel_schaalfactor_fx fW, eindspel_schaalfactor_fx fB) {
  mapSchaalFactor[key(WIT, stukken)] = factor_aantal;
  factor_functies[factor_aantal++] = fW;
  mapSchaalFactor[key(ZWART, stukken) ^ BlackModifier] = factor_aantal;
  factor_functies[factor_aantal++] = fB;
}

int Eindspelen::probe_waarde(Sleutel64 key)
{
	FunctieIndexMap::const_iterator iter = mapWaarde.find(key);
	return iter == mapWaarde.end() ? -1 : iter->second;
}

int Eindspelen::probe_schaalfactor(Sleutel64 key, Kleur& sterkeZijde)
{
	FunctieIndexMap::const_iterator iter = mapSchaalFactor.find(key);
	if (iter != mapSchaalFactor.end())
		return sterkeZijde = WIT, iter->second;
	iter = mapSchaalFactor.find(key ^ BlackModifier);
	if (iter != mapSchaalFactor.end())
		return sterkeZijde = ZWART, iter->second;
	return -1;
}


/// Mate with KX vs K. This function is used to evaluate positions with
/// king and plenty of material vs a lone king. It simply gives the
/// attacking side a bonus for driving the defending king towards the edge
/// of the board, and for keeping the distance between the two kings small.
template <Kleur sterkeZijde>
Waarde endgameValue_KXK(const Stelling& pos) {

  const Kleur zwakkeZijde = ~sterkeZijde;
  assert(verify_material(pos, zwakkeZijde, MATL_0, 0));
  assert(!pos.schaak_gevers()); // Eval is never called when in check

  // Stalemate detection with lone king
  if (pos.aan_zet() == zwakkeZijde && !ZettenLijst<GEN_LEGALE_ZETTEN>(pos).size())
      return REMISE_WAARDE;

  Veld sterkeK = pos.veld<KONING>(sterkeZijde);
  Veld zwakkeK = pos.veld<KONING>(zwakkeZijde);

  Waarde result = WaardeVanMateriaal(pos.niet_pion_materiaal(sterkeZijde))
                + pos.aantal<PION>(sterkeZijde) * WAARDE_PION
                + DrijfNaarKant[zwakkeK]
                + DrijfDichter[afstand(sterkeK, zwakkeK)];

  if (result < WINST_WAARDE)
  if (   pos.aantal<DAME>(sterkeZijde) + pos.aantal<TOREN>(sterkeZijde)
      ||(pos.aantal<LOPER>(sterkeZijde) && pos.aantal<PAARD>(sterkeZijde))
      ||(pos.aantal<LOPER>(sterkeZijde) > 1 && verschillende_kleur(pos.veld_lijst<LOPER>(sterkeZijde)[0],
                                                              pos.veld_lijst<LOPER>(sterkeZijde)[1])))
      result += WINST_WAARDE;

  return sterkeZijde == pos.aan_zet() ? result : -result;
}


/// Mate with KBN vs K. This is similar to KX vs K, but we have to drive the
/// defending king towards a corner square of the right color.
template <Kleur sterkeZijde>
Waarde endgameValue_KBNK(const Stelling& pos) {

  const Kleur zwakkeZijde = ~sterkeZijde;
  assert(verify_material(pos, sterkeZijde, MATL_PAARD + MATL_LOPER, 0));
  assert(verify_material(pos, zwakkeZijde, MATL_0, 0));

  Veld sterkeK = pos.veld<KONING>(sterkeZijde);
  Veld zwakkeK = pos.veld<KONING>(zwakkeZijde);
  Veld bishopSq = pos.veld<LOPER>(sterkeZijde);

  // kbnk_mate_table() tries to drive toward corners A1 or H8. If we have a
  // bishop that cannot reach the above squares, we flip the kings in order
  // to drive the enemy toward corners A8 or H1.
  if (verschillende_kleur(bishopSq, SQ_A1))
  {
      sterkeK = ~sterkeK;
      zwakkeK  = ~zwakkeK;
  }

  Waarde result =  WINST_WAARDE
                + DrijfDichter[afstand(sterkeK, zwakkeK)]
                + DrijfNaarHoek[zwakkeK];

  return sterkeZijde == pos.aan_zet() ? result : -result;
}


/// KP vs K. This endgame is evaluated with the help of a bitbase.
template <Kleur sterkeZijde>
Waarde endgameValue_KPK(const Stelling& pos) {

  const Kleur zwakkeZijde = ~sterkeZijde;
  assert(verify_material(pos, sterkeZijde, MATL_0, 1));
  assert(verify_material(pos, zwakkeZijde, MATL_0, 0));

  // Assume sterkeZijde is white and the pawn is on files A-D
  Veld wksq = normalize(pos, sterkeZijde, pos.veld<KONING>(sterkeZijde));
  Veld bksq = normalize(pos, sterkeZijde, pos.veld<KONING>(zwakkeZijde));
  Veld psq  = normalize(pos, sterkeZijde, pos.veld<PION>(sterkeZijde));

  Kleur us = sterkeZijde == pos.aan_zet() ? WIT : ZWART;

  if (!Bitbases::probe(wksq, psq, bksq, us))
      return REMISE_WAARDE;

  Waarde result = WINST_WAARDE + maak_waarde(50) * int(rij(psq));

  return sterkeZijde == pos.aan_zet() ? result : -result;
}


/*
test posities voor KR_KP (randgevallen)
=======================================
remise
R7/8/8/7K/4p3/4k3/8/8 b - - 0 1
4R3/1K6/8/2k1p3/8/8/8/8 b - - 0 1
R7/2K5/8/2k1p3/8/8/8/8 b - - 0 1
R7/7K/8/4p3/2k5/8/8/8 b - - 0 1
6R1/5K2/8/3p4/2k5/8/8/8 b - - 0 1
3R4/2K5/8/8/2p5/2k5/8/8 w - - 0 1
K5R1/8/6p1/5k2/8/8/8/8 b - - 0 1
=> require 14 - 18 - 16 - 13 - 14 - 14 - 15 plies om remise zonder KR_KP extensie te herkennen

winst
4R3/8/8/8/4p2K/3k4/8/8 b - - 0 1
4R3/2K5/8/2k1p3/8/8/8/8 b - - 0 1
R7/7K/8/2k1p3/8/8/8/8 b - - 0 1
2R5/1K6/8/8/1p6/1k6/8/8 w - - 0 1
K5R1/8/6p1/5k2/8/8/8/8 w - - 0 1
*/
template <Kleur sterkeZijde>
SchaalFactor endgameScaleFactor_KRKP(const Stelling& pos) {

	const Kleur zwakkeZijde = ~sterkeZijde;
	assert(verify_material(pos, sterkeZijde, MATL_TOREN, 0));
	assert(verify_material(pos, zwakkeZijde, MATL_0, 1));

	Veld witte_koning = relatief_veld(sterkeZijde, pos.veld<KONING>(sterkeZijde));
	Veld zwarte_koning = relatief_veld(sterkeZijde, pos.veld<KONING>(zwakkeZijde));
	Veld toren = relatief_veld(sterkeZijde, pos.veld<TOREN>(sterkeZijde));
	Veld pion = relatief_veld(sterkeZijde, pos.veld<PION>(zwakkeZijde));

	Veld promotie = maak_veld(lijn(pion), RIJ_1);

	int promotie_afstand, WK_afstand, ZK_afstand, WK_afstand2;
	bool pion_aangevallen, toren_achter_pion;

	if (!((pos.aanval_van<KONING>(witte_koning) | pos.stukken(sterkeZijde, KONING)) & bb_rijen_voorwaarts(zwakkeZijde, pion)) &&
		(rij(pion) <= RIJ_3 || rij(zwarte_koning) <= rij(pion)))
	{
		ZK_afstand = VeldAfstand[zwarte_koning][pion];
		pion_aangevallen = (pos.aanval_van<TOREN>(pos.veld<TOREN>(sterkeZijde)) | pos.aanval_van<KONING>(pos.veld<KONING>(sterkeZijde))) 
			& pos.veld<PION>(zwakkeZijde);

		if (ZK_afstand <= 1 + (pos.aan_zet() == zwakkeZijde || !pion_aangevallen))
		{
			toren_achter_pion = (lijn(pion) == lijn(toren));
			promotie_afstand = rij(pion) + VeldAfstand[zwarte_koning][promotie] - 2 + (toren_achter_pion);
			WK_afstand = VeldAfstand[witte_koning][promotie] - (pos.aan_zet() == sterkeZijde);
			if ((lijn(witte_koning) < lijn(pion) && lijn(zwarte_koning) < lijn(pion) ||
				lijn(witte_koning) > lijn(pion) && lijn(zwarte_koning) > lijn(pion)) &&
				rij(witte_koning) >= rij(zwarte_koning))
				WK_afstand++;

			if ((lijn(witte_koning) < lijn(zwarte_koning) && lijn(zwarte_koning) < lijn(pion) ||
				lijn(witte_koning) > lijn(zwarte_koning) && lijn(zwarte_koning) > lijn(pion)) &&
				rij(witte_koning) >= rij(zwarte_koning))
				WK_afstand++;

			if (WK_afstand > promotie_afstand)
			{
				WK_afstand2 = VeldAfstand[witte_koning][pion] - (pos.aan_zet() == sterkeZijde);
				if (WK_afstand2 > ZK_afstand ||
					rij(witte_koning) - (pos.aan_zet() == sterkeZijde) > rij(pion))
					return REMISE_FACTOR;
			}
		}
	}
	return GEEN_FACTOR;
}


/// KR vs KB. This is very simple, and always returns drawish scores.  The
/// score is slightly bigger when the defending king is close to the edge.
/*template <Color sterkeZijde>
Value endgameValue_KRKB(const Position& pos) {

  const Color zwakkeZijde = ~sterkeZijde;
  assert(verify_material(pos, sterkeZijde, MATL_TOREN, 0));
  assert(verify_material(pos, zwakkeZijde, MATL_LOPER, 0));

  Value result = DrijfNaarKant[pos.square<KONING>(zwakkeZijde)] / 4;
  return sterkeZijde == pos.aan_zet() ? result : -result;
}*/


/// KR vs KN. The attacking side has slightly better winning chances than
/// in KR vs KB, particularly if the king and the knight are far apart.
/*template <Color sterkeZijde>
Value endgameValue_KRKN(const Position& pos) {

  const Color zwakkeZijde = ~sterkeZijde;
  assert(verify_material(pos, sterkeZijde, MATL_TOREN, 0));
  assert(verify_material(pos, zwakkeZijde, MATL_PAARD, 0));

  Square bksq = pos.square<KONING>(zwakkeZijde);
  Square bnsq = pos.square<PAARD>(zwakkeZijde);
  Value result = (DrijfNaarKant[bksq] + DrijfVerder[distance(bksq, bnsq)]) / 4;
  return sterkeZijde == pos.aan_zet() ? result : -result;
}*/


/*
test posities voor KQ_KP
========================
remise
7Q/8/8/4K3/8/8/p7/1k6 w - - 0 1
Q7/8/8/8/8/8/pk3K2/8 w - - 0 1
2Q5/8/4K3/8/8/8/1kp5/8 w - - 0 1
7Q/8/8/4K3/8/8/2pk4/8 w - - 0 1
without KQ_KP extension even at 30 ply evaluations are higher than +6
4Q3/8/8/3K4/8/8/2pk4/8 w - - 0 1  => uitzonderingspositie op de regel van Euwe !!!

winst
8/8/8/8/8/8/pk3KQ1/8 w - - 0 1
Q7/8/8/K7/8/8/pk6/8 w - - 0 1
7Q/8/8/3K4/8/8/2pk4/8 w - - 0 1
8/8/1Q6/1K6/8/8/1kp5/8 w - - 0 1
*/
template <Kleur sterkeZijde>
SchaalFactor endgameScaleFactor_KQKP(const Stelling& pos) {

  const Kleur zwakkeZijde = ~sterkeZijde;
  assert(verify_material(pos, sterkeZijde, MATL_DAME, 0));
  assert(verify_material(pos, zwakkeZijde, MATL_0, 1));

  if (!(pos.stukken(zwakkeZijde, PION) & (sterkeZijde == WIT ? bb4(SQ_A2, SQ_C2, SQ_F2, SQ_H2) : bb4(SQ_A7, SQ_C7, SQ_F7, SQ_H7))))
	  return GEEN_FACTOR;

  Veld witte_koning = relatief_veld(sterkeZijde, pos.veld<KONING>(sterkeZijde));
  Veld zwarte_koning = relatief_veld(sterkeZijde, pos.veld<KONING>(zwakkeZijde));
  Veld dame = relatief_veld(sterkeZijde, pos.veld<DAME>(sterkeZijde));
  Veld pion = relatief_veld(sterkeZijde, pos.veld<PION>(zwakkeZijde));

  Bitboard aanval = pos.aanval_van<DAME>(pos.veld<DAME>(sterkeZijde)) | pos.aanval_van<KONING>(pos.veld<KONING>(sterkeZijde));
  bool wit_aan_zet = pos.aan_zet() == sterkeZijde;
  Bitboard ZK_schaak = pos.schaak_gevers();

  int flop = sterkeZijde == WIT ? 0 : 070;
  bool tempo = !wit_aan_zet || !(aanval & Veld(pion ^ flop));
  if (dame != pion - 8 && rij(zwarte_koning) <= RIJ_2 + tempo
	  && (VeldAfstand[zwarte_koning][pion] <= 1 + tempo || pion == SQ_C2 && zwarte_koning == SQ_A1 || pion == SQ_F2 && zwarte_koning == SQ_H1))
  {
	  if (lijn(pion) >= LIJN_E)
	  {
		  flop ^= 07;
		  pion = Veld(pion ^ 07);
		  witte_koning = Veld(witte_koning ^ 07);
		  zwarte_koning = Veld(zwarte_koning ^ 07);
		  dame = Veld(dame ^ 07);
	  }
	  if (pion == SQ_A2 && (lijn(witte_koning) > LIJN_E || witte_koning > SQ_D5))
		  return REMISE_FACTOR;
	  if (pion == SQ_C2 && lijn(zwarte_koning) < LIJN_C && (lijn(witte_koning) > LIJN_E || witte_koning > SQ_C4))
		  return REMISE_FACTOR;
	  if (pion == SQ_C2 && lijn(zwarte_koning) >= LIJN_C && (lijn(witte_koning) > LIJN_G || witte_koning > SQ_D5))
		  return REMISE_FACTOR;

	  if (!wit_aan_zet && !ZK_schaak)
	  {
		  if (pion == SQ_A2 && (VeldAfstand[SQ_A1][zwarte_koning] == 1 || lijn(dame) != LIJN_A && !(aanval & Veld(SQ_A1 ^ flop))))
			  return REMISE_FACTOR;

		  if (pion == SQ_C2 && (VeldAfstand[SQ_C1][zwarte_koning] == 1 || lijn(dame) != LIJN_C && !(aanval & Veld(SQ_C1 ^ flop))))
			  return REMISE_FACTOR;
	  }
  }
  return GEEN_FACTOR;
}


/// KQ vs KR.  This is almost identical to KX vs K:  We give the attacking
/// king a bonus for having the kings close together, and for forcing the
/// defending king towards the edge. If we also take care to avoid null move for
/// the defending side in the search, this is usually sufficient to win KQ vs KR.
template <Kleur sterkeZijde>
Waarde endgameValue_KQKR(const Stelling& pos) {

  const Kleur zwakkeZijde = ~sterkeZijde;
  assert(verify_material(pos, sterkeZijde, MATL_DAME, 0));
  assert(verify_material(pos, zwakkeZijde, MATL_TOREN, 0));

  Veld sterkeK = pos.veld<KONING>(sterkeZijde);
  Veld zwakkeK = pos.veld<KONING>(zwakkeZijde);

  Waarde result =  4 * WAARDE_PION
                + DrijfNaarKant[zwakkeK]
                + DrijfDichter[afstand(sterkeK, zwakkeK)];

  return sterkeZijde == pos.aan_zet() ? result : -result;
}


/// Some cases of trivial draws
template <Kleur sterkeZijde>
Waarde endgameValue_KNNK(const Stelling& pos) { 

  return REMISE_WAARDE; 
}


/*
test posities
remise
k7/8/K7/P7/8/2B5/P7/8 w - - 0 1
8/Bk6/1P6/2K5/8/8/8/8 w - - 0 1 Ponziani
1k6/p7/P2K4/8/4B3/8/8/8 w - - 0 1
1k6/p7/P2K4/8/Pp2B3/8/8/8 w - - 0 1
2k5/1p6/1P2K3/8/5B2/8/8/8 w - - 0 1
2k5/1p6/1Pp1K3/8/1P3B2/8/8/8 w - - 0 1
2k5/1p6/1P2K3/8/4B3/8/8/8 w - - 0 1
KBP v KP: 8/8/6K1/8/p7/P4k2/8/4B3 w - - OK!
KBP v KP: 1b6/7p/8/1K5P/8/8/8/5k2 b - - OK!
KBP v KP: 4b3/8/p7/P3K3/8/8/8/2k5 b - - OK!
KBPP v KPP: 2k5/7p/6pP/6P1/5B2/3K4/8/8 w - - OK!
win
1k6/p7/Pp1K4/8/P3B3/8/8/8 w - - 0 1
8/Bpk5/8/P2K4/8/8/8/8 w - - 0 1
*/
template <Kleur sterkeZijde>
SchaalFactor endgameScaleFactor_KBPsK(const Stelling& pos) {

  const Kleur zwakkeZijde = ~sterkeZijde;
  assert(pos.niet_pion_materiaal(sterkeZijde) == MATL_LOPER);
  assert(pos.aantal<PION>(sterkeZijde) >= 1);

  // kan laatste pion geslagen worden?
  Bitboard bb_zwart_aanval = aanval_koning_inc(pos.veld<KONING>(zwakkeZijde));
  Bitboard bb_wit_aanval = pos.aanval_van<KONING>(pos.veld<KONING>(sterkeZijde)) | pos.aanval_van<LOPER>(pos.veld<LOPER>(sterkeZijde));
  if (pos.aantal<PION>(sterkeZijde) == 1 && pos.aan_zet() == zwakkeZijde
	  && pos.stukken(sterkeZijde, PION) & bb_zwart_aanval & ~bb_wit_aanval)
	  return REMISE_FACTOR;

  Veld witte_koning = relatief_veld(sterkeZijde, pos.veld<KONING>(sterkeZijde));
  Veld zwarte_koning = relatief_veld(sterkeZijde, pos.veld<KONING>(zwakkeZijde));
  Veld loper = relatief_veld(sterkeZijde, pos.veld<LOPER>(sterkeZijde));
  Veld pion = relatief_veld(sterkeZijde, pos.veld_lijst<PION>(sterkeZijde)[0]);
  Bitboard witte_pionnen = 0, zwarte_pionnen = 0;

  if (DonkereVelden & loper)
  {
	  witte_koning = Veld(witte_koning ^ 07);
	  zwarte_koning = Veld(zwarte_koning ^ 07);
	  loper = Veld(loper ^ 07);
	  pion = Veld(pion ^ 07);
	  for (int i = 0; i < pos.aantal<PION>(sterkeZijde); i++)
		  witte_pionnen |= relatief_veld(sterkeZijde, Veld(pos.veld_lijst<PION>(sterkeZijde)[i] ^ 07));
	  for (int i = 0; i < pos.aantal<PION>(sterkeZijde); i++)
		  zwarte_pionnen |= relatief_veld(sterkeZijde, Veld(pos.veld_lijst<PION>(zwakkeZijde)[i] ^ 07));
  }
  else
  {
	  for (int i = 0; i < pos.aantal<PION>(sterkeZijde); i++)
		  witte_pionnen |= relatief_veld(sterkeZijde, pos.veld_lijst<PION>(sterkeZijde)[i]);
	  for (int i = 0; i < pos.aantal<PION>(sterkeZijde); i++)
		  zwarte_pionnen |= relatief_veld(sterkeZijde, pos.veld_lijst<PION>(zwakkeZijde)[i]);
  }
  // vanaf hier hebben we de situatie met een witte loper op de lichte velden

  Bitboard bb_koning_dekt = aanval_koning_inc(zwarte_koning);
  if (pos.aantal<PION>(sterkeZijde) > 1)
	  pion = SQ_NONE;

	if (!(witte_pionnen & ~FileHBB))
	{
		if (bb_koning_dekt & bb(SQ_H8))
			if (witte_pionnen & bb(SQ_H5) && (zwarte_pionnen & bb2(SQ_G7, SQ_H6)) == bb2(SQ_G7, SQ_H6))
				;
			else
				return REMISE_FACTOR;
		if (!(zwarte_pionnen & FileGBB)
			&& VeldAfstand[SQ_G7][zwarte_koning] < VeldAfstand[SQ_G7][witte_koning] + 1 - (pos.aan_zet() == sterkeZijde)
			&& VeldAfstand[SQ_H8][zwarte_koning] < 7 - rij(msb(witte_pionnen)) - (pos.aan_zet() == sterkeZijde))
			return SchaalFactor(NORMAAL_FACTOR / 4);
		if (!(witte_pionnen & ~bb2(SQ_H2, SQ_H3)) && (zwarte_pionnen & bb(SQ_H4)))
			return SchaalFactor(NORMAAL_FACTOR / 4);
	}
	if (pion == SQ_G6 && loper == SQ_H7 && bb_koning_dekt & bb(SQ_H8))
		return REMISE_FACTOR;
	if (!(witte_pionnen & ~FileABB) && witte_pionnen & bb(SQ_A6) &&
		zwarte_pionnen & bb(SQ_A7) && bb_koning_dekt & bb(SQ_B8) &&
		(!(zwarte_pionnen & FileBBB) ||
			rij(lsb(witte_pionnen & FileABB)) >= rij(msb(zwarte_pionnen & FileBBB))))
		return REMISE_FACTOR;
	if (!(witte_pionnen & ~FileGBB) && witte_pionnen & bb(SQ_G6) &&
		zwarte_pionnen & bb(SQ_G7) && bb_koning_dekt & bb2(SQ_G8, SQ_F8))
		return REMISE_FACTOR;
	if (pion == SQ_B6 && zwarte_pionnen & bb(SQ_B7) && bb_koning_dekt & bb2(SQ_B8, SQ_C8))
		return REMISE_FACTOR;
	// witte pionnen a6-b5-c4 geblokkeerd door een keten van a7-b6-c5
	if (witte_pionnen == bb2(SQ_A6, SQ_B5)
		&& (zwarte_pionnen & (bb2(SQ_A7, SQ_B6) | bb3(SQ_B7, SQ_C7, SQ_C6))) == bb2(SQ_A7, SQ_B6)
		&& bb_koning_dekt & bb(SQ_B8))
		return REMISE_FACTOR;
	if (witte_pionnen == bb3(SQ_A6, SQ_B5, SQ_C4)
		&& (zwarte_pionnen & (bb3(SQ_A7, SQ_B6, SQ_C5) | bb3(SQ_B7, SQ_C7, SQ_C6) | bb3(SQ_D7, SQ_D6, SQ_D5))) == bb3(SQ_A7, SQ_B6, SQ_C5)
		&& bb_koning_dekt & bb(SQ_B8))
		return REMISE_FACTOR;

  return GEEN_FACTOR;
}

/*
remise
8/4kp2/4r3/8/5QK1/8/8/8 w - - 0 1
winst
8/6kp/6r1/8/4KQ2/8/8/8 w - - 0 1
*/
template <Kleur sterkeZijde>
SchaalFactor endgameScaleFactor_KQKRPs(const Stelling& pos) {

  const Kleur zwakkeZijde = ~sterkeZijde;
  assert(verify_material(pos, sterkeZijde, MATL_DAME, 0));
  assert(pos.aantal<TOREN>(zwakkeZijde) == 1);
  assert(pos.aantal<PION>(zwakkeZijde) >= 1);

  Veld kingSq = pos.veld<KONING>(zwakkeZijde);
  Veld rsq = pos.veld<TOREN>(zwakkeZijde);

  if (    relatieve_rij(zwakkeZijde, kingSq) <= RIJ_2
      &&  relatieve_rij(zwakkeZijde, pos.veld<KONING>(sterkeZijde)) >= RIJ_4
      &&  relatieve_rij(zwakkeZijde, rsq) == RIJ_3
      && (  pos.stukken(zwakkeZijde, PION)
		  & ~FileABB & ~FileHBB // a en h pion maken geen vesting !
          & pos.aanval_van<KONING>(kingSq)
          & pos.aanval_van<PION>(rsq, sterkeZijde)))
          return REMISE_FACTOR;

  return EEN_PION_FACTOR;
}


/// KRP vs KR. This function knows a handful of the most important classes of
/// drawn positions, but is far from perfect. It would probably be a good idea
/// to add more knowledge in the future.
///
/// It would also be nice to rewrite the actual code for this function,
/// which is mostly copied from Glaurung 1.x, and isn't very pretty.
template <Kleur sterkeZijde>
SchaalFactor endgameScaleFactor_KRPKR(const Stelling& pos) {

  const Kleur zwakkeZijde = ~sterkeZijde;
  assert(verify_material(pos, sterkeZijde, MATL_TOREN, 1));
  assert(verify_material(pos, zwakkeZijde, MATL_TOREN, 0));

  // Assume sterkeZijde is white and the pawn is on files A-D
  Veld wksq = normalize(pos, sterkeZijde, pos.veld<KONING>(sterkeZijde));
  Veld bksq = normalize(pos, sterkeZijde, pos.veld<KONING>(zwakkeZijde));
  Veld wrsq = normalize(pos, sterkeZijde, pos.veld<TOREN>(sterkeZijde));
  Veld wpsq = normalize(pos, sterkeZijde, pos.veld<PION>(sterkeZijde));
  Veld brsq = normalize(pos, sterkeZijde, pos.veld<TOREN>(zwakkeZijde));

  Lijn f = lijn(wpsq);
  Rij r = rij(wpsq);
  Veld queeningSq = maak_veld(f, RIJ_8);
  int tempo = (pos.aan_zet() == sterkeZijde);

  // If the pawn is not too far advanced and the defending king defends the
  // queening square, use the third-rank defence.
  if (   r <= RIJ_5
      && afstand(bksq, queeningSq) <= 1
      && wksq <= SQ_H5
      && (rij(brsq) == RIJ_6 || (r <= RIJ_3 && rij(wrsq) != RIJ_6)))
      return REMISE_FACTOR;

  // The defending side saves a draw by checking from behind in case the pawn
  // has advanced to the 6th rank with the king behind.
  if (   r == RIJ_6
      && afstand(bksq, queeningSq) <= 1
      && rij(wksq) + tempo <= RIJ_6
      && (rij(brsq) == RIJ_1 || (!tempo && lijn_afstand(brsq, wpsq) >= 3)))
      return REMISE_FACTOR;

  if (   r >= RIJ_6
      && bksq == queeningSq
      && rij(brsq) == RIJ_1
      && (!tempo || afstand(wksq, wpsq) >= 2))
      return REMISE_FACTOR;

  // White pawn on a7 and rook on a8 is a draw if black's king is on g7 or h7
  // and the black rook is behind the pawn.
  if (   wpsq == SQ_A7
      && wrsq == SQ_A8
      && (bksq == SQ_H7 || bksq == SQ_G7)
      && lijn(brsq) == LIJN_A
      && (rij(brsq) <= RIJ_3 || lijn(wksq) >= LIJN_D || rij(wksq) <= RIJ_5))
      return REMISE_FACTOR;

  // If the defending king blocks the pawn and the attacking king is too far
  // away, it's a draw.
  if (   r <= RIJ_5
      && bksq == wpsq + BOVEN
      && afstand(wksq, wpsq) - tempo >= 2
      && afstand(wksq, brsq) - tempo >= 2)
      return REMISE_FACTOR;

  // Pawn on the 7th rank supported by the rook from behind usually wins if the
  // attacking king is closer to the queening square than the defending king,
  // and the defending king cannot gain tempi by threatening the attacking rook.
  if (   r == RIJ_7
      && f != LIJN_A
      && lijn(wrsq) == f
      && wrsq != queeningSq
      && (afstand(wksq, queeningSq) < afstand(bksq, queeningSq) - 2 + tempo)
      && (afstand(wksq, queeningSq) < afstand(bksq, wrsq) + tempo))
      return SchaalFactor(MAX_FACTOR - 3 * afstand(wksq, queeningSq));

  // Similar to the above, but with the pawn further back
  if (   f != LIJN_A
      && lijn(wrsq) == f
      && wrsq < wpsq
      && (afstand(wksq, queeningSq) < afstand(bksq, queeningSq) - 2 + tempo)
      && (afstand(wksq, wpsq + BOVEN) < afstand(bksq, wpsq + BOVEN) - 2 + tempo)
      && (  afstand(bksq, wrsq) + tempo >= 3
          || (    afstand(wksq, queeningSq) < afstand(bksq, wrsq) + tempo
              && (afstand(wksq, wpsq + BOVEN) < afstand(bksq, wrsq) + tempo))))
      return SchaalFactor(MAX_FACTOR
                         - 12 * afstand(wpsq, queeningSq)
                         - 3 * afstand(wksq, queeningSq));

  // If the pawn is not far advanced and the defending king is somewhere in
  // the pawn's path, it's probably a draw.
  if (r <= RIJ_4 && bksq > wpsq)
  {
      if (lijn(bksq) == lijn(wpsq))
          return SchaalFactor(16);
      if (   lijn_afstand(bksq, wpsq) == 1
          && afstand(wksq, bksq) > 2)
          return SchaalFactor(38 - 3 * afstand(wksq, bksq));
  }
  return GEEN_FACTOR;
}

template <Kleur sterkeZijde>
SchaalFactor endgameScaleFactor_KRPKB(const Stelling& pos) {

  const Kleur zwakkeZijde = ~sterkeZijde;
  assert(verify_material(pos, sterkeZijde, MATL_TOREN, 1));
  assert(verify_material(pos, zwakkeZijde, MATL_LOPER, 0));

  // Test for a rook pawn
  if (pos.stukken(PION) & (FileABB | FileHBB))
  {
      Veld veldK = pos.veld<KONING>(zwakkeZijde);
      Veld bsq = pos.veld<LOPER>(zwakkeZijde);
      Veld psq = pos.veld<PION>(sterkeZijde);
      Rij rk = relatieve_rij(sterkeZijde, psq);
      Veld push = pion_vooruit(sterkeZijde);

      // If the pawn is on the 5th rank and the pawn (currently) is on
      // the same color square as the bishop then there is a chance of
      // a fortress. Depending on the king position give a moderate
      // reduction or a stronger one if the defending king is near the
      // corner but not trapped there.
      if (rk == RIJ_5 && !verschillende_kleur(bsq, psq))
      {
          int d = afstand(psq + 3 * push, veldK);

          if (d <= 2 && !(d == 0 && veldK == pos.veld<KONING>(sterkeZijde) + 2 * push))
              return SchaalFactor(38);
          else
              return SchaalFactor(75);
      }

      // When the pawn has moved to the 6th rank we can be fairly sure
      // it's drawn if the bishop attacks the square in front of the
      // pawn from a reasonable distance and the defending king is near
      // the corner
      if (   rk == RIJ_6
          && afstand(psq + 2 * push, veldK) <= 1
          && (LegeAanval[LOPER][bsq] & (psq + push))
          && lijn_afstand(bsq, psq) >= 2)
          return SchaalFactor(12);
  }

  return GEEN_FACTOR;
}

/// KRPP vs KRP. There is just a single rule: if the stronger side has no passed
/// pawns and the defending king is actively placed, the position is drawish.
template <Kleur sterkeZijde>
SchaalFactor endgameScaleFactor_KRPPKRP(const Stelling& pos) {

  const Kleur zwakkeZijde = ~sterkeZijde;
  assert(verify_material(pos, sterkeZijde, MATL_TOREN, 2));
  assert(verify_material(pos, zwakkeZijde, MATL_TOREN, 1));

  Veld wpsq1 = pos.veld_lijst<PION>(sterkeZijde)[0];
  Veld wpsq2 = pos.veld_lijst<PION>(sterkeZijde)[1];
  Veld bksq = pos.veld<KONING>(zwakkeZijde);

  // Does the stronger side have a passed pawn?
  if (pos.is_vrijpion(sterkeZijde, wpsq1) || pos.is_vrijpion(sterkeZijde, wpsq2))
      return GEEN_FACTOR;

  Rij r = std::max(relatieve_rij(sterkeZijde, wpsq1), relatieve_rij(sterkeZijde, wpsq2));

  if (   lijn_afstand(bksq, wpsq1) <= 1
      && lijn_afstand(bksq, wpsq2) <= 1
      && relatieve_rij(sterkeZijde, bksq) > r)
  {
      assert(r > RIJ_1 && r < RIJ_7);
      return SchaalFactor(KRPPKRPScaleFactors[r]);
  }
  return GEEN_FACTOR;
}


/// K and two or more pawns vs K. There is just a single rule here: If all pawns
/// are on the same rook file and are blocked by the defending king, it's a draw.
template <Kleur sterkeZijde>
SchaalFactor endgameScaleFactor_KPsK(const Stelling& pos) {

  const Kleur zwakkeZijde = ~sterkeZijde;
  assert(pos.niet_pion_materiaal(sterkeZijde) == MATL_0);
  assert(pos.aantal<PION>(sterkeZijde) >= 2);
  assert(verify_material(pos, zwakkeZijde, MATL_0, 0));

  Veld veldK = pos.veld<KONING>(zwakkeZijde);
  Bitboard pawns = pos.stukken(sterkeZijde, PION);

  // If all pawns are ahead of the king, on a single rook file and
  // the king is within one file of the pawns, it's a draw.
  if (   !(pawns & ~bb_rijen_voorwaarts(zwakkeZijde, rij(veldK)))
      && !((pawns & ~FileABB) && (pawns & ~FileHBB))
      &&  lijn_afstand(veldK, lsb(pawns)) <= 1)
      return REMISE_FACTOR;

  return GEEN_FACTOR;
}


/*
test posities voor KBP_KB_identical
===================================
remise
4k3/8/2KP4/5b2/8/8/4B3/8 b - - 0 1
8/2K5/3P4/2k2b2/8/8/4B3/8 w - - 0 1

winst
4k3/8/2KP4/5b2/8/8/4B3/8 w - - 0 1

test posities voor KBP_KB_opposite
==================================
remise
8/4B3/3P4/4Kb2/6k1/8/8/8 w - - 0 1
8/4B3/3P4/4K3/b7/8/8/7k w - - 0 1
*/
const Bitboard AwerbachRegel = 0xfff7e3c180000000;

template <Kleur sterkeZijde>
SchaalFactor endgameScaleFactor_KBPKB(const Stelling& pos) {

  const Kleur zwakkeZijde = ~sterkeZijde;
  assert(verify_material(pos, sterkeZijde, MATL_LOPER, 1));
  assert(verify_material(pos, zwakkeZijde, MATL_LOPER, 0));

  int weakSideToMove = pos.aan_zet() == zwakkeZijde;
  if (weakSideToMove
	  && pos.aanval_van<LOPER>(pos.veld<PION>(sterkeZijde)) & pos.stukken(zwakkeZijde, LOPER)) //& ~ai.gepend[sterkeZijde])
	  return REMISE_FACTOR;

  if (verschillende_kleur(pos.veld<LOPER>(sterkeZijde), pos.veld<LOPER>(zwakkeZijde)))
  {
	  Veld witte_koning = pos.veld<KONING>(sterkeZijde);
	  Veld witte_loper = pos.veld<LOPER>(sterkeZijde);
	  Veld pion = pos.veld<PION>(sterkeZijde);
	  Veld zwarte_koning = pos.veld<KONING>(zwakkeZijde);
	  Veld zwarte_loper = pos.veld<LOPER>(zwakkeZijde);
	  Bitboard wit_aanval = pos.aanval_van<PION>(pion, WIT) | pos.aanval_van<LOPER>(witte_loper) | pos.aanval_van<KONING>(witte_koning);
	  Bitboard zwart_aanval = pos.aanval_van<LOPER>(zwarte_loper) | pos.aanval_van<KONING>(zwarte_koning);
	  if (relatieve_rij(sterkeZijde, pion) <= RIJ_5)
		  return REMISE_FACTOR;
	  if (bb_voorwaarts(sterkeZijde, pion) & zwarte_koning && verschillende_kleur(zwarte_koning, witte_loper))
		  return REMISE_FACTOR;
	  if (bb_voorwaarts(sterkeZijde, pion) & (pos.aanval_van<LOPER>(zwarte_loper) | zwarte_loper)
		  && (weakSideToMove || (!(wit_aanval & zwarte_loper))))
		  return REMISE_FACTOR;
  }
  else
  {
	  int flop = sterkeZijde == WIT ? ((pos.stukken(WIT, LOPER) & DonkereVelden) ? 0 : 07)
		  : ((pos.stukken(ZWART, LOPER) & ~DonkereVelden) ? 070 : 077);

	  Veld pion = Veld(pos.veld<PION>(sterkeZijde) ^ flop);
	  Veld witte_koning = Veld(pos.veld<KONING>(sterkeZijde) ^ flop);
	  Veld zwarte_koning = Veld(pos.veld<KONING>(zwakkeZijde) ^ flop);
	  Veld witte_loper = Veld(pos.veld<LOPER>(sterkeZijde) ^ flop);
	  // hier aangekomen hebben we lopers op donkere velden, en wit is de sterke partij

	  if (zwarte_koning > pion && lijn(pion) == lijn(zwarte_koning) && ~DonkereVelden & zwarte_koning)
		  return REMISE_FACTOR;

	  if (VeldAfstand[pion][zwarte_koning] == 1 && rij(zwarte_koning) < rij(pion)
		  && (abs(lijn(zwarte_koning) - lijn(witte_koning)) <= weakSideToMove)
		  && !(AwerbachRegel & pion))
		  return REMISE_FACTOR;
  }

  return GEEN_FACTOR;
}


/// KBPP vs KB. It detects a few basic draws with opposite-colored bishops
template <Kleur sterkeZijde>
SchaalFactor endgameScaleFactor_KBPPKB(const Stelling& pos) {

  const Kleur zwakkeZijde = ~sterkeZijde;
  assert(verify_material(pos, sterkeZijde, MATL_LOPER, 2));
  assert(verify_material(pos, zwakkeZijde, MATL_LOPER, 0));

  Veld wbsq = pos.veld<LOPER>(sterkeZijde);
  Veld bbsq = pos.veld<LOPER>(zwakkeZijde);

  if (!verschillende_kleur(wbsq, bbsq))
      return GEEN_FACTOR;

  Veld veldK = pos.veld<KONING>(zwakkeZijde);
  Veld psq1 = pos.veld_lijst<PION>(sterkeZijde)[0];
  Veld psq2 = pos.veld_lijst<PION>(sterkeZijde)[1];
  Veld blockSq1, blockSq2;

  if (relatieve_rij(sterkeZijde, psq1) > relatieve_rij(sterkeZijde, psq2))
  {
      blockSq1 = psq1 + pion_vooruit(sterkeZijde);
      blockSq2 = maak_veld(lijn(psq2), rij(psq1));
  }
  else
  {
      blockSq1 = psq2 + pion_vooruit(sterkeZijde);
      blockSq2 = maak_veld(lijn(psq1), rij(psq2));
  }

  switch (lijn_afstand(psq1, psq2))
  {
  case 0:
    // Both pawns are on the same file. It's an easy draw if the defender firmly
    // controls some square in the frontmost pawn's path.
    if (   lijn(veldK) == lijn(blockSq1)
        && relatieve_rij(sterkeZijde, veldK) >= relatieve_rij(sterkeZijde, blockSq1)
        && verschillende_kleur(veldK, wbsq))
        return REMISE_FACTOR;
    else
        return GEEN_FACTOR;

  case 1:
    // Pawns on adjacent files. It's a draw if the defender firmly controls the
    // square in front of the frontmost pawn's path, and the square diagonally
    // behind this square on the file of the other pawn.
    if (   veldK == blockSq1
        && verschillende_kleur(veldK, wbsq)
        && (   bbsq == blockSq2
            || (pos.aanval_van<LOPER>(blockSq2) & pos.stukken(zwakkeZijde, LOPER))
            || rij_afstand(psq1, psq2) >= 2))
        return REMISE_FACTOR;

    else if (   veldK == blockSq2
             && verschillende_kleur(veldK, wbsq)
             && (   bbsq == blockSq1
                 || (pos.aanval_van<LOPER>(blockSq1) & pos.stukken(zwakkeZijde, LOPER))))
        return REMISE_FACTOR;
    else
        return GEEN_FACTOR;

  default:
    // The pawns are not on the same file or adjacent files. No scaling.
    return GEEN_FACTOR;
  }
}


/// KBP vs KN. There is a single rule: If the defending king is somewhere along
/// the path of the pawn, and the square of the king is not of the same color as
/// the stronger side's bishop, it's a draw.
template <Kleur sterkeZijde>
SchaalFactor endgameScaleFactor_KBPKN(const Stelling& pos) {

  const Kleur zwakkeZijde = ~sterkeZijde;
  assert(verify_material(pos, sterkeZijde, MATL_LOPER, 1));
  assert(verify_material(pos, zwakkeZijde, MATL_PAARD, 0));

  Veld pawnSq = pos.veld<PION>(sterkeZijde);
  Veld strongBishopSq = pos.veld<LOPER>(sterkeZijde);
  Veld weakKingSq = pos.veld<KONING>(zwakkeZijde);

  if (   lijn(weakKingSq) == lijn(pawnSq)
      && relatieve_rij(sterkeZijde, pawnSq) < relatieve_rij(sterkeZijde, weakKingSq)
      && (   verschillende_kleur(weakKingSq, strongBishopSq)
          || relatieve_rij(sterkeZijde, weakKingSq) <= RIJ_6))
      return REMISE_FACTOR;

  return GEEN_FACTOR;
}


/*
test posities
remise
k7/P7/2N5/8/8/8/8/7K w - - 0 1
K7/P2k4/8/8/8/8/7N/8 b - - 0 1
K7/P2k4/8/8/8/8/8/7N b - - 0 1
*/
template <Kleur sterkeZijde>
SchaalFactor endgameScaleFactor_KNPK(const Stelling& pos) {

  const Kleur zwakkeZijde = ~sterkeZijde;
  assert(verify_material(pos, sterkeZijde, MATL_PAARD, 1));
  assert(verify_material(pos, zwakkeZijde, MATL_0, 0));

  // Assume sterkeZijde is white and the pawn is on files A-D
  Veld witte_koning = normalize(pos, sterkeZijde, pos.veld<KONING>(sterkeZijde));
  Veld paard = normalize(pos, sterkeZijde, pos.veld<PAARD>(sterkeZijde));
  Veld pion = normalize(pos, sterkeZijde, pos.veld<PION>(sterkeZijde));
  Veld zwarte_koning = normalize(pos, sterkeZijde, pos.veld<KONING>(zwakkeZijde));

  if (pion == SQ_A7 && afstand(SQ_A8, zwarte_koning) <= 1)
      return REMISE_FACTOR;

  if (pion == SQ_A7 && witte_koning == SQ_A8 &&
	  (zwarte_koning == SQ_C7 && (!(pos.aan_zet() == sterkeZijde) ^ !(~DonkereVelden & paard)) ||
		  zwarte_koning == SQ_C8 && (!(pos.aan_zet() == sterkeZijde) ^ !(DonkereVelden & paard))))
	  return REMISE_FACTOR;

  return GEEN_FACTOR;
}


/// KNP vs KB. If knight can block bishop from taking pawn, it's a win.
/// Otherwise the position is drawn.
template <Kleur sterkeZijde>
SchaalFactor endgameScaleFactor_KNPKB(const Stelling& pos) {

  const Kleur zwakkeZijde = ~sterkeZijde;
  Veld pawnSq = pos.veld<PION>(sterkeZijde);
  Veld bishopSq = pos.veld<LOPER>(zwakkeZijde);
  Veld weakKingSq = pos.veld<KONING>(zwakkeZijde);

  // King needs to get close to promoting pawn to prevent knight from blocking.
  // Rules for this are very tricky, so just approximate.
  if (bb_voorwaarts(sterkeZijde, pawnSq) & pos.aanval_van<LOPER>(bishopSq))
      return SchaalFactor(2 * afstand(weakKingSq, pawnSq));

  return GEEN_FACTOR;
}


/// KP vs KP. This is done by removing the weakest side's pawn and probing the
/// KP vs K bitbase: If the weakest side has a draw without the pawn, it probably
/// has at least a draw with the pawn as well. The exception is when the stronger
/// side's pawn is far advanced and not on a rook file; in this case it is often
/// possible to win (e.g. 8/4k3/3p4/3P4/6K1/8/8/8 w - - 0 1).
template <Kleur sterkeZijde>
SchaalFactor endgameScaleFactor_KPKP(const Stelling& pos) {

  const Kleur zwakkeZijde = ~sterkeZijde;
  assert(verify_material(pos, sterkeZijde, MATL_0, 1));
  assert(verify_material(pos, zwakkeZijde, MATL_0, 1));

  // Assume sterkeZijde is white and the pawn is on files A-D
  Veld wksq = normalize(pos, sterkeZijde, pos.veld<KONING>(sterkeZijde));
  Veld bksq = normalize(pos, sterkeZijde, pos.veld<KONING>(zwakkeZijde));
  Veld psq  = normalize(pos, sterkeZijde, pos.veld<PION>(sterkeZijde));

  Kleur us = sterkeZijde == pos.aan_zet() ? WIT : ZWART;

  // If the pawn has advanced to the fifth rank or further, and is not a
  // rook pawn, it's too dangerous to assume that it's at least a draw.
  if (rij(psq) >= RIJ_5 && lijn(psq) != LIJN_A)
      return GEEN_FACTOR;

  // Probe the KPK bitbase with the weakest side's pawn removed. If it's a draw,
  // it's probably at least a draw even with the pawn.
  return Bitbases::probe(wksq, psq, bksq, us) ? GEEN_FACTOR : REMISE_FACTOR;
}


void Eindspelen::init() {

	waarde_aantal = factor_aantal = 0;

	waarde_functies[waarde_aantal++] = &endgameValue_KXK<WIT>;    // index 0
	waarde_functies[waarde_aantal++] = &endgameValue_KXK<ZWART>;  // index 1

	factor_functies[factor_aantal++] = &endgameScaleFactor_KBPsK<WIT>;     // index 0
	factor_functies[factor_aantal++] = &endgameScaleFactor_KBPsK<ZWART>;   // index 1
	factor_functies[factor_aantal++] = &endgameScaleFactor_KQKRPs<WIT>;    // index 2
	factor_functies[factor_aantal++] = &endgameScaleFactor_KQKRPs<ZWART>;  // index 3
	factor_functies[factor_aantal++] = &endgameScaleFactor_KPsK<WIT>;      // index 4
	factor_functies[factor_aantal++] = &endgameScaleFactor_KPsK<ZWART>;    // index 5
	factor_functies[factor_aantal++] = &endgameScaleFactor_KPKP<WIT>;      // index 6
	factor_functies[factor_aantal++] = &endgameScaleFactor_KPKP<ZWART>;    // index 7


	// end games with complete evaluation
	//         KPNBRQ  KPNBRQ
	add_waarde("0110000 0100000", &endgameValue_KPK<WIT>, &endgameValue_KPK<ZWART>);
	add_waarde("0102000 0100000", &endgameValue_KNNK<WIT>, &endgameValue_KNNK<ZWART>);
	add_waarde("0101100 0100000", &endgameValue_KBNK<WIT>, &endgameValue_KBNK<ZWART>);
	add_waarde("0100001 0100010", &endgameValue_KQKR<WIT>, &endgameValue_KQKR<ZWART>);
	//addValue("0100010 0101000", &endgameValue_KRKN<WHITE>, &endgameValue_KRKN<BLACK>);
	//addValue("0100010 0100100", &endgameValue_KRKB<WHITE>, &endgameValue_KRKB<BLACK>);
	//KXK,   // Generic "mate lone king" eval

	// end games with scale factor
	//               KPNBRQ  KPNBRQ
	add_schaalfactor("0100001 0110000", &endgameScaleFactor_KQKP<WIT>, &endgameScaleFactor_KQKP<ZWART>);
	add_schaalfactor("0100010 0110000", &endgameScaleFactor_KRKP<WIT>, &endgameScaleFactor_KRKP<ZWART>);
	add_schaalfactor("0110010 0100010", &endgameScaleFactor_KRPKR<WIT>, &endgameScaleFactor_KRPKR<ZWART>);
	add_schaalfactor("0120010 0110010", &endgameScaleFactor_KRPPKRP<WIT>, &endgameScaleFactor_KRPPKRP<ZWART>);
	add_schaalfactor("0110100 0100100", &endgameScaleFactor_KBPKB<WIT>, &endgameScaleFactor_KBPKB<ZWART>);
	add_schaalfactor("0120100 0100100", &endgameScaleFactor_KBPPKB<WIT>, &endgameScaleFactor_KBPPKB<ZWART>);
	add_schaalfactor("0111000 0100000", &endgameScaleFactor_KNPK<WIT>, &endgameScaleFactor_KNPK<ZWART>);
	add_schaalfactor("0111000 0100100", &endgameScaleFactor_KNPKB<WIT>, &endgameScaleFactor_KNPKB<ZWART>);
	add_schaalfactor("0110100 0101000", &endgameScaleFactor_KBPKN<WIT>, &endgameScaleFactor_KBPKN<ZWART>);
	add_schaalfactor("0110010 0100100", &endgameScaleFactor_KRPKB<WIT>, &endgameScaleFactor_KRPKB<ZWART>);
	//KBPsK,   // KB and pawns vs K
	//KQKRPs,  // KQ vs KR and pawns
	//KPsK,    // K and pawns vs K
	//KPKP     // KP vs KP
}


template Waarde endgameValue_KXK<WIT>(const Stelling& pos);
template Waarde endgameValue_KXK<ZWART>(const Stelling& pos);
template SchaalFactor endgameScaleFactor_KBPsK<WIT>(const Stelling& pos);
template SchaalFactor endgameScaleFactor_KBPsK<ZWART>(const Stelling& pos);
template SchaalFactor endgameScaleFactor_KQKRPs<WIT>(const Stelling& pos);
template SchaalFactor endgameScaleFactor_KQKRPs<ZWART>(const Stelling& pos);
template SchaalFactor endgameScaleFactor_KPsK<WIT>(const Stelling& pos);
template SchaalFactor endgameScaleFactor_KPsK<ZWART>(const Stelling& pos);
template SchaalFactor endgameScaleFactor_KPKP<WIT>(const Stelling& pos);
template SchaalFactor endgameScaleFactor_KPKP<ZWART>(const Stelling& pos);
