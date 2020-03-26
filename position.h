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

#ifndef POSITION_H_INCLUDED
#define POSITION_H_INCLUDED

#include <cassert>
#include <cstddef>  // For offsetof()
#include <string>

#include "bitboard.h"
#include "types.h"

class Stelling;
class Thread;
struct ThreadInfo;
struct NumaInfo;
template<typename T, bool CM> struct StukVeldStatistiek;
typedef StukVeldStatistiek<SorteerWaarde, true> CounterZetWaarden;
struct ZetEx;

namespace PSQT {

  extern Score psq[STUK_N][VELD_N];

  void init();
}

#define KAANVAL
//#define SFAANVAL

struct AanvalInfo {
	Bitboard aanval[KLEUR_N][STUKTYPE_N];
	Bitboard gepend[KLEUR_N];
	Bitboard mobiliteit_mask[KLEUR_N];
#ifdef KAANVAL
	int KAanvalScore[KLEUR_N];
#endif
#ifdef SFAANVAL
	Bitboard kingRing[KLEUR_N];
	int kingAttackersCount[KLEUR_N];
	int kingAttackersWeight[KLEUR_N];
	int kingAdjacentZoneAttacksCount[KLEUR_N];
#endif
};



/// StellingInfo struct stores information needed to restore a Position object to
/// its previous state when we retract a move. Whenever a move is made on the
/// board (by calling Position::speel_zet), a StellingInfo object must be passed.

struct StellingInfo {

  // Copied when making a move
  Sleutel64 pionSleutel;
  Sleutel64 materiaalSleutel, loperKleurSleutel;
  MateriaalWaarde nietPionMateriaal[KLEUR_N];
  Score psq;
  int8_t rokadeMogelijkheden, fase, aantalContempt[2];
  Veld enpassantVeld;

  // Not copied when making a move
  Sleutel64 sleutel;
  int remise50Zetten, afstandTotNullzet, ply, zetNummer;
  Zet vorigeZet;
  Stuk geslagenStuk;
  CounterZetWaarden* counterZetWaarden;
  Bitboard schaakGevers;
  Bitboard xRay[KLEUR_N];
  Bitboard schaakVelden[STUKTYPE_N];
  Zet* pv;
  Zet killers[2];
  Waarde stellingWaarde;
  Zet uitgeslotenZet;
  EvalWaarde evalPositioneel;
  uint8_t evalFactor, lmr_reductie;
  bool doeGeenVroegePruning, zetHerhaling;

  // move picker state
  ZetEx *mp_huidigeZet, *mp_eindeLijst, *mp_eindeSlechteSlag;
  PikZetEtappe mp_etappe;
  Zet mp_ttZet, mp_counterZet;
  Diepte mp_diepte;
  Veld mp_slagVeld;
  SeeWaarde mp_threshold;

  // pinners
  Veld penning_door[VELD_N];
};

static_assert(sizeof(StellingInfo) == 256 + 64, "StellingInfo size incorrect");
static_assert(offsetof(struct StellingInfo, sleutel) == 48, "offset wrong");


/// Position class stores information regarding the board representation as
/// pieces, side to move, hash keys, castling info, etc. Important methods are
/// speel_zet() and neem_zet_terug(), used by the search to update node info when
/// traversing the search tree.

class Stelling {

public:
  static void init();
  static void init_hash_move50(int FiftyMoveDistance);

  Stelling() = default;
  Stelling(const Stelling&) = delete;
  Stelling& operator=(const Stelling&) = delete;

  // FEN string input/output
  Stelling& set(const std::string& fenStr, bool isChess960, Thread* th);
  const std::string fen() const;

  // Position representation
  Bitboard stukken() const;
  Bitboard stukken(StukType pt) const;
  Bitboard stukken(StukType pt1, StukType pt2) const;
  Bitboard stukken(Kleur c) const;
  Bitboard stukken(Kleur c, StukType pt) const;
  Bitboard stukken(Kleur c, StukType pt1, StukType pt2) const;
  Bitboard stukken(Kleur c, StukType pt1, StukType pt2, StukType pt3) const;
  Bitboard pieces_exclude(Kleur c, StukType pt) const;
  Stuk stuk_op_veld(Veld s) const;
  Veld enpassant_veld() const;
  bool leeg_veld(Veld s) const;
  template<StukType Pt> int aantal(Kleur c) const;
  int stuk_aantal(Kleur c, StukType Pt) const;
  template<StukType Pt> const Veld* veld_lijst(Kleur c) const;
  template<StukType Pt> Veld veld(Kleur c) const;
  int alle_stukken_aantal() const;

  // Castling
  int rokade_mogelijkheden(Kleur c) const;
  int rokade_mogelijk(RokadeMogelijkheid rokade) const;
  bool rokade_verhinderd(RokadeMogelijkheid rokade) const;
  Veld rokade_toren_veld(Veld koningVeld) const;

  // Checking
  Bitboard schaak_gevers() const;
  Bitboard aftrek_schaak_mogelijk() const;
  Bitboard gepende_stukken() const;
  void bereken_schaak_penningen() const;
  template<Kleur c> void bereken_penningen() const;

  // Attacks to/from a given square
  Bitboard aanvallers_naar(Veld s) const;
  Bitboard aanvallers_naar(Veld s, Bitboard bezet) const;
  Bitboard aanval_van(Stuk pc, Veld s) const;
  template<StukType> Bitboard aanval_van(Veld s) const;
  template<StukType> Bitboard aanval_van(Veld s, Kleur c) const;

  // Properties of moves
  bool legale_zet(Zet zet) const;
  bool geldige_zet(const Zet zet) const;
  bool is_slagzet(Zet zet) const;
  bool slag_of_promotie(Zet zet) const;
  bool geeft_schaak(Zet zet) const;
  bool vooruitgeschoven_pion(Zet zet) const;
  bool vrijpion_opmars(Zet zet, Rij r) const;
  Stuk bewogen_stuk(Zet zet) const;
  Stuk geslagen_stuk() const;
  bool materiaal_of_rokade_gewijzigd() const;

  // Piece specific
  bool is_vrijpion(Kleur c, Veld s) const;
  bool ongelijke_lopers() const;

  // Doing and undoing moves
  void speel_zet(Zet zet, bool geeftSchaak);
  void speel_zet(Zet zet);
  void neem_zet_terug(Zet zet);
  void speel_null_zet();
  void neem_null_terug();

  // Static exchange evaluation
  const SeeWaarde* see_waarden() const;
  SeeWaarde see(Zet zet) const;
  bool see_test(Zet zet, SeeWaarde v) const;

  // Accessing hash keys
  Sleutel64 sleutel() const;
  Sleutel64 sleutel_na_zet(Zet zet) const;
  Sleutel64 uitzondering_sleutel(Zet zet) const;
  Sleutel64 materiaal_sleutel() const;
  Sleutel64 loperkleur_sleutel() const;
  Sleutel64 pion_sleutel() const;
  Sleutel64 remise50_sleutel() const;

  // Other properties of the position
  Kleur aan_zet() const;
  PartijFase partij_fase() const;
  int partij_ply() const;
  void verhoog_partij_ply();
  void verhoog_tb_hits();
  bool is_chess960() const;
  Thread* mijn_thread() const;
  ThreadInfo* ti() const;
  NumaInfo* ni() const;
  uint64_t bezochte_knopen() const;
  uint64_t tb_hits() const;
  int vijftig_zetten_teller() const;
  Score psq_score() const;
  MateriaalWaarde niet_pion_materiaal(Kleur c) const;
  int contempt_aantal(Kleur c) const;
  StellingInfo* info() const { return st; }
  void kopieer_stelling(const Stelling* pos, Thread* th, StellingInfo* copyState);

  // Position consistency check, for debugging
  bool stelling_is_ok(int* failedStep = nullptr) const;
  void draaiBord();
  std::string valideer_stelling() const;

private:
  // Initialization helpers (used while setting up a position)
  void set_rokade_mogelijkheid(Kleur c, Veld rfrom);
  void set_stelling_info(StellingInfo* si) const;
  void bereken_loper_kleur_sleutel() const;

  // Other helpers
  void zet_stuk(Kleur c, Stuk pc, Veld s);
  void verwijder_stuk(Kleur c, Stuk pc, Veld s);
  void verplaats_stuk(Kleur c, Stuk pc, Veld from, Veld to);
  template<bool Do>
  void speel_rokade(Kleur us, Veld from, Veld to, Veld& rfrom, Veld& rto);
  bool is_remise() const;

  // Data members
  StellingInfo* st;
  Thread* mijnThread;
  ThreadInfo* threadInfo;
  NumaInfo* numaInfo;
  Stuk bord[VELD_N];
  Bitboard stukBB[STUK_N];
  Bitboard kleurBB[KLEUR_N];
  int stukAantal[STUK_N];
  Veld stukLijst[STUK_N][16];
  int index[VELD_N];
  int rokadeMask[VELD_N];
  Veld rokadeTorenVeld[VELD_N];
  Bitboard rokadePad[ROKADE_MOGELIJK_N];
  uint64_t knopen, tbHits;
  int partijPly;
  Kleur aanZet;
  bool chess960;
  char filler[48];
};

static_assert(sizeof(Stelling) == 21 * 64, "Stelling foute grootte");

extern std::ostream& operator<<(std::ostream& os, const Stelling& pos);

inline Kleur Stelling::aan_zet() const {
  return aanZet;
}

inline bool Stelling::leeg_veld(Veld s) const {
  return bord[s] == GEEN_STUK;
}

inline Stuk Stelling::stuk_op_veld(Veld s) const {
  return bord[s];
}

inline Stuk Stelling::bewogen_stuk(Zet zet) const {
  return bord[van_veld(zet)];
}

inline Bitboard Stelling::stukken() const {
  return stukBB[ALLE_STUKKEN];
}

inline Bitboard Stelling::stukken(StukType pt) const {
  return stukBB[maak_stuk(WIT, pt)] | stukBB[maak_stuk(ZWART, pt)];
}

inline Bitboard Stelling::stukken(StukType pt1, StukType pt2) const {
  return stukken(pt1) | stukken(pt2);
}

inline Bitboard Stelling::stukken(Kleur c) const {
  return kleurBB[c];
}

inline Bitboard Stelling::stukken(Kleur c, StukType pt) const {
  return stukBB[maak_stuk(c,pt)];
}

inline Bitboard Stelling::stukken(Kleur c, StukType pt1, StukType pt2) const {
  return stukken(c, pt1) | stukken(c, pt2);
}

inline Bitboard Stelling::stukken(Kleur c, StukType pt1, StukType pt2, StukType pt3) const {
	return stukken(c, pt1) | stukken(c, pt2) | stukken(c, pt3);
}

inline Bitboard Stelling::pieces_exclude(Kleur c, StukType pt) const {
	return kleurBB[c] ^ stukBB[maak_stuk(c, pt)];
}

template<StukType Pt> inline int Stelling::aantal(Kleur c) const {
  return stukAantal[maak_stuk(c, Pt)];
}

inline int Stelling::stuk_aantal(Kleur c, StukType Pt) const {
	return stukAantal[maak_stuk(c, Pt)];
}

inline int Stelling::alle_stukken_aantal() const {
  return popcount(stukken());
}

template<StukType Pt> inline const Veld* Stelling::veld_lijst(Kleur c) const {
  return stukLijst[maak_stuk(c, Pt)];
}

template<StukType Pt> inline Veld Stelling::veld(Kleur c) const {
  assert(stukAantal[maak_stuk(c, Pt)] == 1);
  return stukLijst[maak_stuk(c, Pt)][0];
}

inline Veld Stelling::enpassant_veld() const {
  return st->enpassantVeld;
}

inline int Stelling::rokade_mogelijk(RokadeMogelijkheid rokade) const {
  return st->rokadeMogelijkheden & rokade;
}

inline int Stelling::rokade_mogelijkheden(Kleur c) const {
  return st->rokadeMogelijkheden & ((WIT_KORT | WIT_LANG) << (2 * c));
}

inline bool Stelling::rokade_verhinderd(RokadeMogelijkheid rokade) const {
  return stukken() & rokadePad[rokade];
}

inline Veld Stelling::rokade_toren_veld(Veld koningVeld) const {
  return rokadeTorenVeld[koningVeld];
}

template<StukType Pt>
inline Bitboard Stelling::aanval_van(Veld s) const {
  return  Pt == LOPER || Pt == TOREN ? aanval_bb<Pt>(s, stukken())
        : Pt == DAME  ? aanval_van<TOREN>(s) | aanval_van<LOPER>(s)
        : KorteAanval[Pt][s];
}

template<>
inline Bitboard Stelling::aanval_van<PION>(Veld veld, Kleur c) const {
  return KorteAanval[maak_stuk(c, PION)][veld];
}

inline Bitboard Stelling::aanval_van(Stuk pc, Veld veld) const {
  return aanval_bb(pc, veld, stukken());
}

inline Bitboard Stelling::aanvallers_naar(Veld veld) const {
  return aanvallers_naar(veld, stukken());
}

inline Bitboard Stelling::schaak_gevers() const {
  return st->schaakGevers;
}

inline Bitboard Stelling::aftrek_schaak_mogelijk() const {
  return st->xRay[~aanZet] & stukken(aanZet);
}

inline Bitboard Stelling::gepende_stukken() const {
  return st->xRay[aanZet] & stukken(aanZet);
}

inline bool Stelling::is_vrijpion(Kleur c, Veld veld) const {
  return !(stukken(~c, PION) & vrijpion_mask(c, veld));
}

inline bool Stelling::vooruitgeschoven_pion(Zet zet) const {
  return   stuk_type(bewogen_stuk(zet)) == PION
        && relatieve_rij(aanZet, naar_veld(zet)) >= RIJ_6;
}

inline bool Stelling::vrijpion_opmars(Zet zet, Rij r) const {
	Stuk pc = bewogen_stuk(zet);
	return stuk_type(pc) == PION
		&& relatieve_rij(aanZet, naar_veld(zet)) >= r
		&& is_vrijpion(stuk_kleur(pc), naar_veld(zet));
}

inline Sleutel64 Stelling::sleutel() const {
  return st->sleutel;
}

inline Sleutel64 Stelling::pion_sleutel() const {
  return st->pionSleutel;
}

inline Sleutel64 Stelling::materiaal_sleutel() const {
  return st->materiaalSleutel;
}

inline Sleutel64 Stelling::loperkleur_sleutel() const {
  return st->loperKleurSleutel;
}

inline Score Stelling::psq_score() const {
  return st->psq;
}

inline MateriaalWaarde Stelling::niet_pion_materiaal(Kleur c) const {
  return st->nietPionMateriaal[c];
}

inline int Stelling::partij_ply() const {
  return partijPly;
}

inline void Stelling::verhoog_partij_ply() {
  ++partijPly;
}

inline void Stelling::verhoog_tb_hits() {
  ++tbHits;
}

inline int Stelling::vijftig_zetten_teller() const {
  return st->remise50Zetten;
}

inline int Stelling::contempt_aantal(Kleur c) const {
	return st->aantalContempt[c];
}

inline uint64_t Stelling::bezochte_knopen() const {
  return knopen;
}

inline uint64_t Stelling::tb_hits() const {
  return tbHits;
}

inline bool Stelling::ongelijke_lopers() const {
  return   stukAantal[W_LOPER] == 1
        && stukAantal[Z_LOPER] == 1
        && verschillende_kleur(veld<LOPER>(WIT), veld<LOPER>(ZWART));
}

inline bool Stelling::is_chess960() const {
  return chess960;
}

inline bool Stelling::slag_of_promotie(Zet zet) const {

  assert(is_ok(zet));
  return zet_type(zet) != NORMAAL ? zet_type(zet) != ROKADE : !leeg_veld(naar_veld(zet));
}

inline bool Stelling::is_slagzet(Zet zet) const {

  assert(is_ok(zet));
  return (!leeg_veld(naar_veld(zet)) && zet_type(zet) != ROKADE) || zet_type(zet) == ENPASSANT;
}

inline Stuk Stelling::geslagen_stuk() const {
  return st->geslagenStuk;
}

inline bool Stelling::materiaal_of_rokade_gewijzigd() const {
	return st->materiaalSleutel != (st - 1)->materiaalSleutel
		|| st->rokadeMogelijkheden != (st - 1)->rokadeMogelijkheden;
}

inline Thread* Stelling::mijn_thread() const {
	return mijnThread;
}

inline ThreadInfo* Stelling::ti() const {
  return threadInfo;
}

inline NumaInfo* Stelling::ni() const {
	return numaInfo;
}

inline void Stelling::zet_stuk(Kleur c, Stuk pc, Veld s) {

  bord[s] = pc;
  stukBB[pc] |= s;
  kleurBB[c] |= s;
  index[s] = stukAantal[pc]++;
  stukLijst[pc][index[s]] = s;
}

inline void Stelling::verwijder_stuk(Kleur c, Stuk pc, Veld s) {

  // WARNING: This is not a reversible operation. If we remove a piece in
  // speel_zet() and then replace it in neem_zet_terug() we will put it at the end of
  // the list and not in its original place, it means index[] and pieceList[]
  // are not guaranteed to be invariant to a speel_zet() + neem_zet_terug() sequence.
  stukBB[pc] ^= s;
  kleurBB[c] ^= s;
  /* board[s] = GEEN_STUK;  Not needed, overwritten by the capturing one */
  Veld lastSquare = stukLijst[pc][--stukAantal[pc]];
  index[lastSquare] = index[s];
  stukLijst[pc][index[lastSquare]] = lastSquare;
  stukLijst[pc][stukAantal[pc]] = SQ_NONE;
}

inline void Stelling::verplaats_stuk(Kleur c, Stuk pc, Veld from, Veld to) {

  // index[from] is not updated and becomes stale. This works as long as index[]
  // is accessed just by known occupied squares.
  Bitboard from_to_bb = bbVeld[from] ^ bbVeld[to];
  stukBB[pc] ^= from_to_bb;
  kleurBB[c] ^= from_to_bb;
  bord[from] = GEEN_STUK;
  bord[to] = pc;
  index[to] = index[from];
  stukLijst[pc][index[to]] = to;
}

#endif // #ifndef POSITION_H_INCLUDED
