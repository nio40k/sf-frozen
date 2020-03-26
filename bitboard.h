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

#ifndef BITBOARD_H_INCLUDED
#define BITBOARD_H_INCLUDED

#include <string>

#include "types.h"

namespace Bitbases {

void init();
bool probe(Veld wksq, Veld wpsq, Veld bksq, Kleur us);

}

namespace Bitboards {

void init();
//const std::string pretty(Bitboard b);

}

const Bitboard DonkereVelden = 0xAA55AA55AA55AA55ULL;

const Bitboard FileABB = 0x0101010101010101ULL;
const Bitboard FileBBB = FileABB << 1;
const Bitboard FileCBB = FileABB << 2;
const Bitboard FileDBB = FileABB << 3;
const Bitboard FileEBB = FileABB << 4;
const Bitboard FileFBB = FileABB << 5;
const Bitboard FileGBB = FileABB << 6;
const Bitboard FileHBB = FileABB << 7;

const Bitboard Rank1BB = 0xFF;
const Bitboard Rank2BB = Rank1BB << (8 * 1);
const Bitboard Rank3BB = Rank1BB << (8 * 2);
const Bitboard Rank4BB = Rank1BB << (8 * 3);
const Bitboard Rank5BB = Rank1BB << (8 * 4);
const Bitboard Rank6BB = Rank1BB << (8 * 5);
const Bitboard Rank7BB = Rank1BB << (8 * 6);
const Bitboard Rank8BB = Rank1BB << (8 * 7);

extern int VeldAfstand[VELD_N][VELD_N];

extern Bitboard bbVeld[VELD_N];
extern Bitboard bbLijn[LIJN_N];
extern Bitboard bbRij[RIJ_N];
extern Bitboard bbAangrenzendeLijnen[LIJN_N];
extern Bitboard bbRijenVoorwaarts[KLEUR_N][RIJ_N];
extern Bitboard KorteAanval[STUK_N][VELD_N];
extern Bitboard bbTussen[VELD_N][VELD_N];
extern Bitboard bbVerbinding[VELD_N][VELD_N];
extern Bitboard DistanceRingBB[VELD_N][8];
extern Bitboard bbVoorwaarts[KLEUR_N][VELD_N];
extern Bitboard VrijpionMask[KLEUR_N][VELD_N];
extern Bitboard PionAanvalBereik[KLEUR_N][VELD_N];
extern Bitboard LegeAanval[STUKTYPE_N][VELD_N];


/// Overloads of bitwise operators between a Bitboard and a Square for testing
/// whether a given bit is set in a bitboard, and for setting and clearing bits.


#define USE_BB_SHIFT
#ifdef USE_BB_SHIFT
  #define bb(s) (1ULL << s)
#else
  #define bb(s) bbVeld[s]
#endif

inline Bitboard operator&(Bitboard b, Veld s) {
  return b & bb(s);
}

inline Bitboard operator|(Bitboard b, Veld s) {
  return b | bb(s);
}

inline Bitboard operator^(Bitboard b, Veld s) {
  return b ^ bb(s);
}

inline Bitboard& operator|=(Bitboard& b, Veld s) {
  return b |= bb(s);
}

inline Bitboard& operator^=(Bitboard& b, Veld s) {
  return b ^= bb(s);
}

inline bool meer_dan_een(Bitboard b) {
  return b & (b - 1);
}

#undef bb

/// bb_rij() and bb_lijn() return a bitboard representing all the squares on
/// the given file or rank.

//inline Bitboard bb_rij(Rij r) {
//  return bbRij[r];
//}

inline Bitboard bb_rij(Veld s) {
  return bbRij[rij(s)];
}

//inline Bitboard bb_lijn(Lijn f) {
//  return bbLijn[f];
//}

inline Bitboard bb_lijn(Veld s) {
  return bbLijn[lijn(s)];
}


/// shift_bb() moves a bitboard one step along direction Delta. Mainly for pawns

template<Veld Delta>
inline Bitboard shift_bb(Bitboard b) {
  return  Delta == BOVEN ? b << 8 
	    : Delta == ONDER ? b >> 8
        : Delta == RECHTSBOVEN ? (b & ~FileHBB) << 9 
	    : Delta == RECHTSONDER ? (b & ~FileHBB) >> 7
        : Delta == LINKSBOVEN ? (b & ~FileABB) << 7 
	    : Delta == LINKSONDER ? (b & ~FileABB) >> 9
        : 0;
}


/// bb_aangrenzende_lijnen() returns a bitboard representing all the squares on the
/// adjacent files of the given one.

inline Bitboard bb_aangrenzende_lijnen(Lijn f) {
  return bbAangrenzendeLijnen[f];
}


/// bb_tussen() returns a bitboard representing all the squares between the two
/// given ones. For instance, bb_tussen(SQ_C4, SQ_F7) returns a bitboard with
/// the bits for square d5 and e6 set. If s1 and s2 are not on the same rank, file
/// or diagonal, 0 is returned.

inline Bitboard bb_tussen(Veld s1, Veld s2) {
  return bbTussen[s1][s2];
}

inline Bitboard bb_verbinding(Veld s1, Veld s2) {
	return bbVerbinding[s1][s2];
}


/// bb_rijen_voorwaarts() returns a bitboard representing all the squares on all the ranks
/// in front of the given one, from the point of view of the given color. For
/// instance, bb_rijen_voorwaarts(BLACK, RIJ_3) will return the squares on ranks 1 and 2.

inline Bitboard bb_rijen_voorwaarts(Kleur c, Rij r) {
  return bbRijenVoorwaarts[c][r];
}

inline Bitboard bb_rijen_voorwaarts(Kleur c, Veld s) {
	return bbRijenVoorwaarts[c][rij(s)];
}


/// bb_voorwaarts() returns a bitboard representing all the squares along the line
/// in front of the given one, from the point of view of the given color:
///        bbVoorwaarts[c][s] = bb_rijen_voorwaarts(c, s) & bb_lijn(s)

inline Bitboard bb_voorwaarts(Kleur c, Veld s) {
  return bbVoorwaarts[c][s];
}


/// pion_aanval_bereik() returns a bitboard representing all the squares that can be
/// attacked by a pawn of the given color when it moves along its file, starting
/// from the given square:
///       PionAanvalBereik[c][s] = bb_rijen_voorwaarts(c, s) & bb_aangrenzende_lijnen(s);

inline Bitboard pion_aanval_bereik(Kleur c, Veld s) {
  return PionAanvalBereik[c][s];
}


/// vrijpion_mask() returns a bitboard mask which can be used to test if a
/// pawn of the given color and on the given square is a passed pawn:
///       VrijpionMask[c][s] = pion_aanval_bereik(c, s) | bb_voorwaarts(c, s)

inline Bitboard vrijpion_mask(Kleur c, Veld s) {
  return VrijpionMask[c][s];
}


/// aligned() returns true if the squares s1, s2 and s3 are aligned either on a
/// straight or on a diagonal line.

inline bool aligned(Veld s1, Veld s2, Veld s3) {
  return bbVerbinding[s1][s2] & s3;
}


inline int afstand(Veld x, Veld y) { return VeldAfstand[x][y]; }
inline int lijn_afstand(Veld x, Veld y) { return abs(lijn(x) - lijn(y)); }
inline int rij_afstand(Veld x, Veld y) { return abs(rij(x) - rij(y)); }


extern Bitboard* LoperAanvalTabel[64];
extern Bitboard LoperMask[64];
extern const Bitboard LoperMagics[64];

extern Bitboard* TorenAanvalTabel[64];
extern Bitboard TorenMask[64];
extern const Bitboard TorenMagics[64];

template<StukType Pt>
inline Bitboard aanval_bb(Veld veld, Bitboard bezet) {
	if (Pt == LOPER)
		if (HasPext)
			return LoperAanvalTabel[veld][pext(bezet, LoperMask[veld])];
		else
			return LoperAanvalTabel[veld][((bezet & LoperMask[veld]) * LoperMagics[veld]) >> 55];
	else
		if (HasPext)
			return TorenAanvalTabel[veld][pext(bezet, TorenMask[veld])];
		else
			return TorenAanvalTabel[veld][((bezet & TorenMask[veld]) * TorenMagics[veld]) >> 52];
}


inline Bitboard aanval_bb(Stuk pc, Veld veld, Bitboard bezet) {

  switch (stuk_type(pc))
  {
  case LOPER : return aanval_bb<LOPER>(veld, bezet);
  case TOREN : return aanval_bb<TOREN>(veld, bezet);
  case DAME  : return aanval_bb<LOPER>(veld, bezet) | aanval_bb<TOREN>(veld, bezet);
  default    : return KorteAanval[pc][veld];
  }
}

template<Kleur c>
inline Bitboard pion_aanval(Bitboard bb) {

	if (c == WIT)
		return shift_bb<LINKSBOVEN>(bb) | shift_bb<RECHTSBOVEN>(bb);
	else
		return shift_bb<LINKSONDER>(bb) | shift_bb<RECHTSONDER>(bb);
}

template<Kleur c>
inline Bitboard shift_up(Bitboard bb) {

	if (c == WIT)
		return shift_bb<BOVEN>(bb);
	else
		return shift_bb<ONDER>(bb);
}

template<Kleur c>
inline Bitboard shift_down(Bitboard bb) {

	if (c == WIT)
		return shift_bb<ONDER>(bb);
	else
		return shift_bb<BOVEN>(bb);
}

template<Kleur c>
inline Bitboard shift_upleft(Bitboard bb) {

	if (c == WIT)
		return shift_bb<LINKSBOVEN>(bb);
	else
		return shift_bb<LINKSONDER>(bb);
}

template<Kleur c>
inline Bitboard shift_upright(Bitboard bb) {

	if (c == WIT)
		return shift_bb<RECHTSBOVEN>(bb);
	else
		return shift_bb<RECHTSONDER>(bb);
}


/// popcount() counts the number of non-zero bits in a bitboard

inline int popcount(Bitboard b) {

#ifndef USE_POPCNT

  extern uint8_t PopCnt16[1 << 16];
  union { Bitboard bb; uint16_t u[4]; } v = { b };
  return PopCnt16[v.u[0]] + PopCnt16[v.u[1]] + PopCnt16[v.u[2]] + PopCnt16[v.u[3]];

#elif defined(_MSC_VER) || defined(__INTEL_COMPILER)

  return (int)_mm_popcnt_u64(b);

#else // Assumed gcc or compatible compiler

  return __builtin_popcountll(b);

#endif
}


/// lsb() and msb() return the least/most significant bit in a non-zero bitboard

#if defined(__GNUC__)

inline Veld lsb(Bitboard b) {
  assert(b);
  return Veld(__builtin_ctzll(b));
}

inline Veld msb(Bitboard b) {
  assert(b);
  return Veld(63 - __builtin_clzll(b));
}

#elif defined(_WIN64) && defined(_MSC_VER)

inline Veld lsb(Bitboard b) {
  assert(b);
  unsigned long idx;
  _BitScanForward64(&idx, b);
  return (Veld) idx;
}

inline Veld msb(Bitboard b) {
  assert(b);
  unsigned long idx;
  _BitScanReverse64(&idx, b);
  return (Veld) idx;
}

#else

#define NO_BSF // Fallback on software implementation for other cases

Veld lsb(Bitboard b);
Veld msb(Bitboard b);

#endif


/// pop_lsb() finds and clears the least significant bit in a non-zero bitboard

inline Veld pop_lsb(Bitboard* b) {
  const Veld s = lsb(*b);
  *b &= *b - 1;
  return s;
}


/// frontmost_sq() and backmost_sq() return the square corresponding to the
/// most/least advanced bit relative to the given color.

inline Veld frontmost_sq(Kleur c, Bitboard b) { return c == WIT ? msb(b) : lsb(b); }
inline Veld  backmost_sq(Kleur c, Bitboard b) { return c == WIT ? lsb(b) : msb(b); }

#endif // #ifndef BITBOARD_H_INCLUDED
