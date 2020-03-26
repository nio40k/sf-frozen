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

#include "bitboard.h"
#include "misc.h"

#ifndef USE_POPCNT
uint8_t PopCnt16[1 << 16];
#endif

int VeldAfstand[VELD_N][VELD_N];

Bitboard TorenMask[VELD_N];
Bitboard LoperMask[VELD_N];
Bitboard* LoperAanvalTabel[64];
Bitboard* TorenAanvalTabel[64];

const Bitboard LoperMagics[64] = {
	0x007bfeffbfeffbffull, 0x003effbfeffbfe08ull, 0x0000401020200000ull,
	0x0000200810000000ull, 0x0000110080000000ull, 0x0000080100800000ull,
	0x0007efe0bfff8000ull, 0x00000fb0203fff80ull, 0x00007dff7fdff7fdull,
	0x0000011fdff7efffull, 0x0000004010202000ull, 0x0000002008100000ull,
	0x0000001100800000ull, 0x0000000801008000ull, 0x000007efe0bfff80ull,
	0x000000080f9fffc0ull, 0x0000400080808080ull, 0x0000200040404040ull,
	0x0000400080808080ull, 0x0000200200801000ull, 0x0000240080840000ull,
	0x0000080080840080ull, 0x0000040010410040ull, 0x0000020008208020ull,
	0x0000804000810100ull, 0x0000402000408080ull, 0x0000804000810100ull,
	0x0000404004010200ull, 0x0000404004010040ull, 0x0000101000804400ull,
	0x0000080800104100ull, 0x0000040400082080ull, 0x0000410040008200ull,
	0x0000208020004100ull, 0x0000110080040008ull, 0x0000020080080080ull,
	0x0000404040040100ull, 0x0000202040008040ull, 0x0000101010002080ull,
	0x0000080808001040ull, 0x0000208200400080ull, 0x0000104100200040ull,
	0x0000208200400080ull, 0x0000008840200040ull, 0x0000020040100100ull,
	0x007fff80c0280050ull, 0x0000202020200040ull, 0x0000101010100020ull,
	0x0007ffdfc17f8000ull, 0x0003ffefe0bfc000ull, 0x0000000820806000ull,
	0x00000003ff004000ull, 0x0000000100202000ull, 0x0000004040802000ull,
	0x007ffeffbfeff820ull, 0x003fff7fdff7fc10ull, 0x0003ffdfdfc27f80ull,
	0x000003ffefe0bfc0ull, 0x0000000008208060ull, 0x0000000003ff0040ull,
	0x0000000001002020ull, 0x0000000040408020ull, 0x00007ffeffbfeff9ull,
	0x007ffdff7fdff7fdull
};

const Bitboard TorenMagics[64] = {
	0x00a801f7fbfeffffull, 0x00180012000bffffull, 0x0040080010004004ull,
	0x0040040008004002ull, 0x0040020004004001ull, 0x0020008020010202ull,
	0x0040004000800100ull, 0x0810020990202010ull, 0x000028020a13fffeull,
	0x003fec008104ffffull, 0x00001800043fffe8ull, 0x00001800217fffe8ull,
	0x0000200100020020ull, 0x0000200080010020ull, 0x0000300043ffff40ull,
	0x000038010843fffdull, 0x00d00018010bfff8ull, 0x0009000c000efffcull,
	0x0004000801020008ull, 0x0002002004002002ull, 0x0001002002002001ull,
	0x0001001000801040ull, 0x0000004040008001ull, 0x0000802000200040ull,
	0x0040200010080010ull, 0x0000080010040010ull, 0x0004010008020008ull,
	0x0000020020040020ull, 0x0000010020020020ull, 0x0000008020010020ull,
	0x0000008020200040ull, 0x0000200020004081ull, 0x0040001000200020ull,
	0x0000080400100010ull, 0x0004010200080008ull, 0x0000200200200400ull,
	0x0000200100200200ull, 0x0000200080200100ull, 0x0000008000404001ull,
	0x0000802000200040ull, 0x00ffffb50c001800ull, 0x007fff98ff7fec00ull,
	0x003ffff919400800ull, 0x001ffff01fc03000ull, 0x0000010002002020ull,
	0x0000008001002020ull, 0x0003fff673ffa802ull, 0x0001fffe6fff9001ull,
	0x00ffffd800140028ull, 0x007fffe87ff7ffecull, 0x003fffd800408028ull,
	0x001ffff111018010ull, 0x000ffff810280028ull, 0x0007fffeb7ff7fd8ull,
	0x0003fffc0c480048ull, 0x0001ffffa2280028ull, 0x00ffffe4ffdfa3baull,
	0x007ffb7fbfdfeff6ull, 0x003fffbfdfeff7faull, 0x001fffeff7fbfc22ull,
	0x000ffffbf7fc2ffeull, 0x0007fffdfa03ffffull, 0x0003ffdeff7fbdecull,
	0x0001ffff99ffab2full
};


Bitboard bbVeld[VELD_N];
Bitboard bbLijn[LIJN_N];
Bitboard bbRij[RIJ_N];
Bitboard bbAangrenzendeLijnen[LIJN_N];
Bitboard bbRijenVoorwaarts[KLEUR_N][RIJ_N];
Bitboard KorteAanval[STUK_N][VELD_N];
Bitboard bbTussen[VELD_N][VELD_N];
Bitboard bbVerbinding[VELD_N][VELD_N];
Bitboard DistanceRingBB[VELD_N][8];
Bitboard bbVoorwaarts[KLEUR_N][VELD_N];
Bitboard VrijpionMask[KLEUR_N][VELD_N];
Bitboard PionAanvalBereik[KLEUR_N][VELD_N];
Bitboard LegeAanval[STUKTYPE_N][VELD_N];

namespace {

  Bitboard MagicAanvalT[102400];
  Bitboard MagicAanvalL[5248];

  static const int Local_BishopMagicIndex[64] = {
	  16530,  9162,  9674,
	  18532, 19172, 17700,
	  5730, 19661, 17065,
	  12921, 15683, 17764,
	  19684, 18724,  4108,
	  12936, 15747,  4066,
	  14359, 36039, 20457,
	  43291,  5606,  9497,
	  15715, 13388,  5986,
	  11814, 92656,  9529,
	  18118,  5826,  4620,
	  12958, 55229,  9892,
	  33767, 20023,  6515,
	  6483, 19622,  6274,
	  18404, 14226, 17990,
	  18920, 13862, 19590,
	  5884, 12946,  5570,
	  18740,  6242, 12326,
	  4156, 12876, 17047,
	  17780,  2494, 17716,
	  17067,  9465, 16196,
	  6166
  };

  static const int Local_RookMagicIndex[64] = {
	  85487, 43101,     0,
	  49085, 93168, 78956,
	  60703, 64799, 30640,
	  9256, 28647, 10404,
	  63775, 14500, 52819,
	  2048, 52037, 16435,
	  29104, 83439, 86842,
	  27623, 26599, 89583,
	  7042, 84463, 82415,
	  95216, 35015, 10790,
	  53279, 70684, 38640,
	  32743, 68894, 62751,
	  41670, 25575,  3042,
	  36591, 69918,  9092,
	  17401, 40688, 96240,
	  91632, 32495, 51133,
	  78319, 12595,  5152,
	  32110, 13894,  2546,
	  41052, 77676, 73580,
	  44947, 73565, 17682,
	  56607, 56135, 44989,
	  21479
  };

  void init_magic_sliders();

  // popcount16() counts the non-zero bits using SWAR-Popcount algorithm

  unsigned popcount16(unsigned u) {
    u -= (u >> 1) & 0x5555U;
    u = ((u >> 2) & 0x3333U) + (u & 0x3333U);
    u = ((u >> 4) + u) & 0x0F0FU;
    return (u * 0x0101U) >> 8;
  }
}

#ifdef NO_BSF

/// Software fall-back of lsb() and msb() for CPU lacking hardware support

// De Bruijn sequences. See chessprogramming.wikispaces.com/BitScan
const uint64_t DeBruijn64 = 0x3F79D71B4CB0A89ULL;
const uint32_t DeBruijn32 = 0x783A9B23;

int MSBTable[256];       // To implement software msb()
Veld BSFTable[VELD_N];   // To implement software bitscan

// bsf_index() returns the index into BSFTable[] to look up the bitscan. Uses
// Matt Taylor's folding for 32 bit case, extended to 64 bit by Kim Walisch.

unsigned bsf_index(Bitboard b) {
	b ^= b - 1;
	return Is64Bit ? (b * DeBruijn64) >> 58
		: ((unsigned(b) ^ unsigned(b >> 32)) * DeBruijn32) >> 26;
}

Veld lsb(Bitboard b) {
  assert(b);
  return BSFTable[bsf_index(b)];
}

Veld msb(Bitboard b) {

  assert(b);
  unsigned b32;
  int result = 0;

  if (b > 0xFFFFFFFF)
  {
      b >>= 32;
      result = 32;
  }

  b32 = unsigned(b);

  if (b32 > 0xFFFF)
  {
      b32 >>= 16;
      result += 16;
  }

  if (b32 > 0xFF)
  {
      b32 >>= 8;
      result += 8;
  }

  return Veld(result + MSBTable[b32]);
}

#endif // ifdef NO_BSF


/// Bitboards::pretty() returns an ASCII representation of a bitboard suitable
/// to be printed to standard output. Useful for debugging.
/*
const std::string Bitboards::pretty(Bitboard b) {

  std::string s = "+---+---+---+---+---+---+---+---+\n";

  for (Rij r = RIJ_8; r >= RIJ_1; --r)
  {
      for (Lijn f = LIJN_A; f <= LIJN_H; ++f)
          s += b & maak_veld(f, r) ? "| X " : "|   ";

      s += "|\n+---+---+---+---+---+---+---+---+\n";
  }

  return s;
}
*/


/// Bitboards::init() initializes various bitboard tables. It is called at
/// startup and relies on global objects to be already zero-initialized.

void Bitboards::init() {

#ifndef USE_POPCNT
	for (unsigned i = 0; i < (1 << 16); ++i)
      PopCnt16[i] = (uint8_t) popcount16(i);
#endif

  for (Veld s = SQ_A1; s <= SQ_H8; ++s)
      bbVeld[s] = 1ULL << s;

#ifdef NO_BSF
  for (Veld s = SQ_A1; s <= SQ_H8; ++s)
	  BSFTable[bsf_index(bbVeld[s])] = s;
  for (Bitboard b = 2; b < 256; ++b)
      MSBTable[b] = MSBTable[b - 1] + !meer_dan_een(b);
#endif // ifdef NO_BSF

  for (Lijn f = LIJN_A; f <= LIJN_H; ++f)
      bbLijn[f] = f > LIJN_A ? bbLijn[f - 1] << 1 : FileABB;

  for (Rij r = RIJ_1; r <= RIJ_8; ++r)
      bbRij[r] = r > RIJ_1 ? bbRij[r - 1] << 8 : Rank1BB;

  for (Lijn f = LIJN_A; f <= LIJN_H; ++f)
      bbAangrenzendeLijnen[f] = (f > LIJN_A ? bbLijn[f - 1] : 0) | (f < LIJN_H ? bbLijn[f + 1] : 0);

  for (Rij r = RIJ_1; r < RIJ_8; ++r)
      bbRijenVoorwaarts[WIT][r] = ~(bbRijenVoorwaarts[ZWART][r + 1] = bbRijenVoorwaarts[ZWART][r] | bbRij[r]);

  for (Kleur c = WIT; c <= ZWART; ++c)
      for (Veld s = SQ_A1; s <= SQ_H8; ++s)
      {
          bbVoorwaarts[c][s]      = bbRijenVoorwaarts[c][rij(s)] & bbLijn[lijn(s)];
          PionAanvalBereik[c][s] = bbRijenVoorwaarts[c][rij(s)] & bbAangrenzendeLijnen[lijn(s)];
          VrijpionMask[c][s] = bbVoorwaarts[c][s] | PionAanvalBereik[c][s];
      }

  for (Veld s1 = SQ_A1; s1 <= SQ_H8; ++s1)
      for (Veld s2 = SQ_A1; s2 <= SQ_H8; ++s2)
          if (s1 != s2)
          {
              VeldAfstand[s1][s2] = std::max(lijn_afstand(s1, s2), rij_afstand(s1, s2));
              DistanceRingBB[s1][VeldAfstand[s1][s2] - 1] |= s2;
          }

  int steps[STUKTYPE_N][9] = { {}, { 9, 7, -7, -9, 8, 1, -1, -8 }, { 7, 9 }, { 17, 15, 10, 6, -6, -10, -15, -17 },
                     {}, {}, {} };

  for (Kleur c = WIT; c <= ZWART; ++c)
      for (StukType pt = KONING; pt <= PAARD; ++pt)
          for (Veld s = SQ_A1; s <= SQ_H8; ++s)
              for (int i = 0; steps[pt][i]; ++i)
              {
                  Veld to = s + Veld(c == WIT ? steps[pt][i] : -steps[pt][i]);

                  if (is_ok(to) && afstand(s, to) < 3)
                      KorteAanval[maak_stuk(c, pt)][s] |= to;
              }

  init_magic_sliders();

  for (Veld s1 = SQ_A1; s1 <= SQ_H8; ++s1)
  {
      LegeAanval[DAME][s1]  = LegeAanval[LOPER][s1] = aanval_bb<LOPER>(s1, 0);
      LegeAanval[DAME][s1] |= LegeAanval[TOREN][s1] = aanval_bb<TOREN>(s1, 0);

      for (Stuk pc = W_LOPER; pc <= W_TOREN; ++pc)
          for (Veld s2 = SQ_A1; s2 <= SQ_H8; ++s2)
          {
              if (!(LegeAanval[pc][s1] & s2))
                  continue;

              bbVerbinding[s1][s2] = (aanval_bb(pc, s1, 0) & aanval_bb(pc, s2, 0)) | s1 | s2;
              bbTussen[s1][s2] = aanval_bb(pc, s1, bbVeld[s2]) & aanval_bb(pc, s2, bbVeld[s1]);
          }
  }
}


namespace {

	Bitboard sliding_attacks(int sq, Bitboard block, const int deltas[4][2],
		int fmin, int fmax, int rmin, int rmax)
	{
		Bitboard result = 0;
		int rk = sq / 8, fl = sq % 8, r, f;

		for (int richting = 0; richting < 4; richting++)
		{
			int dx = deltas[richting][0];
			int dy = deltas[richting][1];
			for (f = fl + dx, r = rk + dy;
				(dx == 0 || (f >= fmin && f <= fmax)) && (dy == 0 || (r >= rmin && r <= rmax));
				f += dx, r += dy)
			{
				result |= bbVeld[f + r * 8];
				if (block & bbVeld[f + r * 8])
					break;
			}
		}
		return result;
	}

	void init_magic_bb_pext(Bitboard *aanval, Bitboard* veldIndex[], Bitboard *mask, const int deltas[4][2])
	{
		for (int sq = 0; sq < 64; sq++)
		{
			veldIndex[sq] = aanval;
			mask[sq] = sliding_attacks(sq, 0, deltas, 1, 6, 1, 6);

			Bitboard b = 0;
			do  // loop over complete set of mask[sq], see http://chessprogramming.wikispaces.com/Traversing+Subsets+of+a+Set
			{
				veldIndex[sq][pext(b, mask[sq])] = sliding_attacks(sq, b, deltas, 0, 7, 0, 7);
				b = (b - mask[sq]) & mask[sq];
				aanval++;
			} while (b);
		}
	}

	void init_magic_bb(Bitboard *aanval, const int attackIndex[], Bitboard* veldIndex[], Bitboard *mask,
		int shift, const Bitboard mult[], const int deltas[4][2])
	{
		for (int sq = 0; sq < 64; sq++)
		{
			int index = attackIndex[sq];
			veldIndex[sq] = aanval + index;
			mask[sq] = sliding_attacks(sq, 0, deltas, 1, 6, 1, 6);

			Bitboard b = 0;
			do  // loop over complete set of mask[sq], see http://chessprogramming.wikispaces.com/Traversing+Subsets+of+a+Set
			{
				int offset = (unsigned int)((b * mult[sq]) >> shift);
				veldIndex[sq][offset] = sliding_attacks(sq, b, deltas, 0, 7, 0, 7);
				b = (b - mask[sq]) & mask[sq];
			} while (b);
		}
	}


	void init_magic_sliders()
	{
		const int toren_deltas[4][2] = { { 0, 1 },{ 0, -1 },{ 1, 0 },{ -1, 0 } };
		const int loper_deltas[4][2] = { { 1, 1 },{ -1, 1 },{ 1, -1 },{ -1, -1 } };

		if (HasPext)
		{
			init_magic_bb_pext(MagicAanvalT, TorenAanvalTabel, TorenMask, toren_deltas);
			init_magic_bb_pext(MagicAanvalL, LoperAanvalTabel, LoperMask, loper_deltas);
		}
		else {
			init_magic_bb(MagicAanvalT, Local_RookMagicIndex, TorenAanvalTabel, TorenMask, 52, TorenMagics, toren_deltas);
			init_magic_bb(MagicAanvalT, Local_BishopMagicIndex, LoperAanvalTabel,
				LoperMask, 55, LoperMagics, loper_deltas);
		}
	}

}
