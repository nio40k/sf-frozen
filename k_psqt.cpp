/*
  Stockfish, a UCI chess playing engine derived from Glaurung 2.1
  Copyright (C) 2004-2008 Tord Romstad (Glaurung author)
  Copyright (C) 2008-2015 Marco Costalba, Joona Kiiski, Tord Romstad

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

#include "types.h"
#include "uci.h"


namespace PSQT {

	inline Score SCORE_MULDIV(Score x, int mul, int div) {
		return remake_score(mg_waarde(x) * mul / div, eg_waarde(x) * mul / div);
	}

	#define SC(mg, eg) maak_score(mg, eg)

	static const Score PST[DAME + 1][64] = {
		{},
		// PST_K[64]
		{
		SC(-568,-864), SC(-520,-624), SC(-696,-480), SC(-792,-432), SC(-792,-432), SC(-696,-480), SC(-520,-624), SC(-568,-864),
		SC(-504,-624), SC(-456,-384), SC(-640,-240), SC(-744,-192), SC(-744,-192), SC(-640,-240), SC(-456,-384), SC(-504,-624),
		SC(-448,-480), SC(-400,-240), SC(-592, -96), SC(-704, -48), SC(-704, -48), SC(-592, -96), SC(-400,-240), SC(-448,-480),
		SC(-384,-432), SC(-336,-192), SC(-536, -48), SC(-656,   0), SC(-656,   0), SC(-536, -48), SC(-336,-192), SC(-384,-432),
		SC(-312,-480), SC(-264,-240), SC(-472, -96), SC(-600, -48), SC(-600, -48), SC(-472, -96), SC(-264,-240), SC(-312,-480),
		SC(-240,-624), SC(-192,-384), SC(-408,-240), SC(-544,-192), SC(-544,-192), SC(-408,-240), SC(-192,-384), SC(-240,-624),
		SC( -96,-864), SC( -48,-624), SC(-280,-480), SC(-432,-432), SC(-432,-432), SC(-280,-480), SC( -48,-624), SC( -96,-864),
		SC( -48,-1200),SC(   0,-960), SC(-240,-816), SC(-400,-768), SC(-400,-768), SC(-240,-816), SC(   0,-960), SC( -48,-1200)
		},
		{
		SC(   0, 0), SC(  0,  0), SC(  0,  0), SC(  0,  0), SC(  0,  0), SC(  0,  0), SC(  0,  0), SC(   0, 0),
		SC( -80,24), SC( 24, 17), SC( 88, 10), SC(152,  3), SC(152,  3), SC( 88, 10), SC( 24, 17), SC( -80,24),
		SC( -80,24), SC( 24, 17), SC( 88, 10), SC(152,  3), SC(152,  3), SC( 88, 10), SC( 24, 17), SC( -80,24),
		SC( -88,17), SC(  8, 10), SC( 64,  3), SC(120, -3), SC(120, -3), SC( 64,  3), SC(  8, 10), SC( -88,17),
		SC( -96,10), SC( -8,  3), SC( 40, -3), SC( 88,-10), SC( 88,-10), SC( 40, -3), SC( -8,  3), SC( -96,10),
		SC(-104, 3), SC(-24, -3), SC( 16,-10), SC( 56,-17), SC( 56,-17), SC( 16,-10), SC(-24, -3), SC(-104, 3),
		SC(-108, 0), SC(-32, -7), SC(  4,-14), SC( 40,-21), SC( 40,-21), SC(  4,-14), SC(-32, -7), SC(-108, 0),
		SC(   0, 0), SC(  0,  0), SC(  0,  0), SC(  0,  0), SC(  0,  0), SC(  0,  0), SC(  0,  0), SC(   0, 0)
		},
		// PST_P[64]
		{
		SC(-1210,-400), SC(-110, -50), SC( -66, -30), SC( -44,-20), SC( -44,-20), SC( -66, -30), SC(-110, -50), SC(-1210,-400),    // waarde van a8/h8 verminderen (-500,-200)
		SC( -154, -70), SC(- 66, -30), SC(   0,   0), SC(  22, 10), SC(  22, 10), SC(  22,  10), SC(- 66, -30), SC( -154, -70),    // f7 zou (0,0) moeten zijn ipv (22,10)
		SC(  -88, -40), SC(   0,   0), SC(  66,  30), SC( 110, 50), SC( 110, 50), SC(  66,  30), SC(   0,   0), SC(  -88, -40),
		SC(  -88, -40), SC(   0,   0), SC(  66,  30), SC( 110, 50), SC( 110, 50), SC(  66,  30), SC(   0,   0), SC(  -88, -40),
		SC( -110, -50), SC( -22, -10), SC(  44,  20), SC(  66, 30), SC(  66, 30), SC(  44,  20), SC( -22, -10), SC( -110, -50),
		SC( -154, -70), SC( -66, -30), SC( -22, -10), SC(   0,  0), SC(   0,  0), SC( -22, -10), SC( -66, -30), SC( -154, -70),
		SC( -220,-100), SC(-154, -70), SC(-110, -50), SC( -88,-40), SC( -88,-40), SC(-110, -50), SC(-154, -70), SC( -220,-100),
		SC( -330,-150), SC(-264,-120), SC(-220,-100), SC(-198,-90), SC(-198,-90), SC(-220,-100), SC(-264,-120), SC( -330,-150)
		},
		// PST_L[64]
		{
		SC(-28,-10), SC(-35,-20), SC(-35,-20), SC(-28,-10), SC(-28,-10), SC(-35,-20), SC(-35,-20), SC(-28,-10),
		SC(-42,-20), SC( 21, 10), SC( 35, 20), SC( 14, 10), SC( 14, 10), SC( 35, 20), SC( 21, 10), SC(-42,-20),
		SC(-49,-20), SC( 28, 10), SC( 42, 20), SC( 35, 20), SC( 35, 20), SC( 42, 20), SC( 28, 10), SC(-49,-20),
		SC(-49,-20), SC(  0,  0), SC( 28, 10), SC( 63, 30), SC( 63, 30), SC( 28, 10), SC(  0,  0), SC(-49,-20),
		SC(-42,-20), SC(  7,  0), SC( 35, 20), SC( 70, 30), SC( 70, 30), SC( 35, 20), SC(  7,  0), SC(-42,-20),
		SC(-42,-20), SC( 21, 10), SC( 35, 20), SC( 28, 10), SC( 28, 10), SC( 35, 20), SC( 21, 10), SC(-42,-20),
		SC(-35,-20), SC( 14, 10), SC( 14, 10), SC( -7,  0), SC( -7,  0), SC( 14, 10), SC( 14, 10), SC(-35,-20),
		SC(-21,-10), SC(-42,-20), SC(-56,-30), SC(-63,-30), SC(-63,-30), SC(-56,-30), SC(-42,-20), SC(-21,-10)
		},
		// PST_T[64]
		{
		SC(-26,  0), SC( 13,  0), SC( 52,  0), SC( 91,  0), SC( 91,  0), SC( 52,  0), SC( 13,  0), SC(-26,  0),
		SC(-26, 32), SC( 13, 32), SC( 52, 32), SC( 91, 32), SC( 78, 32), SC( 52, 32), SC( 13, 32), SC(-26, 32),   // e7 zou (91,32) moeten zijn ipv (78,32)
		SC(-39, 32), SC(  0, 32), SC( 39, 32), SC( 78, 32), SC( 78, 32), SC( 39, 32), SC(  0, 32), SC(-39, 32),
		SC(-52, 16), SC(-13, 16), SC( 26, 16), SC( 65, 16), SC( 65, 16), SC( 26, 16), SC(-13, 16), SC(-52, 16),
		SC(-52,  0), SC(-13,  0), SC( 26,  0), SC( 65,  0), SC( 65,  0), SC( 26,  0), SC(-13,  0), SC(-52,  0),
		SC(-52,  0), SC(-13,  0), SC( 26,  0), SC( 65,  0), SC( 65,  0), SC( 26,  0), SC(-13,  0), SC(-52,  0),
		SC(-52,  0), SC(-13,  0), SC( 26,  0), SC( 65,  0), SC( 65,  0), SC( 26,  0), SC(-13,  0), SC(-52,  0),
		SC(-52,  0), SC(-13,  0), SC( 26,  0), SC( 65,  0), SC( 65,  0), SC( 26,  0), SC(-13,  0), SC(-52,  0)
		},
		// PST_D[64]
		{
		SC(-30,-54), SC(-30,-54), SC(-30,-54), SC(-30,-54), SC(-30,-54), SC(-30,-54), SC(-30,-54), SC(-30,-54),
		SC(-30,-54), SC(  0,  0), SC(  0,  0), SC(  0,  0), SC(  0,  0), SC(  0,  0), SC(  0,  0), SC(-30,-54),
		SC(-30,-54), SC(  0,  0), SC( 25, 45), SC( 25, 45), SC( 25, 45), SC( 25, 45), SC(  0,  0), SC(-30,-54),
		SC(-30,-54), SC(  0,  0), SC( 25, 45), SC( 45, 81), SC( 45, 81), SC( 25, 45), SC(  0,  0), SC(-30,-54),
		SC(-30,-54), SC(  0,  0), SC( 25, 45), SC( 45, 81), SC( 45, 81), SC( 25, 45), SC(  0,  0), SC(-30,-54),
		SC(-30,-54), SC(  0,  0), SC( 25, 45), SC( 25, 45), SC( 25, 45), SC( 25, 45), SC(  0,  0), SC(-30,-54),
		SC(-30,-54), SC(  0,  0), SC(  0,  0), SC(  0,  0), SC(  0,  0), SC(  0,  0), SC(  0,  0), SC(-30,-54),
		SC(-30,-54), SC(-30,-54), SC(-30,-54), SC(-30,-54), SC(-30,-54), SC(-30,-54), SC(-30,-54), SC(-30,-54)
		}
	};

	#undef SC

Score psq[STUK_N][VELD_N];

// init() initializes piece square tables: the white halves of the tables are
// copied from Bonus[] adding the piece value, then the black halves of the
// tables are initialized by flipping and changing the sign of the white scores.
void init() {

  for (StukType pt = KONING; pt <= DAME; ++pt)
  {
      for (Veld s = SQ_A1; s <= SQ_H8; ++s)
      {
		  Score waarde = PST[pt][s];
		  if (pt == PION) waarde = SCORE_MULDIV(waarde, 128, 256);
		  if (pt == DAME) waarde = SCORE_MULDIV(waarde, 220, 256);
		  psq[maak_stuk(WIT, pt)][~s] = waarde;
          psq[maak_stuk(ZWART, pt)][s] = -waarde;
      }
  }

  // Mobiliteit multiplier
  const int PstRankQuad[64] = {
	  -9,-9,-9,-9,-9,-9,-9,-9,
	  -3,-3,-3,-3,-3,-3,-3,-3,
	  1, 1, 1, 1, 1, 1, 1, 1,
	  3, 3, 3, 3, 3, 3, 3, 3,
	  3, 3, 3, 3, 3, 3, 3, 3,
	  1, 1, 1, 1, 1, 1, 1, 1,
	  -3,-3,-3,-3,-3,-3,-3,-3,
	  -9,-9,-9,-9,-9,-9,-9,-9
  };
  const int PstFileQuad[64] = {
	  -9,-3, 1, 3, 3, 1,-3,-9,
	  -9,-3, 1, 3, 3, 1,-3,-9,
	  -9,-3, 1, 3, 3, 1,-3,-9,
	  -9,-3, 1, 3, 3, 1,-3,-9,
	  -9,-3, 1, 3, 3, 1,-3,-9,
	  -9,-3, 1, 3, 3, 1,-3,-9,
	  -9,-3, 1, 3, 3, 1,-3,-9,
	  -9,-3, 1, 3, 3, 1,-3,-9
  };
  const int PstRank[64] = {
	  -3,-3,-3,-3,-3,-3,-3,-3,
	  -2,-2,-2,-2,-2,-2,-2,-2,
	  -1,-1,-1,-1,-1,-1,-1,-1,
	  0, 0, 0, 0, 0, 0, 0, 0,
	  1, 1, 1, 1, 1, 1, 1, 1,
	  2, 2, 2, 2, 2, 2, 2, 2,
	  3, 3, 3, 3, 3, 3, 3, 3,
	  4, 4, 4, 4, 4, 4, 4, 4
  };
  const int PstMult[DAME + 1][6] = {
	  {},
	  { 15,  5,-12,-18,  0,  2 }, //{  9, 13,-11, -9, -6,  3 }
	  {  0,  0,  0,  0,  0,  0 }, //{ -5, -1,  5,  0, -8,  7 },
	  { 10,  6,  4, -6, -3, 11 },
	  {  0,  0,  0,  0,  0,  0 }, /////////////{  2, -5,  1,  2, -2,  0 },
	  { -2,  2,  4,  6,  0,  5 },
	  {  0,  0, -4,  8, -8, -2 }, /////////////{  4,  0,  4,  0, -3, -2 },
  };

  for (StukType pt = KONING; pt <= DAME; ++pt)
  {
	  for (Veld s = SQ_A1; s <= SQ_H8; ++s)
	  {
		  int mg = (PstMult[pt][0] * PstRankQuad[s] + PstMult[pt][1] * PstFileQuad[s]) / 2 + PstMult[pt][2] * PstRank[s];
		  int eg = (PstMult[pt][3] * PstRankQuad[s] + PstMult[pt][4] * PstFileQuad[s]) / 2 + PstMult[pt][5] * PstRank[s];
		  Score waarde = maak_score(mg, eg);
		  psq[maak_stuk(WIT, pt)][~s] += waarde;
		  psq[maak_stuk(ZWART, pt)][s] -= waarde;
	  }
  }
#if 0
  for (Veld s = SQ_A1; s <= SQ_H8; ++s)
  {
	  int mg = (TUNE_1 * PstRankQuad[s] + TUNE_2 * PstFileQuad[s]) / 2 + TUNE_3 * PstRank[s];
	  int eg = (TUNE_4 * PstRankQuad[s] + TUNE_5 * PstFileQuad[s]) / 2 + TUNE_6 * PstRank[s];
	  Score waarde = maak_score(mg, eg);
	  psq[WIT][PION][~s] += waarde;
	  psq[ZWART][PION][s] -= waarde;
  }
#endif
}

} // namespace PSQT