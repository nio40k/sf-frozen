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

#include <algorithm>
#include <cassert>
#include <cstring>   // For std::memset
#include <iomanip>
#include <sstream>
#include <iostream>

#include "bitboard.h"
#include "evaluate.h"
#include "material.h"
#include "pawns.h"
#include "uci.h"
#include "thread.h"


namespace
{
	static const Bitboard NIET_LIJN_A = 0xFEFEFEFEFEFEFEFE;
	static const Bitboard NIET_LIJN_H = 0x7F7F7F7F7F7F7F7F;
	inline Bitboard bb(Veld s) { return (1ULL << s); }
	inline Bitboard bb2(Veld s1, Veld s2) { return (1ULL << s1) | (1ULL << s2); }
	inline Bitboard bb3(Veld s1, Veld s2, Veld s3) { return (1ULL << s1) | (1ULL << s2) | (1ULL << s3); }
	inline Bitboard bb4(Veld s1, Veld s2, Veld s3, Veld s4) { return (1ULL << s1) | (1ULL << s2) | (1ULL << s3) | (1ULL << s4); }

	unsigned int MobiMult_P[64], MobiMult_L1[64], MobiMult_L2[64], MobiMult_T[64], MobiMult_D[64];
	Score mobi_P[256], mobi_L1[256], mobi_L2[256], mobi_T[256], mobi_D[256];

	Score Score_Afstand_P_K[9];
	Score Score_PionOpKleurLoper[9], Score_PionAndereKleurLoper[9];
	Score Score_Pion_Vast_Dvleugel[9];
	Score Score_Geblokkeerde_Pionnen[9];
	Score Score_PionLijnBreedte[9];
	Score Score_Vrijpion_verdedigd[9];
	Score Score_Vrijpion_niet_verdedigd[9];
	Score Score_Vrijpion_aangevallen[9];
	Score Score_Vrijpion_vrije_doorgang[9];
	Score Score_Vrijpion_niet_DvD[9];
	Score Score_Vrijpion_DvD[9];

	Score VeiligheidTabel[1024];

	Score Score_PL_achter_Pion[9];
	Score Score_NietVerdedigdePionnen[9];
	Score Score_Dreigingen[9];
	Score Score_StukkenMoetenVerdedigen[9];
	Score Score_Ondersteunde_Stukken[9];
	Score Score_SterkVeld_PL[9];
	Score Score_VeiligVoorPion_TLP[9];
	Score Score_SterkP_voor_Pion[9];

	static const int afstand_P_K_tabel[9] = { 0, 8, 3, 0, -2, -3, -4, -5, 0 };

	static const int KoningGevaar[256] = {
		0, 1, 3, 6, 11, 17, 25, 34, 44, 55, 68, 81, 96, 112, 129, 147,
		166, 185, 205, 227, 248, 270, 293, 317, 340, 364, 388, 413, 438, 463, 487, 512,
		537, 562, 587, 611, 636, 660, 684, 708, 731, 754, 777, 799, 821, 842, 863, 884,
		904, 924, 943, 962, 980, 998, 1016, 1033, 1049, 1065, 1081, 1096, 1111, 1125, 1139, 1152,
		1165, 1178, 1190, 1201, 1213, 1224, 1234, 1245, 1254, 1264, 1273, 1282, 1291, 1299, 1307, 1314,
		1322, 1329, 1336, 1342, 1349, 1355, 1360, 1366, 1371, 1377, 1382, 1387, 1391, 1396, 1400, 1404,
		1408, 1412, 1415, 1419, 1422, 1425, 1428, 1431, 1434, 1437, 1439, 1442, 1444, 1447, 1449, 1451,
		1453, 1455, 1457, 1459, 1460, 1462, 1464, 1465, 1467, 1468, 1469, 1471, 1472, 1473, 1474, 1475,
		1476, 1476, 1476, 1476, 1476, 1476, 1476, 1476, 1476, 1476, 1476, 1476, 1476, 1476, 1476, 1476,
		1476, 1476, 1476, 1476, 1476, 1476, 1476, 1476, 1476, 1476, 1476, 1476, 1476, 1476, 1476, 1476,
		1476, 1476, 1476, 1476, 1476, 1476, 1476, 1476, 1476, 1476, 1476, 1476, 1476, 1476, 1476, 1476,
		1476, 1476, 1476, 1476, 1476, 1476, 1476, 1476, 1476, 1476, 1476, 1476, 1476, 1476, 1476, 1476,
		1476, 1476, 1476, 1476, 1476, 1476, 1476, 1476, 1476, 1476, 1476, 1476, 1476, 1476, 1476, 1476,
		1476, 1476, 1476, 1476, 1476, 1476, 1476, 1476, 1476, 1476, 1476, 1476, 1476, 1476, 1476, 1476,
		1476, 1476, 1476, 1476, 1476, 1476, 1476, 1476, 1476, 1476, 1476, 1476, 1476, 1476, 1476, 1476,
		1476, 1476, 1476, 1476, 1476, 1476, 1476, 1476, 1476, 1476, 1476, 1476, 1476, 1476, 1476, 1476
	};

	static const Bitboard bb_L_domineert_P[2][64] = 
	{
		{
			0, 0, 0, 0, 0, 0, 0, 0,
			bb(SQ_A5), bb(SQ_B5), bb(SQ_C5), bb(SQ_D5), bb(SQ_E5), bb(SQ_F5), bb(SQ_G5), bb(SQ_H5),
			bb(SQ_A6), bb(SQ_B6), bb(SQ_C6), bb(SQ_D6), bb(SQ_E6), bb(SQ_F6), bb(SQ_G6), bb(SQ_H6),
			bb(SQ_A7), bb(SQ_B7), bb(SQ_C7), bb(SQ_D7), bb(SQ_E7), bb(SQ_F7), bb(SQ_G7), bb(SQ_H7),
			bb(SQ_A8), bb(SQ_B8), bb(SQ_C8), bb(SQ_D8), bb(SQ_E8), bb(SQ_F8), bb(SQ_G8), bb(SQ_H8),
			0, 0, 0, 0, 0, 0, 0, 0,
			0, 0, 0, 0, 0, 0, 0, 0,
			0, 0, 0, 0, 0, 0, 0, 0
		},
		{
			0, 0, 0, 0, 0, 0, 0, 0,
			0, 0, 0, 0, 0, 0, 0, 0,
			0, 0, 0, 0, 0, 0, 0, 0,
			bb(SQ_A1), bb(SQ_B1), bb(SQ_C1), bb(SQ_D1), bb(SQ_E1), bb(SQ_F1), bb(SQ_G1), bb(SQ_H1),
			bb(SQ_A2), bb(SQ_B2), bb(SQ_C2), bb(SQ_D2), bb(SQ_E2), bb(SQ_F2), bb(SQ_G2), bb(SQ_H2),
			bb(SQ_A3), bb(SQ_B3), bb(SQ_C3), bb(SQ_D3), bb(SQ_E3), bb(SQ_F3), bb(SQ_G3), bb(SQ_H3),
			bb(SQ_A4), bb(SQ_B4), bb(SQ_C4), bb(SQ_D4), bb(SQ_E4), bb(SQ_F4), bb(SQ_G4), bb(SQ_H4),
			0, 0, 0, 0, 0, 0, 0, 0
		}
	};

	static const Bitboard opgesloten_loper_b3_c2[2][64] =
	{
		{
			bb(SQ_B2), bb(SQ_C2), 0, 0, 0, 0, bb(SQ_F2), bb(SQ_G2),
			bb(SQ_B3), 0, 0, 0, 0, 0, 0, bb(SQ_G3),
			0, 0, 0, 0, 0, 0, 0, 0,
			0, 0, 0, 0, 0, 0, 0, 0,
			0, 0, 0, 0, 0, 0, 0, 0,
			bb(SQ_B5), 0, 0, 0, 0, 0, 0, bb(SQ_G5),
			bb(SQ_B6), 0, 0, 0, 0, 0, 0, bb(SQ_G6),
			bb(SQ_B7), bb(SQ_C7), 0, 0, 0, 0, bb(SQ_F7), bb(SQ_G7)
		},
		{
			bb(SQ_B2), bb(SQ_C2), 0, 0, 0, 0, bb(SQ_F2), bb(SQ_G2),
			bb(SQ_B3), 0, 0, 0, 0, 0, 0, bb(SQ_G3),
			bb(SQ_B4), 0, 0, 0, 0, 0, 0, bb(SQ_G4),
			0, 0, 0, 0, 0, 0, 0, 0,
			0, 0, 0, 0, 0, 0, 0, 0,
			0, 0, 0, 0, 0, 0, 0, 0,
			bb(SQ_B6), 0, 0, 0, 0, 0, 0, bb(SQ_G6),
			bb(SQ_B7), bb(SQ_C7), 0, 0, 0, 0, bb(SQ_F7), bb(SQ_G7)
		} 
	};
	static const Bitboard opgesloten_loper_b3_c2_extra[64] =
	{
		bb(SQ_B3), bb(SQ_B3), 0, 0, 0, 0, bb(SQ_G3), bb(SQ_G3),
		bb(SQ_C2), 0, 0, 0, 0, 0, 0, bb(SQ_F2),
		bb(SQ_C3), 0, 0, 0, 0, 0, 0, bb(SQ_F3),
		0, 0, 0, 0, 0, 0, 0, 0,
		0, 0, 0, 0, 0, 0, 0, 0,
		bb(SQ_C6), 0, 0, 0, 0, 0, 0, bb(SQ_F6),
		bb(SQ_C7), 0, 0, 0, 0, 0, 0, bb(SQ_F7),
		bb(SQ_B6), bb(SQ_B6), 0, 0, 0, 0, bb(SQ_G6), bb(SQ_G6)
	};

	/*
	static const Bitboard opgesloten_loper_b4_d2[2][64] =
	{
		{
			bb2(SQ_B4, SQ_C3), 0, bb2(SQ_B4, SQ_C3), 0, 0, bb2(SQ_G4, SQ_F3), 0, bb2(SQ_G4, SQ_F3),
			0, bb2(SQ_B4, SQ_C3), 0, 0, 0, 0, bb2(SQ_G4, SQ_F3), 0,
			bb2(SQ_B4, SQ_C3), 0, 0, 0, 0, 0, 0, bb2(SQ_G4, SQ_F3),
			0, 0, 0, 0, 0, 0, 0, 0,
			0, 0, 0, 0, 0, 0, 0, 0,
			0, 0, 0, 0, 0, 0, 0, 0,
			0, 0, 0, 0, 0, 0, 0, 0,
			0, 0, 0, 0, 0, 0, 0, 0
		},
		{
			0, 0, 0, 0, 0, 0, 0, 0,
			0, 0, 0, 0, 0, 0, 0, 0,
			0, 0, 0, 0, 0, 0, 0, 0,
			0, 0, 0, 0, 0, 0, 0, 0,
			0, 0, 0, 0, 0, 0, 0, 0,
			bb2(SQ_B5, SQ_C6), 0, 0, 0, 0, 0, 0, bb2(SQ_G5, SQ_F6),
			0, bb2(SQ_B5, SQ_C6), 0, 0, 0, 0, bb2(SQ_G5, SQ_F6), 0,
			bb2(SQ_B5, SQ_C6), 0, bb2(SQ_B5, SQ_C6), 0, 0, bb2(SQ_G5, SQ_F6), 0, bb2(SQ_G5, SQ_F6)
		}
	};
	static const Bitboard opgesloten_loper_b4_d2_extra[64] =
	{
		bb2(SQ_D2, SQ_E3), 0, bb2(SQ_D2, SQ_E3), 0, 0, bb2(SQ_E2, SQ_D3), 0, bb2(SQ_E2, SQ_D3),
		0, bb2(SQ_D2, SQ_E3), 0, 0, 0, 0, bb2(SQ_E2, SQ_D3), 0,
		bb2(SQ_D2, SQ_E3), 0, 0, 0, 0, 0, 0, bb2(SQ_E2, SQ_D3),
		0, 0, 0, 0, 0, 0, 0, 0,
		0, 0, 0, 0, 0, 0, 0, 0,
		bb2(SQ_D7, SQ_E6), 0, 0, 0, 0, 0, 0, bb2(SQ_E7, SQ_D6),
		0, bb2(SQ_D7, SQ_E6), 0, 0, 0, 0, bb2(SQ_E7, SQ_D6), 0,
		bb2(SQ_D7, SQ_E6), 0, bb2(SQ_D7, SQ_E6), 0, 0, bb2(SQ_E7, SQ_D6), 0, bb2(SQ_E7, SQ_D6)
	};

	static const Bitboard opgesloten_toren[64] =
	{
		0, bb2(SQ_A1, SQ_A2), bb4(SQ_A1, SQ_A2, SQ_B1, SQ_B2), 0, 0, bb4(SQ_H1, SQ_H2, SQ_G1, SQ_G2), bb2(SQ_H1, SQ_H2), 0,
		0, 0, 0, 0, 0, 0, 0, 0,
		0, 0, 0, 0, 0, 0, 0, 0,
		0, 0, 0, 0, 0, 0, 0, 0,
		0, 0, 0, 0, 0, 0, 0, 0,
		0, 0, 0, 0, 0, 0, 0, 0,
		0, 0, 0, 0, 0, 0, 0, 0,
		0, bb2(SQ_A8, SQ_A7), bb4(SQ_A8, SQ_A7, SQ_B8, SQ_B7), 0, 0, bb4(SQ_H8, SQ_H7, SQ_G8, SQ_G7), bb2(SQ_H8, SQ_H7), 0
	};
	*/

	// evaluate_scale_factor() computes the scale factor for the winning side
	SchaalFactor bereken_schaalfactor(const Stelling& pos, const Materiaal::Tuple* materiaalTuple, EvalWaarde value) {

		Kleur sterkeZijde = value > REMISE_WAARDE ? WIT : ZWART;
		SchaalFactor sf = materiaalTuple->schaalfactor_uit_functie(pos, sterkeZijde);

		// If we don't already have an unusual scale factor, check for certain
		// types of endgames, and use a lower scale for those.
		if (abs(value) <= LOPER_EVAL && (sf == NORMAAL_FACTOR || sf == EEN_PION_FACTOR))
		{
			if (pos.ongelijke_lopers())
			{
				// Endgame with opposite-colored bishops and no other pieces (ignoring pawns)
				// is almost a draw, in case of KBP vs KB is even more a draw.
				if (pos.niet_pion_materiaal(WIT) == MATL_LOPER
					&& pos.niet_pion_materiaal(ZWART) == MATL_LOPER)
					sf = pos.aantal<PION>(sterkeZijde) > 1 ? SchaalFactor(50) : SchaalFactor(12);

				// Endgame with opposite-colored bishops, but also other pieces. Still
				// a bit drawish, but not as drawish as with only the two bishops.
				else
					sf = SchaalFactor(3 * sf / 4);
			}
			// Endings where weaker side can place his king in front of the opponent's
			// pawns are drawish.
			else if (pos.aantal<PION>(sterkeZijde) <= 2
				&& !pos.is_vrijpion(~sterkeZijde, pos.veld<KONING>(~sterkeZijde)))
				sf = SchaalFactor(58 + 11 * pos.aantal<PION>(sterkeZijde));
		}

		return sf;
	}

#define threat_make_score(mg, eg) maak_score(230 * (174 * mg + 12 * eg) / 64 / 186, 188 * (-26 * mg + 212 * eg) / 64 / 186)
#define S(mg, eg) threat_make_score(mg, eg)

	const Score StukDreiging[STUKTYPE_N] = {
		S(0, 0), S(0, 0), S(0, 33), S(45, 43), S(46, 47), S(72, 107), S(48,118)
	};
	const Score TorenDreiging[STUKTYPE_N] = {
		S(0, 0), S(0, 0), S(0, 25), S(40, 62), S(40, 59), S(0, 34), S(35, 48)
	};
	const Score PionDreiging[STUKTYPE_N] = {
		S(0, 0), S(0, 0), S(0, 0), S(176, 139), S(131, 127), S(217, 218), S(203, 215)
	};

#undef S

	// King danger constants and variables. The king danger scores are looked-up
	// in KingDanger[]. Various little "meta-bonuses" measuring the strength
	// of the enemy attack are added up into an integer, which is used as an
	// index to KingDanger[].
	Score KingDanger[512];

	// KingAttackWeights[StukType] contains king attack weights by piece type
	const int KingAttackWeights[STUKTYPE_N] = { 0, 0, 35, 125, 80, 80, 72 };

	// Penalties for enemy's safe checks
	const int QueenContactCheck = 89;
	const int QueenCheck = 50;
	const int RookCheck = 45;
	const int BishopCheck = 6;
	const int KnightCheck = 14;

	static const int VrijpionNabijheid[8] = { 13, 9, 6, 4, 3, 2, 1, 0 };
	static const Score Score_Pion_Vrijpion_ifvLijn[8] = {
		maak_score(0, 60 * 32 / 35), maak_score(0, 20 * 32 / 35), maak_score(0, -20 * 32 / 35), maak_score(0, -60 * 32 / 35),
		maak_score(0, -60 * 32 / 35), maak_score(0, -20 * 32 / 35), maak_score(0, 20 * 32 / 35), maak_score(0, 60 * 32 / 35)
	};

	Score Score_Pion_W[6][8];
	Score Score_Pion_AA[6][8];

	//inline Score SCORE_MULDIV(Score x, int mul, int div) {
	//	return maak_score(mg_waarde(x) * mul / div, eg_waarde(x) * mul / div);
	//}

	//Bitboard bb_centrum_pivot_W[LIJN_NB], bb_centrum_pivot_Z[LIJN_NB];
	//Bitboard bb_koning_flank_W[LIJN_NB] = {
	//	(Rank4BB | Rank5BB | Rank6BB | Rank7BB | Rank8BB) & (FileABB | FileBBB | FileCBB | FileDBB),
	//	(Rank4BB | Rank5BB | Rank6BB | Rank7BB | Rank8BB) & (FileABB | FileBBB | FileCBB | FileDBB),
	//	(Rank4BB | Rank5BB | Rank6BB | Rank7BB | Rank8BB) & (FileABB | FileBBB | FileCBB | FileDBB),
	//	(Rank4BB | Rank5BB | Rank6BB | Rank7BB | Rank8BB) & (FileCBB | FileDBB | FileEBB | FileFBB),
	//	(Rank4BB | Rank5BB | Rank6BB | Rank7BB | Rank8BB) & (FileCBB | FileDBB | FileEBB | FileFBB),
	//	(Rank4BB | Rank5BB | Rank6BB | Rank7BB | Rank8BB) & (FileEBB | FileFBB | FileGBB | FileHBB),
	//	(Rank4BB | Rank5BB | Rank6BB | Rank7BB | Rank8BB) & (FileEBB | FileFBB | FileGBB | FileHBB),
	//	(Rank4BB | Rank5BB | Rank6BB | Rank7BB | Rank8BB) & (FileEBB | FileFBB | FileGBB | FileHBB)
	//};
	//Bitboard bb_koning_flank_Z[LIJN_NB] = {
	//	(Rank5BB | Rank4BB | Rank3BB | Rank2BB | Rank1BB) & (FileABB | FileBBB | FileCBB | FileDBB),
	//	(Rank5BB | Rank4BB | Rank3BB | Rank2BB | Rank1BB) & (FileABB | FileBBB | FileCBB | FileDBB),
	//	(Rank5BB | Rank4BB | Rank3BB | Rank2BB | Rank1BB) & (FileABB | FileBBB | FileCBB | FileDBB),
	//	(Rank5BB | Rank4BB | Rank3BB | Rank2BB | Rank1BB) & (FileCBB | FileDBB | FileEBB | FileFBB),
	//	(Rank5BB | Rank4BB | Rank3BB | Rank2BB | Rank1BB) & (FileCBB | FileDBB | FileEBB | FileFBB),
	//	(Rank5BB | Rank4BB | Rank3BB | Rank2BB | Rank1BB) & (FileEBB | FileFBB | FileGBB | FileHBB),
	//	(Rank5BB | Rank4BB | Rank3BB | Rank2BB | Rank1BB) & (FileEBB | FileFBB | FileGBB | FileHBB),
	//	(Rank5BB | Rank4BB | Rank3BB | Rank2BB | Rank1BB) & (FileEBB | FileFBB | FileGBB | FileHBB)
	//};

} // namespace


template<Kleur IK>
Bitboard bereken_aanval(const Stelling& pos)
{
	const Veld* pVeld;
	Veld veld;
	Bitboard aanval;

	aanval = pos.aanval_van<KONING>(pos.veld<KONING>(IK));
	aanval |= pion_aanval<IK>(pos.stukken(IK, PION));

	pVeld = pos.veld_lijst<PAARD>(IK);
	while ((veld = *pVeld++) != SQ_NONE)
		aanval |= pos.aanval_van<PAARD>(veld);

	pVeld = pos.veld_lijst<LOPER>(IK);
	while ((veld = *pVeld++) != SQ_NONE)
		aanval |= pos.aanval_van<LOPER>(veld);

	pVeld = pos.veld_lijst<TOREN>(IK);
	while ((veld = *pVeld++) != SQ_NONE)
		aanval |= pos.aanval_van<TOREN>(veld);

	pVeld = pos.veld_lijst<DAME>(IK);
	while ((veld = *pVeld++) != SQ_NONE)
		aanval |= pos.aanval_van<DAME>(veld);

	return aanval;
}


template<Kleur IK>
bool minstens_twee_mobiele_stukken(const Stelling& pos)
{
	const Kleur JIJ = (IK == WIT ? ZWART : WIT);
	const Veld* pVeld;
	Veld veld;

	Bitboard jouwAanval = bereken_aanval<JIJ>(pos);
	Bitboard gepend = pos.info()->xRay[IK];

	Bitboard veilig = ~pos.stukken(IK) & ~jouwAanval;
	bool mobiel = false;

	Bitboard aanval = pos.aanval_van<KONING>(pos.veld<KONING>(IK));
	if (aanval & veilig)
	{
		// koning die aangevallen stuk verdedigt is niet mobiel
		if (!(aanval & pos.stukken(IK) & jouwAanval))
			mobiel = true;
	}

	pVeld = pos.veld_lijst<PAARD>(IK);
	while ((veld = *pVeld++) != SQ_NONE)
	{
		if (gepend & veld)
			continue;
		if (pos.aanval_van<PAARD>(veld) & veilig)
		{
			if (mobiel)
				return true;
			mobiel = true;
		}
	}

	pVeld = pos.veld_lijst<LOPER>(IK);
	while ((veld = *pVeld++) != SQ_NONE)
	{
		if (gepend & veld)
			continue;
		if (pos.aanval_van<LOPER>(veld) & veilig)
		{
			if (mobiel)
				return true;
			mobiel = true;
		}
	}

	pVeld = pos.veld_lijst<TOREN>(IK);
	while ((veld = *pVeld++) != SQ_NONE)
	{
		if (gepend & veld)
			continue;
		if (pos.aanval_van<TOREN>(veld) & veilig)
		{
			if (mobiel)
				return true;
			mobiel = true;
		}
	}

	pVeld = pos.veld_lijst<DAME>(IK);
	while ((veld = *pVeld++) != SQ_NONE)
	{
		if (gepend & veld)
			continue;
		if (pos.aanval_van<DAME>(veld) & veilig)
		{
			if (mobiel)
				return true;
			mobiel = true;
		}
	}

	return false;
}

bool Eval::minstens_twee_mobiele_stukken(const Stelling& pos)
{
	if (pos.aan_zet() == WIT)
		return ::minstens_twee_mobiele_stukken<WIT>(pos);
	else
		return ::minstens_twee_mobiele_stukken<ZWART>(pos);
}


template<Kleur IK>
inline Score Dreigingen(const Stelling& pos, const AanvalInfo& ai)
{
	const Kleur JIJ = (IK == WIT ? ZWART : WIT);

	Bitboard bb_niet_verdedigd_Z = pos.stukken(JIJ) & ~ai.aanval[JIJ][ALLE_STUKKEN];

	Bitboard ZP_Pion_aangevallen_L = ai.aanval[IK][LOPER] & pos.stukken(JIJ, PAARD, PION);
	Bitboard ZLP_Pion_aangevallen_T = ai.aanval[IK][TOREN] & pos.stukken(JIJ, LOPER, PAARD, PION);
	Bitboard ZL_Pion_aangevallen_P = ai.aanval[IK][PAARD] & pos.stukken(JIJ, LOPER, PION);

	// zwarte stukken aangevallen door WPion, ZD aangevallend door WT, ZD of ZT aangevallend door WL of WP (waardevol aangevallen door minder waardevol)
	int dreigingenW = popcount((pos.stukken(JIJ) ^ pos.stukken(JIJ, PION)) & ai.aanval[IK][PION]
		| pos.stukken(JIJ, DAME) & ai.aanval[IK][TOREN]
		| pos.stukken(JIJ, DAME, TOREN) & (ai.aanval[IK][LOPER] | ai.aanval[IK][PAARD]));
	Score score = Score_Dreigingen[dreigingenW];

	// niet verdedigde zwarte stukken aangevallen door waardevoller stuk
	dreigingenW += popcount(bb_niet_verdedigd_Z & (ai.aanval[IK][KONING] | ZP_Pion_aangevallen_L | ZL_Pion_aangevallen_P | ZLP_Pion_aangevallen_T
		| ai.aanval[IK][DAME] & (pos.stukken(JIJ) ^ pos.stukken(JIJ, DAME))));
	score += Score_Dreigingen[dreigingenW];

	score += Score_NietVerdedigdePionnen[popcount(pos.stukken(IK, PION) & ~ai.aanval[IK][STUKKEN_ZONDER_KONING])];
	score += Score_StukkenMoetenVerdedigen[popcount((ai.aanval[JIJ][STUKKEN_ZONDER_KONING] ^ ai.aanval[JIJ][PION]) & ai.aanval[IK][STUKKEN_ZONDER_KONING] & pos.stukken(JIJ))];

	return score;
}

template<Kleur IK>
Score sf_Dreigingen(const Stelling& pos, const AanvalInfo& ai) 
{
	const Kleur JIJ = (IK == WIT ? ZWART : WIT);
	const Bitboard RANK2 = (IK == WIT ? Rank2BB : Rank7BB);
	const Bitboard RANK7 = (IK == WIT ? Rank7BB : Rank2BB);

#define S(mg, eg) threat_make_score(mg, eg)
	const Score HangendePionDreiging = S(70, 63);
	const Score KoningDreigingEnkel = S(3, 62);
	const Score KoningDreigingMeerdere = S(9, 138);
	const Score HangendeStukken = S(48, 28);
	const Score PionOpmars = S(31, 19);
	const Score LooseEnemies = S(0, 25);
#undef S

	Score score = SCORE_ZERO;

	// Loose enemies
	//if ((pos.stukken(JIJ) ^ pos.stukken(JIJ, DAME, KONING)) & ~ai.attack[Us][ALLE_STUKKEN] & ~ai.attack[JIJ][ALLE_STUKKEN])
	//	score += LooseEnemies;

	// Non-pawn enemies attacked by a pawn
	Bitboard pionDreigingen = (pos.stukken(JIJ) ^ pos.stukken(JIJ, PION)) & ai.aanval[IK][PION];

	if (pionDreigingen)
	{
		Bitboard veiligePionnen = pos.stukken(IK, PION) & (~ai.aanval[JIJ][ALLE_STUKKEN] | ai.aanval[IK][ALLE_STUKKEN]);
		Bitboard veiligeDreigingen = pion_aanval<IK>(veiligePionnen) & pionDreigingen;

		if (pionDreigingen ^ veiligeDreigingen)
			score += HangendePionDreiging;

		while (veiligeDreigingen)
			score += PionDreiging[stuk_type(pos.stuk_op_veld(pop_lsb(&veiligeDreigingen)))];
	}

	// Non-pawn enemies defended by a pawn
	Bitboard ondersteundeStukken = (pos.stukken(JIJ) ^ pos.stukken(JIJ, PION)) & ai.aanval[JIJ][PION];

	// Enemies not defended by a pawn and under our attack
	Bitboard zwakkeStukken = pos.stukken(JIJ) & ~ai.aanval[JIJ][PION] & ai.aanval[IK][ALLE_STUKKEN];

	// Add a bonus according to the kind of attacking pieces
	if (ondersteundeStukken | zwakkeStukken)
	{
		Bitboard b = (ondersteundeStukken | zwakkeStukken) & (ai.aanval[IK][PAARD] | ai.aanval[IK][LOPER]);
		while (b)
			score += StukDreiging[stuk_type(pos.stuk_op_veld(pop_lsb(&b)))];

		b = (pos.stukken(JIJ, DAME) | zwakkeStukken) & ai.aanval[IK][TOREN];
		while (b)
			score += TorenDreiging[stuk_type(pos.stuk_op_veld(pop_lsb(&b)))];

		score += HangendeStukken * popcount(zwakkeStukken & ~ai.aanval[JIJ][ALLE_STUKKEN]);

		b = zwakkeStukken & ai.aanval[IK][KONING];
		if (b)
			score += meer_dan_een(b) ? KoningDreigingMeerdere : KoningDreigingEnkel;
	}

	// Bonus if some pawns can safely push and attack an enemy piece
	Bitboard b = pos.stukken(IK, PION) & ~RANK7;
	b = shift_up<IK>(b | (shift_up<IK>(b & RANK2) & ~pos.stukken()));
	//b = shift_up<IK>(b | (shift_up<IK>(b & RANK2) & ~pos.stukken() & ~ai.aanval[JIJ][PION]));

	b &= ~pos.stukken()
		& ~ai.aanval[JIJ][PION]
		& (ai.aanval[IK][ALLE_STUKKEN] | ~ai.aanval[JIJ][ALLE_STUKKEN]);

	b = pion_aanval<IK>(b)
		&  pos.stukken(JIJ)
		& ~ai.aanval[IK][PION];

	score += PionOpmars * popcount(b);

	return score;
}

template<Kleur IK>
inline Score Vrijpionnen(const Stelling& pos, const AanvalInfo& ai, Bitboard bb_vrijpionnen)
{
	const Kleur JIJ = (IK == WIT ? ZWART : WIT);

	Score score = SCORE_ZERO;
	while (bb_vrijpionnen)
	{
		Veld vrijpion = pop_lsb(&bb_vrijpionnen);

		score += Score_Pion_Vrijpion_ifvLijn[lijn(vrijpion)];

		int pion_rij = relatieve_rij(IK, vrijpion) - 1;
		if (pos.niet_pion_materiaal(WIT) == MATL_DAME && pos.niet_pion_materiaal(ZWART) == MATL_DAME)
			score += Score_Vrijpion_DvD[pion_rij];
		else
			score += Score_Vrijpion_niet_DvD[pion_rij];

		if (pion_rij > 1)
		{
			// afstand tot veld voor pion
			Veld veld_voor_pion = vrijpion + pion_vooruit(IK);
			int mijnAfstand = afstand(veld_voor_pion, pos.veld<KONING>(IK));
			int jouwAfstand = afstand(veld_voor_pion, pos.veld<KONING>(JIJ));

			score += Score_Pion_W[pion_rij][mijnAfstand];  // afstand mijn K tot mijn vrijpion
			score += Score_Pion_AA[pion_rij][jouwAfstand];  // afstand jouw K tot mijn vrijpion

			// afstand tot promotieveld
			Veld promotie_veld = Veld(maak_veld(lijn(vrijpion), Rij(7 * JIJ)));
			if (afstand(promotie_veld, pos.veld<KONING>(IK)) <= 1)
				score += maak_score(0, 129 * 32 / 35);
			if (afstand(promotie_veld, pos.veld<KONING>(JIJ)) <= 1)
				score -= maak_score(0, 129 * 32 / 35);

			// veld voor pion = vrijpion + voorwaarts_offset
			if (pion_rij > 2)
			{
				Bitboard bb_achter_vrijpion = bb_voorwaarts(JIJ, vrijpion);
				// toren achter pion
				if (bb_achter_vrijpion & pos.stukken(IK, TOREN))
					score += maak_score(62, 274);
				if (bb_achter_vrijpion & pos.stukken(JIJ, TOREN))
					score -= maak_score(62, 274);
			}
			if (!(pos.stukken() & (vrijpion + pion_vooruit(IK))))
			{
				// vrijpion kan voorwaarts
				Bitboard vrijpion_pad = bb_voorwaarts(IK, vrijpion);
				// AlleAanval hier is aanvallen behalve K
				Bitboard bb_ondersteund = vrijpion_pad & ai.aanval[IK][STUKKEN_ZONDER_KONING];
				Bitboard bb_opmars_gestuit = vrijpion_pad & (pos.stukken(JIJ) | ai.aanval[JIJ][STUKKEN_ZONDER_KONING]);

				Bitboard aangevallen = pos.stukken(JIJ, TOREN, DAME) & bb_voorwaarts(JIJ, vrijpion);
				// jouw T of D achter vrijpion -> hele vrijpion_pad is aangevallen
				while (aangevallen)
				{
					Veld veld = pop_lsb(&aangevallen);
					// geen stuk tussen vrijpion en D/T achter vrijpion -> hele pad is gestuit
					if (!((pos.stukken() ^ veld) & bb_tussen(vrijpion, veld)))
						bb_opmars_gestuit = vrijpion_pad;
				}

				if (bb_opmars_gestuit)
				{
					// pad is aangevallen, maar ook verdedigd
					if ((bb_opmars_gestuit & bb_ondersteund) == bb_opmars_gestuit)
						score += Score_Vrijpion_aangevallen[pion_rij];
				}
				else if (vrijpion_pad == bb_ondersteund)
					// hele pad is verdedigd
					score += Score_Vrijpion_verdedigd[pion_rij];
				else
					score += Score_Vrijpion_niet_verdedigd[pion_rij];

				if (!(vrijpion_pad & pos.stukken(IK)))
					score += Score_Vrijpion_vrije_doorgang[pion_rij];
			}
		}
	}

	//ai.EvalInteraction[eitVrijpionnen] += std::max((int)eg_waarde(score), 0);
	return score;
}

template<Kleur IK>
inline Score KoningsAanval(const Stelling& pos, const AanvalInfo& ai, int AanvalScore)
{
	const Kleur JIJ = (IK == WIT ? ZWART : WIT);

	int aanvalIndex = AanvalScore
		+ 16 * popcount(ai.aanval[JIJ][KONING] & ai.aanval[IK][ALLE_STUKKEN] & ~ai.aanval[JIJ][STUKKEN_ZONDER_KONING]);

	if (ai.gepend[JIJ]) // penningen of aftrekschaak
		aanvalIndex += 12;

	// niet verdedigde aangevallen velden 2 rijen voor de koning
	aanvalIndex += 11 * popcount(shift_down<IK>(ai.aanval[JIJ][KONING]) & ~ai.aanval[JIJ][ALLE_STUKKEN] & ai.aanval[IK][ALLE_STUKKEN] & ~pos.stukken(IK));

	// Analyse the enemy's safe distance checks for sliders and knights
	const Veld veldK = pos.veld<KONING>(JIJ);
	Bitboard Schaak_OK = ~(pos.stukken(IK) | ai.aanval[JIJ][PION] & ~pos.stukken(JIJ));
	
	Bitboard Schaakvelden_R = pos.aanval_van<TOREN>(veldK) & Schaak_OK;
	Bitboard Schaakvelden_B = pos.aanval_van<LOPER>(veldK) & Schaak_OK;
	Bitboard PaardSchaakvelden = pos.aanval_van<PAARD>(veldK) & Schaak_OK;

	// Safe checks
	aanvalIndex += 32 * popcount(PaardSchaakvelden & ai.aanval[IK][PAARD]);
	aanvalIndex += 24 * popcount(Schaakvelden_B & ai.aanval[IK][LOPER]);
	aanvalIndex += 32 * popcount(Schaakvelden_R & ai.aanval[IK][TOREN]);

	// Enemy queen safe checks
	Bitboard dame_schaak = ai.aanval[IK][DAME] & (Schaakvelden_B | Schaakvelden_R);
	int KaanvalScore_D = popcount(dame_schaak) * 8;
	KaanvalScore_D += popcount(dame_schaak & ~ai.aanval[JIJ][KONING]) * 16;
#if 1
	dame_schaak &= ai.aanval[JIJ][KONING];
	while (dame_schaak)
	{
		Veld schaakveld = pop_lsb(&dame_schaak);
		Veld van = pos.veld_lijst<DAME>(IK)[0];
		#if 0
		if (pos.aantal<DAME>(IK) > 1 && afstand(van, schaakveld) > 1)
		{
			Bitboard b = bb_tussen(van, schaakveld);
			if (!b || (pos.stukken() & b))
				van = pos.veld_lijst<DAME>(IK)[1];
		}
		#endif
		if (pos.see_test(maak_zet(van, schaakveld), SEE_0))
			KaanvalScore_D += 72;
	}
#else
	//dame_schaak &= ai.attack[JIJ][KONING] & ~ai.attack[JIJ][STUKKEN_ZONDER_KONING];
	dame_schaak &= ai.aanval[JIJ][KONING] & (~ai.aanval[JIJ][STUKKEN_ZONDER_KONING] | pos.stukken(JIJ, DAME));
	while (dame_schaak)
	{
		Veld schaakveld = pop_lsb(&dame_schaak);
		if (pos.stuk_op_veld(schaakveld) == maak_stuk(JIJ, DAME))
		{
			KaanvalScore_D += 72;
			continue;
		}

		Veld van = pos.veld_lijst<DAME>(IK)[0];
		if (pos.aantal<DAME>(IK) > 1 && afstand(van, schaakveld) > 1)
		{
			Bitboard b = bb_tussen(van, schaakveld);
			if (!b || (pos.stukken() & b))
				van = pos.veld_lijst<DAME>(IK)[1];
		}

		Bitboard attackers = pos.aanvallers_naar(schaakveld, pos.stukken() ^ van);
		bool dubbelAanval = attackers & (pos.stukken(IK) ^ van);
		bool verdedigd = attackers & (pos.stukken(JIJ) ^ pos.stukken(JIJ, KONING));

		if (dubbelAanval && !verdedigd)
			KaanvalScore_D += 72;
	}
#endif
	aanvalIndex += std::min(KaanvalScore_D, 200);

	if (aanvalIndex < 0)
		aanvalIndex = 0;
	if (aanvalIndex > 1000)
		aanvalIndex = 1000;

	//ai.KAanvalScore[Us] = aanvalIndex;
	return VeiligheidTabel[aanvalIndex];
}

#ifdef SFAANVAL
template<Kleur IK>
inline Score sf_KoningsAanval(const Stelling& pos, const AanvalInfo& ai, int safety)
{
	const Kleur JIJ = (IK == WIT ? ZWART : WIT);
	const Score Checked = maak_score(20, 20);

	Bitboard undefended, b, b1, b2, safe;
	int attackUnits;
	const Veld veldK = pos.veld<KONING>(IK);

	// King shelter and enemy pawns storm
	Score score = SCORE_ZERO; // ei.pi->koning_veiligheid<Us>(pos, veldK);

	// Main king safety evaluation
	if (ai.kingAttackersCount[JIJ])
	{
		// Find the attacked squares around the king which have no defenders
		// apart from the king itself.
		undefended = ai.aanval[JIJ][ALLE_STUKKEN]
			& ai.aanval[IK][KONING]
			& ~ai.aanval[IK][STUKKEN_ZONDER_KONING];

		// Initialize the 'attackUnits' variable, which is used later on as an
		// index into the KingDanger[] array. The initial value is based on the
		// number and types of the enemy's attacking pieces, the number of
		// attacked and undefended squares around our king and the quality of
		// the pawn shelter (current 'score' value).
		attackUnits = std::min(72, ai.kingAttackersCount[JIJ] * ai.kingAttackersWeight[JIJ])
			+ 9 * ai.kingAdjacentZoneAttacksCount[JIJ]
			+ 27 * popcount(undefended)
			+ 11 * !!ai.gepend[IK]
			- 64 * !pos.aantal<DAME>(JIJ)
			- safety / 8;

		// Analyse the enemy's safe queen contact checks. Firstly, find the
		// undefended squares around the king reachable by the enemy queen...
		b = undefended & ai.aanval[JIJ][DAME] & ~pos.stukken(JIJ);
		if (b)
		{
			// ...and then remove squares not supported by another enemy piece
			b &= ai.aanval[JIJ][PION] | ai.aanval[JIJ][PAARD]
				| ai.aanval[JIJ][LOPER] | ai.aanval[JIJ][TOREN]
				| ai.aanval[JIJ][KONING];

			if (b)
				attackUnits += QueenContactCheck * popcount(b);
		}

		// Analyse the enemy's safe distance checks for sliders and knights
		safe = ~(ai.aanval[IK][ALLE_STUKKEN] | pos.stukken(JIJ));

		b1 = pos.aanval_van<TOREN  >(veldK) & safe;
		b2 = pos.aanval_van<LOPER>(veldK) & safe;

		// Enemy queen safe checks
		b = (b1 | b2) & ai.aanval[JIJ][DAME];
		if (b)
		{
			attackUnits += QueenCheck * popcount(b);
			score -= Checked;
		}

		// Enemy rooks safe checks
		b = b1 & ai.aanval[JIJ][TOREN];
		if (b)
		{
			attackUnits += RookCheck * popcount(b);
			score -= Checked;
		}

		// Enemy bishops safe checks
		b = b2 & ai.aanval[JIJ][LOPER];
		if (b)
		{
			attackUnits += BishopCheck * popcount(b);
			score -= Checked;
		}

		// Enemy knights safe checks
		b = pos.aanval_van<PAARD>(veldK) & ai.aanval[JIJ][PAARD] & safe;
		if (b)
		{
			attackUnits += KnightCheck * popcount(b);
			score -= Checked;
		}

		// Finally, extract the king danger score from the KingDanger[]
		// array and subtract the score from evaluation.
		score -= KingDanger[std::max(std::min(attackUnits, 399), 0)];
	}

	EvalWaarde mg = mg_waarde(score);
	EvalWaarde eg = eg_waarde(score);

	return remake_score(4 * 175 * (174 * mg + 12 * eg) / 186 / 256, 4 * 175 * (-26 * mg + 212 * eg) / 186 / 256);
}
#endif


template<Kleur IK>
inline Score SterkeVelden(const Stelling& pos, const AanvalInfo& ai, const Pionnen::Tuple *pionTuple)
{
	const Kleur JIJ = (IK == WIT ? ZWART : WIT);
	const Bitboard RIJ456 = (IK == WIT ? 0x3C3C3C000000 : 0x3C3C3C0000);

	Bitboard bb_veilig_voor_ZPion = ~pionTuple->pion_aanval_bereik(JIJ);
	Bitboard sterkePL = bb_veilig_voor_ZPion & ai.aanval[IK][PION] & pos.stukken(IK, PAARD, LOPER) & RIJ456;
	Score score = Score_SterkVeld_PL[popcount(sterkePL)];
	if (sterkePL && !pos.stukken(JIJ, PAARD))
	{
		do
		{
			Veld veld = pop_lsb(&sterkePL);
			Bitboard velden_zelfde_kleur = (DonkereVelden & veld) ? DonkereVelden : ~DonkereVelden;
			if (!(pos.stukken(JIJ, LOPER) & velden_zelfde_kleur))
				score += maak_score(156, 74);
		} while (sterkePL);
	}
	score += Score_VeiligVoorPion_TLP[popcount(bb_veilig_voor_ZPion & pos.stukken(IK, PAARD, LOPER, TOREN))];
	score += Score_SterkP_voor_Pion[popcount(bb_veilig_voor_ZPion & pos.stukken(IK, PAARD) & shift_down<IK>(pos.stukken(JIJ, PION)))];

	// P en L achter eigen pion
	score += Score_PL_achter_Pion[popcount(pos.stukken(IK, PAARD) & pion_aanval<JIJ>(pos.stukken(IK, PION)))];
	score += Score_PL_achter_Pion[popcount((pos.stukken(IK, PAARD, LOPER)) & shift_down<IK>(pos.stukken(IK, PION)))];
	score += Score_Ondersteunde_Stukken[popcount((pos.stukken(IK) ^ pos.stukken(IK, PION)) & ai.aanval[IK][PION])];

	return score;
}


template<Kleur IK>
inline void eval_init(const Stelling& pos, AanvalInfo& ai, const Pionnen::Tuple *pionTuple)
{
	const Kleur JIJ = (IK == WIT ? ZWART : WIT);

	ai.aanval[IK][KONING] = pos.aanval_van<KONING>(pos.veld<KONING>(IK));
	ai.aanval[IK][PION] = pionTuple->pion_aanval(IK);
	ai.aanval[IK][PAARD] = 0;
	ai.aanval[IK][LOPER] = 0;
	ai.aanval[IK][TOREN] = 0;
	ai.aanval[IK][DAME] = 0;

	ai.gepend[IK] = pos.info()->xRay[IK];

#ifdef KAANVAL
	ai.KAanvalScore[IK] = 0;
#endif
}


template<Kleur IK>
inline Score eval_Paarden(const Stelling& pos, AanvalInfo& ai)
{
	const Kleur JIJ = (IK == WIT ? ZWART : WIT);
	Score score = SCORE_ZERO;

	Bitboard velden = pos.stukken(IK, PAARD);
	assert(velden);
	do
	{
		Veld veld = pop_lsb(&velden);
		score += Score_Afstand_P_K[afstand(veld, pos.veld<KONING>(IK))];
		Bitboard aanval = pos.aanval_van<PAARD>(veld);
#ifdef KAANVAL
		if (aanval & ai.aanval[JIJ][KONING])
			ai.KAanvalScore[IK] += 3 * 8;
#endif
#ifdef SFAANVAL
		if (aanval & ai.kingRing[JIJ])
		{
			ai.kingAttackersCount[IK]++;
			ai.kingAttackersWeight[IK] += KingAttackWeights[PAARD];
			Bitboard bb = aanval & ai.aanval[JIJ][KONING];
			if (bb)
				ai.kingAdjacentZoneAttacksCount[IK] += popcount(bb);
		}
#endif
		ai.aanval[IK][PAARD] |= aanval;

		int mobiliteit;
		if (ai.gepend[IK] & veld)
			mobiliteit = 0;
		else
		{
			aanval &= ai.mobiliteit_mask[IK];
			// voorwaartse mobiliteit telt dubbel
			mobiliteit = popcount(aanval) + popcount(aanval & bb_rijen_voorwaarts(IK, veld));
		}
		score += mobi_P[(mobiliteit * MobiMult_P[relatief_veld(IK, veld)] + 16) / 32];
	} while (velden);

	return score;
}


template<Kleur IK>
inline Score eval_Lopers(const Stelling& pos, AanvalInfo& ai, const Pionnen::Tuple *pionTuple)
{
	const Kleur JIJ = (IK == WIT ? ZWART : WIT);
	Score score = SCORE_ZERO;

	Bitboard velden = pos.stukken(IK, LOPER);
	assert(velden);

	if (shift_up<IK>(pos.stukken(IK, KONING)) & velden)
		score += maak_score(73, 0);
	if (IK == WIT)
	{
		if (velden & bb2(SQ_A1, SQ_H1))
		{
			if ((pos.stukken(IK) & (velden << 9)) & SQ_B2)
				score -= maak_score(63, 96);
			if ((pos.stukken(IK) & (velden << 7)) & SQ_G2)
				score -= maak_score(63, 96);
		}
	}
	else
	{
		if (velden & bb2(SQ_A8, SQ_H8))
		{
			if ((pos.stukken(IK) & (velden >> 7)) & SQ_B7)
				score -= maak_score(63, 96);
			if ((pos.stukken(IK) & (velden >> 9)) & SQ_G7)
				score -= maak_score(63, 96);
		}
	}

	do
	{
		Veld veld = pop_lsb(&velden);
		Bitboard aanval = aanval_bb<LOPER>(veld, pos.stukken(PION));
		score += mobi_L1[(popcount(aanval) * MobiMult_L1[relatief_veld(IK, veld)] + 16) / 32];
		//score += mobi_L1[std::min((int)(popcount(aanval) * MobiMult_L1[relatief_veld(Us, veld)] + 16) / 32, 255)];

		if (pos.stukken(PION) & opgesloten_loper_b3_c2[IK][veld])
		{
			if (pos.stukken(PION) & opgesloten_loper_b3_c2_extra[veld])
				score -= maak_score(665, 665);
			else
				score -= maak_score(315, 315);
		}
#if 0
		else if (opgesloten_loper_b4_d2[IK][veld] && (pos.stukken(PION) & opgesloten_loper_b4_d2[IK][veld]) == opgesloten_loper_b4_d2[IK][veld])
		{
			if (pos.stukken(PION) & opgesloten_loper_b4_d2_extra[veld])
				score -= maak_score(600, 600);
			else
				score -= maak_score(300, 300);
		}
#endif
#if 0
		else
		{
			aanval &= ~(ai.aanval[JIJ][PION] | pos.stukken(IK, PION));
			if (!meer_dan_een(aanval))
			{
				if (aanval == 0)
					score -= maak_score(250, 350);
				else
					score -= maak_score(80, 135);
			}
		}
#endif

		aanval = pos.aanval_van<LOPER>(veld);
		//aanval = aanval_bb<LOPER>(veld, pos.stukken() ^ pos.stukken(Us, DAME));
#ifdef KAANVAL
		if (aanval & ai.aanval[JIJ][KONING])
			ai.KAanvalScore[IK] += 8;
#endif
#ifdef SFAANVAL
		if (aanval & ai.kingRing[JIJ])
		{
			ai.kingAttackersCount[IK]++;
			ai.kingAttackersWeight[IK] += KingAttackWeights[LOPER];
			Bitboard bb = aanval & ai.aanval[JIJ][KONING];
			if (bb)
				ai.kingAdjacentZoneAttacksCount[IK] += popcount(bb);
		}
#endif
		ai.aanval[IK][LOPER] |= aanval;

		aanval &= ai.mobiliteit_mask[IK];
		if (ai.gepend[IK] & veld)
			aanval &= bb_tussen(pos.veld<KONING>(IK), veld);
		score += mobi_L2[(popcount(aanval) * MobiMult_L2[relatief_veld(IK, veld)] + 16) / 32];

		Score pionnenOpKleur = Score_PionOpKleurLoper[pionTuple->pionnen_op_kleur(IK, veld)];
		score += pionnenOpKleur;
		score += Score_PionAndereKleurLoper[pionTuple->pionnen_niet_op_kleur(IK, veld)];

		// als ik loperpaar heb en jij hebt geen loper op dezelfde kleur
		// na ruil van lopers blijf ik zitten met slechte loper
		Bitboard velden_zelfde_kleur = (DonkereVelden & veld) ? DonkereVelden : ~DonkereVelden;
		if ((pos.stukken(IK, LOPER) & ~velden_zelfde_kleur) && !(velden_zelfde_kleur & pos.stukken(JIJ, LOPER)))
			score += pionnenOpKleur;

		if (pos.stukken(JIJ, PAARD) & bb_L_domineert_P[IK][veld])
			// extra score voor vijandelijk Paard dat 3 rijen voor de loper staat (wordt gedomineerd door Loper)
			score += maak_score(20, 19);
	} while (velden);

	return score;
}


template<Kleur IK>
inline Score eval_Torens(const Stelling& pos, AanvalInfo& ai)
{
	const Kleur JIJ = (IK == WIT ? ZWART : WIT);
	Score score = SCORE_ZERO;
	Bitboard velden = pos.stukken(IK, TOREN);
	assert(velden);

	// T opgesloten naast K
#if 1
	if (pos.stukken(IK, KONING) & (IK == WIT ? bb2(SQ_F1, SQ_G1) : bb2(SQ_F8, SQ_G8)) && velden & (IK == WIT ? 0xC0C0 : 0xC0C0000000000000))
		score -= maak_score(282, 101);
#else
	Bitboard bbx = opgesloten_toren[pos.veld<KONING>(IK)] & velden;
	if (bbx)
	{
		Veld veld = backmost_sq(IK, bbx);
		bbx = bb_voorwaarts(IK, veld) & pos.stukken(IK, PION);
		if (bbx)
		{
			veld = backmost_sq(IK, bbx);
			Rij r = relatieve_rij(IK, veld);
			if (!pos.rokade_mogelijk(IK))  // test nodig voor Chess960
				score -= maak_score(110 * (6 - r), 0);
			else
				score -= maak_score(100 + 20 * (6 - r), 0);
		}
	}
#endif

	do
	{
		Veld veld = pop_lsb(&velden);
		Bitboard aanval = pos.aanval_van<TOREN>(veld);
		//Bitboard aanval = aanval_bb<TOREN>(veld, pos.stukken() ^ pos.stukken(Us, TOREN, DAME));
#ifdef KAANVAL
		if (aanval & ai.aanval[JIJ][KONING])
			ai.KAanvalScore[IK] += 8;
#endif
#ifdef SFAANVAL
		if (aanval & ai.kingRing[JIJ])
		{
			ai.kingAttackersCount[IK]++;
			ai.kingAttackersWeight[IK] += KingAttackWeights[TOREN];
			Bitboard bb = aanval & ai.aanval[JIJ][KONING];
			if (bb)
				ai.kingAdjacentZoneAttacksCount[IK] += popcount(bb);
		}
#endif
		ai.aanval[IK][TOREN] |= aanval;

		aanval &= ai.mobiliteit_mask[IK];
		if (ai.gepend[IK] & veld)
			aanval &= bb_tussen(pos.veld<KONING>(IK), veld);
		score += mobi_T[(popcount(aanval) * MobiMult_T[relatief_veld(IK, veld)] + 16) / 32];

		// andere toren op zelfde lijn of rij (verbonden)
		if (pos.stukken(IK, TOREN) & LegeAanval[TOREN][veld])
			score += maak_score(24, 7);
		// koning op achtste rij, T op zevende
		const Bitboard ACHTSTE_RIJ = IK == WIT ? Rank8BB : Rank1BB;
		if (relatieve_rij(IK, veld) == RIJ_7 && pos.stukken(JIJ, KONING) & ACHTSTE_RIJ)
			score += maak_score(64, 163);
#ifdef KAANVAL
		// T op lijn K, als er geen eigen pionnen tussen staan
		if (lijn(veld) == lijn(pos.veld<KONING>(JIJ)) && !(pos.stukken(IK, PION) & bb_tussen(pos.veld<KONING>(JIJ), veld)))
			ai.KAanvalScore[IK] += 2 * 8;
#endif

		if (!(bb_lijn(veld) & pos.stukken(IK, PION)))
		{
			Bitboard pion = pos.stukken(JIJ, PION) & bb_lijn(veld);
			if (!pion)
				// open lijn
				score += maak_score(185, 157);
			else if (pion & ai.aanval[JIJ][PION])
				// halfopen verdedigd door pion
				score += maak_score(20, 44);
			else
				// halfopen lijn
				score += maak_score(112, 94);
		}
	} while (velden);

	return score;
}


template<Kleur IK>
inline Score eval_Dames(const Stelling& pos, AanvalInfo& ai)
{
	const Kleur JIJ = (IK == WIT ? ZWART : WIT);
	Score score = SCORE_ZERO;

	Bitboard velden = pos.stukken(IK, DAME);
	assert(velden);

	Bitboard mobiliteit_mask_D = ~(ai.aanval[JIJ][LOPER] | ai.aanval[JIJ][TOREN] | pos.stukken(IK, KONING, PION) | ai.aanval[JIJ][PION] | ai.aanval[JIJ][PAARD]);
#ifdef KAANVAL
	ai.KAanvalScore[IK] += 3 * 8;
#endif

	do
	{
		Veld veld = pop_lsb(&velden);

		Bitboard aanval = pos.aanval_van<DAME>(veld);
#ifdef KAANVAL
		if (aanval & ai.aanval[JIJ][KONING])
			ai.KAanvalScore[IK] += 8;
#endif
#ifdef SFAANVAL
		if (aanval & ai.kingRing[JIJ])
		{
			ai.kingAttackersCount[IK]++;
			ai.kingAttackersWeight[IK] += KingAttackWeights[DAME];
			Bitboard bb = aanval & ai.aanval[JIJ][KONING];
			if (bb)
				ai.kingAdjacentZoneAttacksCount[IK] += popcount(bb);
		}
#endif
		ai.aanval[IK][DAME] |= aanval;

		aanval &= mobiliteit_mask_D;
		if (ai.gepend[IK] & veld)
			aanval &= bb_tussen(pos.veld<KONING>(IK), veld);

		const Bitboard CENTRUM_VIERKANT = 0x00003C3C3C3C0000;
		score += mobi_D[((popcount(aanval) + popcount(aanval & CENTRUM_VIERKANT)) * MobiMult_D[relatief_veld(IK, veld)] + 32) / 64];
	} while (velden);

	return score;
}


/// evaluate() is the main evaluation function. It returns a static evaluation
/// of the position from the point of view of the side to move.

Waarde Eval::evaluate(const Stelling& pos, Waarde alfa, Waarde beta)
{
	assert(!pos.schaak_gevers());

	Materiaal::Tuple* materiaalTuple;
	Pionnen::Tuple* pionTuple;

	// Probe the material hash table
	materiaalTuple = Materiaal::probe(pos);

	// If we have a specialized evaluation function for the current material
	// configuration, call it and return.
	if (materiaalTuple->heeft_waarde_functie())
		return materiaalTuple->waarde_uit_functie(pos);

	StellingInfo* st = pos.info();

	bool doLazyEval = beta < WINST_WAARDE && (st - 1)->evalPositioneel != GEEN_EVAL && alfa > -WINST_WAARDE
		&& pos.niet_pion_materiaal(WIT) + pos.niet_pion_materiaal(ZWART) > 2 * MATL_LOPER
		&& !(pos.stukken(WIT, PION) & Rank7BB) && !(pos.stukken(ZWART, PION) & Rank2BB);

	if (doLazyEval)
	{
		EvalWaarde v = (st - 1)->evalPositioneel;
		int evalFactor = (st - 1)->evalFactor;
#if 0
		// laat meerdere lazyEvals achter elkaar toe
		st->evalPositioneel = v;
		st->evalFactor = evalFactor;
#endif
		v += materiaalTuple->waarde * evalFactor / MAX_FACTOR;

		if (pos.aan_zet() == ZWART)
			v = -v;

		//Waarde lazyResult = ValueFromEval(v) + Waarde((unsigned int)WAARDE_TEMPO * (unsigned int)evalFactor / MAX_FACTOR);
		Waarde lazyResult = ValueFromEval(v) + WAARDE_TEMPO;

		//dbg_hit_on(1, lazyResult <= alfa || lazyResult >= beta);
		if (lazyResult <= alfa || lazyResult >= beta)
			return lazyResult;
	}

	// Probe the pawn hash table
	pionTuple = Pionnen::probe(pos);

	Score koningVeiligheid = pionTuple->koning_veiligheid<WIT>(pos);
	koningVeiligheid -= pionTuple->koning_veiligheid<ZWART>(pos);


	// evaluatie stukken begint hier
	// =============================
	AanvalInfo ai;
	
	eval_init<WIT>(pos, ai, pionTuple);
	eval_init<ZWART>(pos, ai, pionTuple);

#ifdef SFAANVAL
	ai.kingAdjacentZoneAttacksCount[WIT] = ai.kingAdjacentZoneAttacksCount[ZWART] = 0;
	if (pos.niet_pion_materiaal(WIT) >= MATL_DAME)
	{
		ai.kingRing[ZWART] = ai.aanval[ZWART][KONING]; // | shift_bb<DELTA_S>(ai.attack[BLACK][KONING]);
		ai.kingAttackersCount[WIT] = popcount(ai.aanval[ZWART][KONING] & ai.aanval[WIT][PION]);
		ai.kingAttackersWeight[WIT] = ai.kingAttackersCount[WIT] * KingAttackWeights[PION];
	}
	else
		ai.kingRing[ZWART] = ai.kingAttackersCount[WIT] = ai.kingAttackersWeight[WIT] = 0;

	if (pos.niet_pion_materiaal(ZWART) >= MATL_DAME)
	{
		ai.kingRing[WIT] = ai.aanval[WIT][KONING]; // | shift_bb<DELTA_N>(ai.attack[WHITE][KONING]);
		ai.kingAttackersCount[ZWART] = popcount(ai.aanval[WIT][KONING] & ai.aanval[ZWART][PION]);
		ai.kingAttackersWeight[ZWART] = ai.kingAttackersCount[ZWART] * KingAttackWeights[PION];
	}
	else
		ai.kingRing[WIT] = ai.kingAttackersCount[ZWART] = ai.kingAttackersWeight[ZWART] = 0;
#endif

	ai.mobiliteit_mask[WIT] = ~(ai.aanval[ZWART][PION] | pos.stukken(WIT, PION) & shift_down<WIT>(pos.stukken()))
		| pos.pieces_exclude(ZWART, PION);
	ai.mobiliteit_mask[ZWART] = ~(ai.aanval[WIT][PION] | pos.stukken(ZWART, PION) & shift_down<ZWART>(pos.stukken()))
		| pos.pieces_exclude(WIT, PION);

	Score evalScore = pos.psq_score();

	// paarden
	if (pos.stukken(WIT, PAARD))
		evalScore += eval_Paarden<WIT>(pos, ai);
	if (pos.stukken(ZWART, PAARD))
		evalScore -= eval_Paarden<ZWART>(pos, ai);

	// lopers
	if (pos.stukken(WIT, LOPER))
		evalScore += eval_Lopers<WIT>(pos, ai, pionTuple);
	if (pos.stukken(ZWART, LOPER))
		evalScore -= eval_Lopers<ZWART>(pos, ai, pionTuple);

	// torens
	if (pos.stukken(WIT, TOREN))
		evalScore += eval_Torens<WIT>(pos, ai);
	if (pos.stukken(ZWART, TOREN))
		evalScore -= eval_Torens<ZWART>(pos, ai);
	
	// dames
	if (pos.stukken(WIT, DAME))
		evalScore += eval_Dames<WIT>(pos, ai);
	if (pos.stukken(ZWART, DAME))
		evalScore -= eval_Dames<ZWART>(pos, ai);

	ai.aanval[WIT][STUKKEN_ZONDER_KONING] = ai.aanval[WIT][PION] | ai.aanval[WIT][DAME] | ai.aanval[WIT][TOREN] | ai.aanval[WIT][LOPER] | ai.aanval[WIT][PAARD];
	ai.aanval[ZWART][STUKKEN_ZONDER_KONING] = ai.aanval[ZWART][PION] | ai.aanval[ZWART][DAME] | ai.aanval[ZWART][TOREN] | ai.aanval[ZWART][PAARD] | ai.aanval[ZWART][LOPER];
	ai.aanval[WIT][ALLE_STUKKEN] = ai.aanval[WIT][STUKKEN_ZONDER_KONING] | ai.aanval[WIT][KONING];
	ai.aanval[ZWART][ALLE_STUKKEN] = ai.aanval[ZWART][STUKKEN_ZONDER_KONING] | ai.aanval[ZWART][KONING];

	//evalScore += maak_score(-12, 0) * (popcount<Full>(ai.attack[WHITE][ALLE_STUKKEN]) - popcount<Full>(ai.attack[BLACK][ALLE_STUKKEN]));

	// score koningsaanval
	// ===================
#ifdef KAANVAL
	evalScore += KoningsAanval<WIT>(pos, ai, ai.KAanvalScore[WIT] - pionTuple->safety[ZWART]);
	evalScore -= KoningsAanval<ZWART>(pos, ai, ai.KAanvalScore[ZWART] - pionTuple->safety[WIT]);
#endif
#ifdef SFAANVAL
	evalScore += sf_KoningsAanval<WIT>(pos, ai, pionTuple->sf_safety[WIT]);
	evalScore -= sf_KoningsAanval<ZWART>(pos, ai, pionTuple->sf_safety[ZWART]);
#endif

	// dreigingen Wit
	// ==============
	evalScore += sf_Dreigingen<WIT>(pos, ai);
	evalScore -= sf_Dreigingen<ZWART>(pos, ai);

	// vrijpionnen
	// ===========
	if (pionTuple->vrijpionnen(WIT))
		evalScore += Vrijpionnen<WIT>(pos, ai, pionTuple->vrijpionnen(WIT));
	if (pionTuple->vrijpionnen(ZWART))
		evalScore -= Vrijpionnen<ZWART>(pos, ai, pionTuple->vrijpionnen(ZWART));

	// sterke velden voor P en L
	// =========================
	evalScore += SterkeVelden<WIT>(pos, ai, pionTuple);
	evalScore -= SterkeVelden<ZWART>(pos, ai, pionTuple);

	// geblokkeerde pionnen
	evalScore += Score_Geblokkeerde_Pionnen[popcount(pos.stukken(WIT, PION) & shift_down<WIT>(pos.stukken()))];
	evalScore -= Score_Geblokkeerde_Pionnen[popcount(pos.stukken(ZWART, PION) & shift_down<ZWART>(pos.stukken()))];

	// vastgelegde D-vleugelpionnen op beginveld
	Bitboard DVleugelPionnen_W = lijn(pos.veld<KONING>(WIT)) <= 3 ? 0x7800 : 0x1E00;
	evalScore -= Score_Pion_Vast_Dvleugel[popcount(pos.stukken(WIT, PION) & shift_down<WIT>(pos.stukken()) & DVleugelPionnen_W)];
	Bitboard DVleugelPionnen_Z = lijn(pos.veld<KONING>(ZWART)) <= 3 ? 0x78000000000000 : 0x1E000000000000; // d7 e7 f7 g7 : b7 c7 d7 e7
	evalScore += Score_Pion_Vast_Dvleugel[popcount(pos.stukken(ZWART, PION) & shift_down<ZWART>(pos.stukken()) & DVleugelPionnen_Z)];

#if 0
	// beheersing aanvalsvelden
	Bitboard bb_centrum_pivot = bb_centrum_pivot_W[lijn(pos.veld<KONING>(WIT))] | bb_centrum_pivot_Z[lijn(pos.veld<KONING>(ZWART))];
	evalScore += popcount(bb_centrum_pivot & ai.aanval[WIT][STUKKEN_ZONDER_KONING]) * maak_score(20, 0);
	evalScore -= popcount(bb_centrum_pivot & ai.aanval[ZWART][STUKKEN_ZONDER_KONING]) * maak_score(20, 0);
#endif
#if 0
	Bitboard bb_flank = ai.aanval[WIT][ALLE_STUKKEN] & bb_koning_flank_W[lijn(pos.veld<KONING>(ZWART))] & ~ai.aanval[ZWART][KONING];
	evalScore += popcount(bb_flank) * maak_score(20, 0);
	bb_flank = ai.aanval[ZWART][ALLE_STUKKEN] & bb_koning_flank_Z[lijn(pos.veld<KONING>(WIT))] & ~ai.aanval[WIT][KONING];
	evalScore -= popcount(bb_flank) * maak_score(20, 0);
#endif

	//evalScore += (5 - 2 * pionTuple->lijn_breedte) * maak_score(0, 40) * (pos.count<PAARD>(WHITE) - pos.count<PAARD>(BLACK));

	// positionele eval wordt maal (140/128) gedaan
	int mul = Threads.tactischeModus ? 50 : 35;
	Score score = pionTuple->pionnen_score() + koningVeiligheid + remake_score(mg_waarde(evalScore) * mul / 32, eg_waarde(evalScore) * mul / 32);

	// in laat eindspel (zonder dames, hoogstens 2 torens)
	if (pos.niet_pion_materiaal(WIT) + pos.niet_pion_materiaal(ZWART) <= 4 * MATL_LOPER)
		score += Score_PionLijnBreedte[pionTuple->pion_bereik(WIT)] - Score_PionLijnBreedte[pionTuple->pion_bereik(ZWART)];

	// Interpolate between a middlegame and a (scaled by 'sf') endgame score
	EvalWaarde mg = (106 * mg_waarde(score) - 6 * eg_waarde(score)) / 100;
	EvalWaarde eg = (13 * mg_waarde(score) + 87 * eg_waarde(score)) / 100;

	// Evaluate scale factor for the winning side
	//SchaalFactor sf = bereken_schaalfactor(pos, materiaalTuple, materiaalTuple->waarde + eg_waarde(score));
	SchaalFactor sf = bereken_schaalfactor(pos, materiaalTuple, materiaalTuple->waarde + eg);
	SchaalFactor conversie = materiaalTuple->conversie;

	if (materiaalTuple->conversie_is_geschat && !pos.stukken(DAME) && !(pionTuple->vrijpionnen(WIT) | pionTuple->vrijpionnen(ZWART)))
		conversie = SchaalFactor(conversie * 115 / 128); // ongeveer 0.9
	if (pionTuple->conversie_moeilijk)
		conversie = SchaalFactor(conversie * 115 / 128); // ongeveer 0.9

	SchaalFactor evalFactor = sf == NORMAAL_FACTOR ? conversie : SchaalFactor(std::min((int)conversie, (int)2 * sf));

	int fase = materiaalTuple->partij_fase();
	EvalWaarde v = (mg * conversie / MAX_FACTOR * fase + eg * evalFactor / MAX_FACTOR * int(MIDDENSPEL_FASE - fase)) / int(MIDDENSPEL_FASE);
	pos.info()->evalPositioneel = v;
	pos.info()->evalFactor = evalFactor;
	v += materiaalTuple->waarde * evalFactor / MAX_FACTOR;

#if 1
	if (Threads.stukContempt)
	{
		const int ContemptHigh = 90 * 16;
		const int ContemptLow = 50 * 16;
		int contemptScore = 8 * Threads.stukContempt * pos.contempt_aantal(Threads.rootKleur) * evalFactor / MAX_FACTOR;
		int vv = v + (pos.aan_zet() == WIT ? 8 * WAARDE_TEMPO : -8 * WAARDE_TEMPO);
		if (Threads.rootKleur == WIT)
		{
			if (vv < ContemptLow)
				v += EvalWaarde(contemptScore);
			else if (vv < ContemptHigh)
				v += EvalWaarde(contemptScore * (ContemptHigh - vv) / (ContemptHigh - ContemptLow));
		}
		else
		{
			if (vv > -ContemptLow)
				v -= EvalWaarde(contemptScore);
			else if (vv > -ContemptHigh)
				v -= EvalWaarde(contemptScore * (ContemptHigh + vv) / (ContemptHigh - ContemptLow));
		}
	}
#endif

	if (pos.aan_zet() == ZWART)
		v = -v;

	// tot hier is de score in 1/1600 pion (EvalWaarde)
	// uiteindelijk resultaat in 1/200 pion (Value)
	//Waarde result = ValueFromEval(v) + Waarde((unsigned int)WAARDE_TEMPO * (unsigned int)evalFactor / MAX_FACTOR);
	Waarde result = ValueFromEval(v) + WAARDE_TEMPO;

	// 50 zet regel
	if (pos.vijftig_zetten_teller() > Threads.vijftigZettenAfstand)
		result = result * (5 * (2 * Threads.vijftigZettenAfstand - pos.vijftig_zetten_teller()) + 6) / 256;

	// test op pat als enkel K + pionnen
	if (!pos.niet_pion_materiaal(pos.aan_zet()))
	{
		if (pos.aan_zet() == WIT)
		{
			if ((pos.aanval_van<KONING>(pos.veld<KONING>(WIT)) & ~pos.stukken(WIT) & ~ai.aanval[ZWART][ALLE_STUKKEN]) == 0
				&& ((pos.stukken(WIT, PION) << 8) & ~pos.stukken()) == 0
				&& (((pos.stukken(WIT, PION) & FileABB) << 7) & pos.stukken(ZWART)) == 0
				&& (((pos.stukken(WIT, PION) & ~FileHBB) << 9) & pos.stukken(ZWART)) == 0)
				result = REMISE_WAARDE;
		}
		else
			if ((pos.aanval_van<KONING>(pos.veld<KONING>(ZWART)) & ~pos.stukken(ZWART) & ~ai.aanval[WIT][ALLE_STUKKEN]) == 0
				&& ((pos.stukken(ZWART, PION) >> 8) & ~pos.stukken()) == 0
				&& (((pos.stukken(ZWART, PION) & ~FileABB) >> 9) & pos.stukken(WIT)) == 0
				&& (((pos.stukken(ZWART, PION) & ~FileHBB) >> 7) & pos.stukken(WIT)) == 0)
				result = REMISE_WAARDE;
	}

	return result;
}


/// trace() is like evaluate(), but instead of returning a value, it returns
/// a string (suitable for outputting to stdout) that contains the detailed
/// descriptions and values of each evaluation term. Useful for debugging.
double to_cp(Waarde v) { return double(v) / WAARDE_PION; }

std::string Eval::trace(const Stelling& pos)
{
	Waarde v = evaluate(pos, GEEN_WAARDE, GEEN_WAARDE);
	v = pos.aan_zet() == WIT ? v : -v; // White's point of view

	std::stringstream ss;
	ss << std::showpos << std::fixed << std::setprecision(2);
	ss << "\nEvaluation: " << to_cp(v) << "\n";

	return ss.str();
}


/// init() computes evaluation weights, usually at startup

void Eval::init()
{
	// Mobiliteit multiplier
	const int MobiMultConst[64] = {
		 1, 1, 1, 1, 1, 1, 1, 1,
		 1, 1, 1, 1, 1, 1, 1, 1,
		 1, 1, 1, 1, 1, 1, 1, 1,
		 1, 1, 1, 1, 1, 1, 1, 1,
		 1, 1, 1, 1, 1, 1, 1, 1,
		 1, 1, 1, 1, 1, 1, 1, 1,
		 1, 1, 1, 1, 1, 1, 1, 1,
		 1, 1, 1, 1, 1, 1, 1, 1
	};
	const int MobiMultRankQuad[64] = {
		-9,-9,-9,-9,-9,-9,-9,-9,
		-3,-3,-3,-3,-3,-3,-3,-3,
		1, 1, 1, 1, 1, 1, 1, 1,
		3, 3, 3, 3, 3, 3, 3, 3,
		3, 3, 3, 3, 3, 3, 3, 3,
		1, 1, 1, 1, 1, 1, 1, 1,
		-3,-3,-3,-3,-3,-3,-3,-3,
		-9,-9,-9,-9,-9,-9,-9,-9
	};
	const int MobiMultFileQuad[64] = {
		-9,-3, 1, 3, 3, 1,-3,-9,
		-9,-3, 1, 3, 3, 1,-3,-9,
		-9,-3, 1, 3, 3, 1,-3,-9,
		-9,-3, 1, 3, 3, 1,-3,-9,
		-9,-3, 1, 3, 3, 1,-3,-9,
		-9,-3, 1, 3, 3, 1,-3,-9,
		-9,-3, 1, 3, 3, 1,-3,-9,
		-9,-3, 1, 3, 3, 1,-3,-9
	};
	const int MobiMultCenter[64] = {
		4, 3, 2, 1, 1, 2, 3, 4,
		3, 2, 1, 0, 0, 1, 2, 3,
		2, 1, 0,-1,-1, 0, 1, 2,
		1, 0,-1,-2,-2,-1, 0, 1,
		1, 0,-1,-2,-2,-1, 0, 1,
		2, 1, 0,-1,-1, 0, 1, 2,
		3, 2, 1, 0, 0, 1, 2, 3,
		4, 3, 2, 1, 1, 2, 3, 4
	};
	const int MobiMultRank[64] = {
		-3,-3,-3,-3,-3,-3,-3,-3,
		-2,-2,-2,-2,-2,-2,-2,-2,
		-1,-1,-1,-1,-1,-1,-1,-1,
		 0, 0, 0, 0, 0, 0, 0, 0,
		 1, 1, 1, 1, 1, 1, 1, 1,
		 2, 2, 2, 2, 2, 2, 2, 2,
		 3, 3, 3, 3, 3, 3, 3, 3,
		 4, 4, 4, 4, 4, 4, 4, 4
	};
	const int MobiMultRand[64] = {
		-3,-3,-3,-3,-3,-3,-3,-3,
		-3,-1,-1,-1,-1,-1,-1,-3,
		-3,-1, 1, 1, 1, 1,-1,-3,
		-3,-1, 1, 3, 3, 1,-1,-3,
		-3,-1, 1, 3, 3, 1,-1,-3,
		-3,-1, 1, 1, 1, 1,-1,-3,
		-3,-1,-1,-1,-1,-1,-1,-3,
		-3,-3,-3,-3,-3,-3,-3,-3
	};
	for (int n = 0; n < 64; n++)
	{
		MobiMult_P[n] = 270 * MobiMultConst[n] + (0 * MobiMultRankQuad[n] + 6 * MobiMultFileQuad[n]) / 2
			- 2 * MobiMultCenter[n] + 0 * MobiMultRank[n] + 5 * MobiMultRand[n];
		MobiMult_L1[n] = 256;
		//MobiMult_L1[n] = 271 * MobiMultConst[n] + (-5 * MobiMultRankQuad[n] + 0 * MobiMultFileQuad[n]) / 2
		//	+ 0 * MobiMultCenter[n] + 0 * MobiMultRank[n] + 0 * MobiMultRand[n];
		MobiMult_L2[n] = 249 * MobiMultConst[n] + (-8 * MobiMultRankQuad[n] - 3 * MobiMultFileQuad[n]) / 2
			+ 0 * MobiMultCenter[n] - 3 * MobiMultRank[n] - 4 * MobiMultRand[n];
		MobiMult_T[n] = 255 * MobiMultConst[n] + (1 * MobiMultRankQuad[n] - 5 * MobiMultFileQuad[n]) / 2
			- 6 * MobiMultCenter[n] - 1 * MobiMultRank[n] - 2 * MobiMultRand[n];
		MobiMult_D[n] = 272 * MobiMultConst[n] + (-2 * MobiMultRankQuad[n] + 4 * MobiMultFileQuad[n]) / 2
			+ 1 * MobiMultCenter[n] - 2 * MobiMultRank[n] - 8 * MobiMultRand[n];

		//int MobiMult = TUNE_1 * MobiMultConst[n] + (TUNE_2 * MobiMultRankQuad[n] + TUNE_3 * MobiMultFileQuad[n]) / 2
		//	+ TUNE_4 * MobiMultCenter[n] + TUNE_5 * MobiMultRank[n] + TUNE_6 * MobiMultRand[n];
		//MobiMult_L1[n] = std::max(std::min(MobiMult, 512), 64);
	}
	for (int n = 0; n < 256; n++)
	{
		double v38 = sqrt(0.125 * n + 1.5) - sqrt(1.5);
		mobi_P[n] = maak_score(std::lround(v38 * 207.32 - 417.0), std::lround(v38 * 252.68 - 509.0));
		mobi_T[n] = maak_score(std::lround(v38 * 125.90 - 190.0), std::lround(v38 * 218.96 - 331.0));
		mobi_L2[n] = maak_score(std::lround(v38 * 221.48 - 374.0), std::lround(v38 * 203.99 - 344.0));
		mobi_L1[n] = maak_score(std::lround(v38 * 92.43 - 171.0), std::lround(v38 * 104.75 - 194.0));
		v38 = sqrt(0.25 * n + 1.5) - sqrt(1.5);
		mobi_D[n] = maak_score(std::lround(v38 * 203.42 - 616.0), std::lround(v38 * 165.33 - 555.0));
	}

	for (int n = 0; n < 9; n++)
	{
		Score_Afstand_P_K[n] = maak_score(5 * afstand_P_K_tabel[n], 2 * afstand_P_K_tabel[n]);

		Score_PionOpKleurLoper[n] = maak_score(-17, -27) * (n - 2);
		Score_PionAndereKleurLoper[n] = maak_score(29, 11) * (n - 2);
		Score_Pion_Vast_Dvleugel[n] = maak_score(20, 3) * n;
		Score_Geblokkeerde_Pionnen[n] = maak_score(-43, -167) * n;
		Score_PionLijnBreedte[n] = maak_score(0, 2 * (n > 5 ? 9 * n - 36 : n * n - 16));
		Score_Vrijpion_verdedigd[n] = maak_score(13, 67) * (n - 1) * n;
		Score_Vrijpion_niet_verdedigd[n] = maak_score(10, 60) * (n - 1) * n;
		Score_Vrijpion_aangevallen[n] = maak_score(10, 36) * (n - 1) * n;
		Score_Vrijpion_vrije_doorgang[n] = maak_score(1, 3) * (n - 1) * n;
		Score_Vrijpion_niet_DvD[n] = maak_score(49 * (n - 1) * n, 34 * (n * n + 1));
		Score_Vrijpion_DvD[n] = maak_score(46 * (n - 1) * n, 43 * (n * n + 1));

		Score_PL_achter_Pion[n] = maak_score(32, 8) * n;
		Score_NietVerdedigdePionnen[n] = maak_score(-15, -30) * n;
		Score_Dreigingen[n] = maak_score(138, 123) * (n > 1 ? n + 2 : n);
		Score_StukkenMoetenVerdedigen[n] = maak_score(17, 42) * n;
		Score_Ondersteunde_Stukken[n] = maak_score(55, 29) * n;
		Score_SterkVeld_PL[n] = maak_score(62, 70) * n;
		Score_VeiligVoorPion_TLP[n] = maak_score(35, 27) * n;
		Score_SterkP_voor_Pion[n] = maak_score(14, 34) * n;
	}

	for (int progress = 0; progress < 6; progress++)
	{
		for (int afstand = 0; afstand < 8; afstand++)
		{
			int v49 = 30 * VrijpionNabijheid[afstand];
			double v50 = (sqrt(afstand + 1.0) - 1.0) * progress * (progress - 1);
			Score_Pion_W[progress][afstand] = maak_score(0, (v49 - int(v50 * 40.0 + 0.5)) * 32 / 35);
			Score_Pion_AA[progress][afstand] = maak_score(0, (int(v50 * 76.0 + 0.5) - v49) * 32 / 35);
		}
	}

#if 1
	const int UCI_KingSafety = 65;
	int vprev;
	for (int n = 0; n < 128; n++) {
		int v = UCI_KingSafety * KoningGevaar[n] / 10;   // default score 65
		VeiligheidTabel[8 * n] = maak_score(v, 0);
		if (n > 0)
			for (int i = 1; i < 8; i++)
				VeiligheidTabel[8 * n - 8 + i] = maak_score((i * v + (8 - i) * vprev) / 8, 0);
		vprev = v;
	}
#else
	const double MaxHelling = 25 * 6.5 / 8 * TUNE_1 / 256;
	const double InflectionPoint = 29 * 8 * TUNE_2 / 256;
	const int MaxWaarde = std::lround(1400 * 6.5);
	for (int n = 0; n < 1024; n++) {
		int waarde;
		if (n > InflectionPoint)
			waarde = (int)VeiligheidTabel[n - 1] + std::lround(MaxHelling);
		else {
			const double d2 = 3.1416 / 4 / InflectionPoint;
			waarde = std::lround(d2 * MaxHelling * pow(sin(d2 * n) / d2, 2));
		}
		VeiligheidTabel[n] = maak_score(std::min(MaxWaarde, waarde), 0);
	}
#endif

#ifdef SFAANVAL
	const int MaxSlope = 8700; // --> actief vanaf i = 162, dan is KingDanger ongeveer 3 pionnen...
	const int Peak = 1280000;
	int t = 0;

	for (int i = 0; i < 400; ++i)
	{
		t = std::min(Peak, std::min(i * i * 27, t + MaxSlope));
		KingDanger[i] = maak_score(t * 330 / (256 * 1000), 0);
	}
#endif

	//for (File f = LIJN_A; f <= LIJN_H; ++f)
	//{
	//	bb_centrum_pivot_W[f] = bb4(SQ_D4, SQ_D5, SQ_E4, SQ_E5)
	//		| (f >= LIJN_E ? bb3(SQ_F4, SQ_E3, SQ_F3) : bb3(SQ_C4, SQ_D3, SQ_C3));
	//	bb_centrum_pivot_Z[f] = bb4(SQ_D4, SQ_D5, SQ_E4, SQ_E5)
	//		| (f >= LIJN_E ? bb3(SQ_F5, SQ_E6, SQ_F6) : bb3(SQ_C5, SQ_D6, SQ_C6));
	//}
}

void Eval::init_tune()
{
	Eval::init();
	Pionnen::init();
	PSQT::init();
	Search::init();
}
