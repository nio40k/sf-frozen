/*
  Stockfish, a UCI chess playing engine derived from Glaurung 2.1
  Copyright (C) 2004-2008 Tord Romstad (Glaurung author)
  Copyright (C) 2008-2015 Marco Costalba, Joona Kiiski, Tord Romstad

  Stockfish is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  Stockfish is distributed in the hope that it will be useful,
  but WHITEHOUT ANY WARRANTY; WHITEhout even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along WHITEh this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#include <algorithm>
#include <cassert>

#include "bitboard.h"
#include "pawns.h"
#include "position.h"
#include "thread.h"
#include "uci.h"


namespace {

	Bitboard LijnAchter(Bitboard x) {
		x = x | (x >> 8);
		x = x | (x >> 16);
		x = x | (x >> 32);
		return x;
	}

	Bitboard LijnVoor(Bitboard x) {
		x = x | (x << 8);
		x = x | (x << 16);
		x = x | (x << 32);
		return x;
	}

	Bitboard LijnVoorAchter(Bitboard x) {
		x = x | (x >> 8) | (x << 8);
		x = x | (x >> 16) | (x << 16);
		x = x | (x >> 32) | (x << 32);
		return x;
	}

	static const Bitboard CENTRUM_VIERKANT = 0x00003C3C3C3C0000;
	static const Bitboard RAND_BORD = 0xFFFFC3C3C3C3FFFF;
	static const Bitboard LIJN_AH = 0x8181818181818181;
	static const Bitboard NIET_LIJN_AH = 0x7E7E7E7E7E7E7E7E;
	static const Bitboard NIET_LIJN_A = 0xFEFEFEFEFEFEFEFE;
	static const Bitboard NIET_LIJN_H = 0x7F7F7F7F7F7F7F7F;

	static const Bitboard RIJEN_1_TOT_4[2] = { 0x00000000FFFFFFFF, 0xFFFFFFFF00000000 };
	static const int PionSchild_Constanten[8] = { 0, 27, 22, 10, 5, 0, 0, 0 };
	static const int PionStorm_Constanten[8] = { -2, 0, 0, -6, -13, -40, 0, 0 };

#define S(mg, eg) maak_score(mg, eg) * 4 / 16
	static const Score VrijPionWaarden4[8] = {
		S(0, 0), S(28, 47), S(28, 47), S(127, 188),
		S(325, 470), S(622, 893), S(1018, 1457), S(0, 0)
	};
#undef S

	Score Score_K_Pion_afstand[17][8];
	EvalWaarde Score_PionSchild[8];
	EvalWaarde Score_PionBestorming[8];
	EvalWaarde Score_StormHalfOpenLijn[8];  // Score_PionBestorming + 1/8
	EvalWaarde Score_Aanvalslijnen[8];

	static const Bitboard AanvalsLijnen[LIJN_N] = {
		FileABB | FileBBB | FileCBB, 
		FileABB | FileBBB | FileCBB, 
		FileBBB | FileCBB | FileDBB,
		FileCBB | FileDBB | FileEBB, 
		FileDBB | FileEBB | FileFBB, 
		FileEBB | FileFBB | FileGBB,
		FileFBB | FileGBB | FileHBB,
		FileFBB | FileGBB | FileHBB
	};

	template <Kleur IK>
	EvalWaarde veiligheid_op_veld(const Stelling& pos, const Pionnen::Tuple* e, Veld veldK)
	{
		const Kleur JIJ = IK == WIT ? ZWART : WIT;
		EvalWaarde result = EVAL_0;

		// pionnenschild voor koning
		Bitboard UsPawns = pos.stukken(IK, PION);
		Bitboard lijnen_rond_K = AanvalsLijnen[lijn(veldK)];

		Bitboard bb_schild = UsPawns & lijnen_rond_K & ~bb_rijen_voorwaarts(JIJ, rij(veldK));
		while (bb_schild)
		{
			Veld veld = pop_lsb(&bb_schild);
			EvalWaarde v = Score_PionSchild[relatieve_rij(IK, veld)];
			if (lijn(veld) == lijn(veldK))
				result += v;
			result += v;
		}

		// bestorming, het veld voor de koning telt niet mee (hier dient de aanvallende pion als schild!)
		Bitboard bb_storm = pos.stukken(JIJ, PION) & lijnen_rond_K;
		//Bitboard bb_storm = pos.stukken(JIJ, PION) & lijnen_rond_K & ~shift_up<Us>(pos.stukken(Us, KONING));
		while (bb_storm)
		{
			Veld veld = pop_lsb(&bb_storm);
			if (bb_voorwaarts(JIJ, veld) & UsPawns)
				result += Score_PionBestorming[relatieve_rij(JIJ, veld)];
			else
				result += Score_StormHalfOpenLijn[relatieve_rij(JIJ, veld)];
		}
		// aantal halfopen lijnen voor aanval
		result += Score_Aanvalslijnen[popcount(lijnen_rond_K & e->halfOpenLijnen[JIJ])];

		return result;
	}

#if 0
	template <Kleur IK>
	Score evaluate(const Stelling& pos, Pionnen::Tuple* e, const Bitboard bb_pionlijnen[2])
	{
		const Kleur JIJ = (IK == WIT ? ZWART : WIT);

		const Bitboard bb_mijnPion = pos.stukken(IK, PION);
		const Bitboard bb_jouwPion = pos.stukken(JIJ, PION);
		const Bitboard RIJ56 = IK == WIT ? 0x0000FFFF00000000 : 0x00000000FFFF0000;
		const Bitboard TweedeRij = IK == WIT ? 0x000000000000FF00 : 0x00FF000000000000;
		const Veld Up = IK == WIT ? BOVEN : ONDER;

		Score result;

		e->pionnenOpKleur[IK][ZWART] = popcount(bb_mijnPion & DonkereVelden);
		e->pionnenOpKleur[IK][WIT] = pos.aantal<PION>(IK) - e->pionnenOpKleur[IK][ZWART];
		e->halfOpenLijnen[IK] = (int)~bb_pionlijnen[IK] & 0xFF;
		Bitboard b = e->halfOpenLijnen[IK] ^ 0xFF;
		e->pionBereik[IK] = b ? int(msb(b) - lsb(b)) : 0;

		Bitboard BuurVelden = ((bb_mijnPion << 1) & NIET_LIJN_A) | ((bb_mijnPion >> 1) & NIET_LIJN_H);
		Bitboard GeisoleerdeLijnen = ~((bb_pionlijnen[IK] << 1) & NIET_LIJN_A | (bb_pionlijnen[IK] >> 1) & NIET_LIJN_H);

		// pionaanval op rij 5 of 6
		result = maak_score(89, 78) * popcount(bb_mijnPion & RIJ56 & e->pionAanval[JIJ]);

		// KwetsbaarVoorToren = aangevallen door vijandelijke toren op de 8ste rij
		Bitboard KwetsbaarVoorToren = IK == WIT ? ~LijnAchter(bb_jouwPion | (bb_mijnPion >> 8)) : ~LijnVoor(bb_jouwPion | (bb_mijnPion << 8));

		// dubbelpionnen (voorste op de lijn)
		Bitboard bb_dubbelpionnen = IK == WIT ? bb_mijnPion & ~LijnAchter(bb_mijnPion >> 8) & LijnVoor(bb_mijnPion << 8) :
			bb_mijnPion & ~LijnVoor(bb_mijnPion << 8) & LijnAchter(bb_mijnPion >> 8);

		result += (popcount(bb_mijnPion) - popcount(bb_pionlijnen[IK] & 0xFF)) * maak_score(-42, -65);    // dubbelpionnen
		result += popcount(bb_dubbelpionnen & ~e->pionAanval[IK]) * maak_score(-36, -50); // niet gedekte dubbelpionnen

		if (bb_mijnPion & GeisoleerdeLijnen)
		{
			result += popcount(bb_mijnPion & GeisoleerdeLijnen & ~bb_pionlijnen[JIJ] & NIET_LIJN_AH) * maak_score(-121, -136);  // geisoleerde pion op halfopen lijn, niet aan de rand
			result += popcount(bb_mijnPion & GeisoleerdeLijnen & ~bb_pionlijnen[JIJ] & LIJN_AH) * maak_score(-75, -87);  // geisoleerde randpion op halfopen lijn
			result += popcount(bb_mijnPion & GeisoleerdeLijnen & bb_pionlijnen[JIJ] & NIET_LIJN_AH) * maak_score(-34, -40);  // geisoleerde pion niet op halfopen lijn, niet aan de rand
			result += popcount(bb_mijnPion & GeisoleerdeLijnen & bb_pionlijnen[JIJ] & LIJN_AH) * maak_score(-31, -26);  // geisoleerde randpion niet op halfopen lijn
		}

		if (bb_dubbelpionnen & GeisoleerdeLijnen)
		{
			result += popcount(bb_dubbelpionnen & GeisoleerdeLijnen & ~bb_pionlijnen[JIJ] & NIET_LIJN_AH) * maak_score(-24, -47);  // geisoleerde dubbelpion op halfopen lijn, niet aan de rand
			result += popcount(bb_dubbelpionnen & GeisoleerdeLijnen & ~bb_pionlijnen[JIJ] & LIJN_AH) * maak_score(-18, -42);  // geisoleerde dubbelpion op halfopen lijn, aan de rand
			result += popcount(bb_dubbelpionnen & GeisoleerdeLijnen & bb_pionlijnen[JIJ] & NIET_LIJN_AH) * maak_score(-14, -8);  // geisoleerde dubbelpion niet op halfopen lijn, niet aan de rand
			result += popcount(bb_dubbelpionnen & GeisoleerdeLijnen & bb_pionlijnen[JIJ] & LIJN_AH) * maak_score(-10, -5);  // geisoleerde dubbelpion niet op halfopen lijn, aan de rand
		}

		result += popcount(e->pionAanval[IK] & CENTRUM_VIERKANT) * maak_score(9, 11);  // controle van centrum
		result += popcount(e->pionAanval[IK] & RAND_BORD) * maak_score(6, 8);  // controle van rest van het bord

		result += popcount(bb_mijnPion & BuurVelden & e->pionAanval[IK]) * maak_score(2, -5);  // pion ernaast en verdedigd
		result += popcount(bb_mijnPion & e->pionAanval[IK]) * maak_score(48, 26);  // pion verdedigd
		result += popcount(bb_mijnPion & BuurVelden) * maak_score(22, 23);  // pion ernaast


		// het volgende lijkt heel bizar - houdt er ook geen rekening mee of de hangende pion gedekt is of niet
		// de versie hieronder rekent trouwens enkel randpionnen in (door de vermoedelijk ontbrekende code)
		/*
		Bitboard bb_hangend = RIJEN_1_TOT_4[Us] & bb_mijnPion & BuurVelden & ~bb_pionlijnen[JIJ];// hangende pionnen op halfopen lijnen op rij 1 tot 4
		while (bb_hangend)
		{
			Square veld = pop_lsb(&bb_hangend);

			Bitboard bb_hangende_pion = bbVeld[veld];
			Bitboard buren = (bb_hangende_pion >> 1) & NIET_LIJN_H | (bb_hangende_pion << 1) & NIET_LIJN_A;
			// hier ontbreekt de volgende lijn ?!?
			//buren &= bb_mijnPion;
			if (buren && !meer_dan_een(buren))  // enkel als popcount == 1, dus een pion naast één enkele andere pion
			{
				Bitboard blokkade = Us == WHITE ? bb_jouwPion & (buren << 16) : bb_jouwPion & (buren >> 16);
				if (!blokkade || meer_dan_een(blokkade))  // 0 of 2 pionnen die opmars hangende pion stoppen
					result -= maak_score(1, 50);
				else // 1 pion stopt opmars hangende pion
					result += maak_score(-1, -25);
			}
		}
		*/

		// misschien toevoegen: een pion die een vijandelijke pion aanvalt (= door een vijandelijke pion aangevallen wordt) is niet achtergebleven
		Bitboard bb_falanx = e->pionAanval[IK] | BuurVelden;
		Bitboard bb_achtergebleven = bb_mijnPion & ~e->pionAanvalBereik[IK] & ~GeisoleerdeLijnen;
		Bitboard bb_keten = e->pionAanval[IK] | BuurVelden & ~bb_dubbelpionnen;  // laatste conditie is gek - voorste dubbelpion is achtergebleven zelfs als er een buur is?
		// misschien omdat pion niet langs achter kan gedekt worden, en de pion pushen na ruil een geïsoleerde dubbelpion oplevert
		Bitboard bb_echt_achtergebleven = bb_achtergebleven & ~bb_keten;
		Bitboard bb_velden_voor_achtergebleven = shift_bb<Up>(bb_echt_achtergebleven);

		result += popcount(bb_velden_voor_achtergebleven & KwetsbaarVoorToren & e->pionAanval[JIJ]) * maak_score(-55, -86);   // achtergebleven halfopen lijn, pionaanval stopt opmars
		result += popcount(bb_velden_voor_achtergebleven & (pos.stukken(PION) | e->pionAanval[JIJ]) & NIET_LIJN_AH) * maak_score(-30, -32);  // achtergebleven, pion of pionaanval, niet aan de rand
		result += popcount(bb_velden_voor_achtergebleven & (pos.stukken(PION) | e->pionAanval[JIJ]) & LIJN_AH) * maak_score(-26, -25);  // achtergebleven, pion of pionaanval, aan de rand

		Bitboard bb_begin_blocked = e->pionAanval[JIJ] & shift_bb<Up>(bb_mijnPion & TweedeRij);
		result += popcount(bb_begin_blocked) * maak_score(-11, -12); // geblokkeerde pionnen op beginpositie


		// vrijpionnen
		// ===========
		Bitboard bb_vrijpionnen = bb_mijnPion & ~e->pionAanvalBereik[JIJ] & KwetsbaarVoorToren;
		Bitboard bb_gedekt_vrijpion = bb_vrijpionnen & bb_falanx;
		while (bb_gedekt_vrijpion)
		{
			Veld veld = pop_lsb(&bb_gedekt_vrijpion);
			result += 4 * VrijPionWaarden[relatieve_rij(IK, veld)] / 16; // gedekte vrijpion
		}

		e->vrijPionnen[IK] = bb_vrijpionnen;
		if (meer_dan_een(bb_vrijpionnen))
			result += maak_score(33, 128);

		// potentiele vrijpionnen
		// voorste pionnen in falanx op halfopen lijn
		Bitboard voorste_pionnen = IK == WIT ? bb_mijnPion & ~LijnAchter(bb_mijnPion >> 8) : bb_mijnPion & ~LijnVoor(bb_mijnPion << 8);
		Bitboard bb_potentieel = voorste_pionnen & bb_falanx & KwetsbaarVoorToren & ~bb_vrijpionnen;
		while (bb_potentieel)
		{
			Veld veld = pop_lsb(&bb_potentieel);
			Bitboard bb_actief = voorste_pionnen; // toevoegen: & ~bb_echt_achtergebleven  // achtergebleven pionnen kunnen potentiële vrijpion niet helpen
			if (lijn(veld) != LIJN_A)
			{
				Veld veld_links = Veld(veld - 1);
				if (bbVeld[veld_links] & bb_jouwPion)
					bb_actief &= ~bb_lijn(veld_links);
			}
			if (lijn(veld) != LIJN_H)
			{
				Veld veld_rechts = Veld(veld + 1);
				if (bbVeld[veld_rechts] & bb_jouwPion)
					bb_actief &= ~bb_lijn(veld_rechts);
			}

			if (popcount(bb_aangrenzende_lijnen(lijn(veld)) & ~bb_rijen_voorwaarts(IK, rij(veld)) & bb_actief) >= 
				popcount(bb_aangrenzende_lijnen(lijn(veld)) & bb_rijen_voorwaarts(IK, rij(veld)) & bb_jouwPion))
				result += 11 * VrijPionWaarden[relatieve_rij(IK, veld)] / 16;  // potentiele vrijpionnen
		}

		/* contempt stuff
		if (UCI_Contempt > 0 && __popcnt(WPion | ZPion) > 14)
		pionScore[Root_Kleur] -= SCORE32(UCI_Contempt, 0);

		geblokkeerde_pionnen = __popcnt(ZPion & (WPion << 8)) - 1;
		if (geblokkeerde_pionnen > 0)
		pionScore[Root_Kleur] -= SCORE32(5 * UCI_Contempt * geblokkeerde_pionnen, 0);
		*/

		//entry->conversie_is_geschat = 1;

		return result;
	}

#define V Waarde
	// Weakness of our pawn shelter in front of the king by [distance from edge][rank]
	const Waarde ShelterWeakness[][RIJ_NB] = {
		{ V(97), V(21), V(26), V(51), V(87), V(89), V(99) },
		{ V(120), V(0), V(28), V(76), V(88), V(103), V(104) },
		{ V(101), V(7), V(54), V(78), V(77), V(92), V(101) },
		{ V(80), V(11), V(44), V(68), V(87), V(90), V(119) } };

	// Danger of enemy pawns moving toward our king by [type][distance from edge][rank]
	const Waarde StormDanger[][4][RIJ_NB] = {
		{ { V(0),  V(67), V(134), V(38), V(32) },
		{ V(0),  V(57), V(139), V(37), V(22) },
		{ V(0),  V(43), V(115), V(43), V(27) },
		{ V(0),  V(68), V(124), V(57), V(32) } },
		{ { V(20),  V(43), V(100), V(56), V(20) },
		{ V(23),  V(20), V(98), V(40), V(15) },
		{ V(23),  V(39), V(103), V(36), V(18) },
		{ V(28),  V(19), V(108), V(42), V(26) } },
		{ { V(0),  V(0), V(75), V(14), V(2) },
		{ V(0),  V(0), V(150), V(30), V(4) },
		{ V(0),  V(0), V(160), V(22), V(5) },
		{ V(0),  V(0), V(166), V(24), V(13) } },
		{ { V(0),  V(-283), V(-281), V(57), V(31) },
		{ V(0),  V(58), V(141), V(39), V(18) },
		{ V(0),  V(65), V(142), V(48), V(32) },
		{ V(0),  V(60), V(126), V(51), V(19) } } };

	// Max bonus for king safety. Corresponds to start position with all the pawns
	// in front of the king and no enemy pawn on the horizon.
	const Waarde MaxSafetyBonus = V(258);
#undef V
#endif

#define pawn_make_score(mg, eg) maak_score((87 * 177 * mg + 6 * 142 * eg) / (93 * 64), (-13 * 177 * mg + 106 * 142 * eg) / (93 * 64))
#define S(mg, eg) pawn_make_score(mg, eg)
	// Doubled pawn penalty by file
	const Score Doubled[LIJN_N] = {
		S(13, 43), S(20, 48), S(23, 48), S(23, 48),
		S(23, 48), S(23, 48), S(20, 48), S(13, 43) };

	// Isolated pawn penalty by opposed flag and file
	const Score Isolated[2][LIJN_N] = {
		{ S(37, 45), S(54, 52), S(60, 52), S(60, 52),
		S(60, 52), S(60, 52), S(54, 52), S(37, 45) },
		{ S(25, 30), S(36, 35), S(40, 35), S(40, 35),
		S(40, 35), S(40, 35), S(36, 35), S(25, 30) } };

	// Backward pawn penalty by opposed flag
	const Score Backward[2] = { S(67, 42), S(49, 24) };

	// Connected pawn bonus by opposed, phalanx, twice supported and rank
	Score Connected[2][2][2][RIJ_N];

	// Levers bonus by rank
	const Score Lever[RIJ_N] = {
		S(0, 0), S(0, 0), S(0, 0), S(0, 0),
		S(20,20), S(40,40), S(0, 0), S(0, 0) };
#undef S

	template<Kleur IK>
	Score sf_evaluate(const Stelling& pos, Pionnen::Tuple* e) {

		const Kleur JIJ = (IK == WIT ? ZWART : WIT);
		const Bitboard TweedeRij = IK == WIT ? 0x000000000000FF00 : 0x00FF000000000000;

		const Score UnsupportedPawnPenalty = pawn_make_score(20, 10);
		const Score CenterBind = pawn_make_score(16, 0);

		const Bitboard CenterBindMask =
			IK == WIT ? (FileDBB | FileEBB) & (Rank5BB | Rank6BB | Rank7BB)
			: (FileDBB | FileEBB) & (Rank4BB | Rank3BB | Rank2BB);

		Bitboard b, neighbours, doubled, supported, phalanx;
		Veld veld;
		bool passed, isolated, opposed, backward, lever, connected;
		Score score = SCORE_ZERO;
		//Score k_score = SCORE_ZERO;
		const Veld* pVeld = pos.veld_lijst<PION>(IK);
		const Bitboard* pawnAttacksBB = KorteAanval[maak_stuk(IK, PION)];

		Bitboard ourPawns = pos.stukken(IK, PION);
		Bitboard theirPawns = pos.stukken(JIJ, PION);

		e->vrijPionnen[IK] = 0;
		e->koningVeld[IK] = SQ_NONE;
		e->pionnenOpKleur[IK][ZWART] = popcount(ourPawns & DonkereVelden);
		e->pionnenOpKleur[IK][WIT] = pos.aantal<PION>(IK) - e->pionnenOpKleur[IK][ZWART];

		// Loop through all pawns of the current color and score each pawn
		while ((veld = *pVeld++) != SQ_NONE)
		{
			assert(pos.stuk_op_veld(veld) == maak_stuk(IK, PION));

			Lijn f = lijn(veld);

			// Flag the pawn
			neighbours = ourPawns & bb_aangrenzende_lijnen(f);
			doubled = ourPawns & bb_voorwaarts(IK, veld);
			opposed = theirPawns & bb_voorwaarts(IK, veld);
			passed = !(theirPawns & vrijpion_mask(IK, veld));
			lever = theirPawns & pawnAttacksBB[veld];
			phalanx = neighbours & bb_rij(veld);
			supported = neighbours & bb_rij(veld - pion_vooruit(IK));
			connected = supported | phalanx;
			isolated = !neighbours;

			// Test for backward pawn.
			// If the pawn is passed, isolated, lever or connected it cannot be
			// backward. If there are friendly pawns behind on adjacent files
			// or if it is sufficiently advanced, it cannot be backward either.
			if ((passed | isolated | lever | connected)
				|| (ourPawns & pion_aanval_bereik(JIJ, veld))
				|| (relatieve_rij(IK, veld) >= RIJ_5))
				backward = false;
			else
			{
				// We now know there are no friendly pawns beside or behind this
				// pawn on adjacent files. We now check whether the pawn is
				// backward by looking in the forward direction on the adjacent
				// files, and picking the closest pawn there.
				b = pion_aanval_bereik(IK, veld) & (ourPawns | theirPawns);
				b = pion_aanval_bereik(IK, veld) & bb_rij(backmost_sq(IK, b));

				// If we have an enemy pawn in the same or next rank, the pawn is
				// backward because it cannot advance without being captured.
				backward = (b | shift_up<IK>(b)) & theirPawns;
			}

			assert(opposed | passed | (pion_aanval_bereik(IK, veld) & theirPawns));

			// Passed pawns will be properly scored in evaluation because we need
			// full attack info to evaluate passed pawns. Only the frontmost passed
			// pawn on each file is considered a true passed pawn.
			if (passed && !doubled)
			{
				e->vrijPionnen[IK] |= veld;
				if (connected)
					score += VrijPionWaarden4[relatieve_rij(IK, veld)]; // gedekte vrijpion (k_score)
			}

			// Score this pawn
			if (isolated)
				score -= Isolated[opposed][f];

			else if (backward)
				score -= Backward[opposed];

			else if (!supported)
				score -= UnsupportedPawnPenalty;

			if (connected)
				score += Connected[opposed][!!phalanx][meer_dan_een(supported)][relatieve_rij(IK, veld)];

			if (doubled)
				score -= Doubled[f] / rij_afstand(veld, frontmost_sq(IK, doubled));

			if (lever)
				score += Lever[relatieve_rij(IK, veld)];
		}

		b = e->halfOpenLijnen[IK] ^ 0xFF;
		e->pionBereik[IK] = b ? int(msb(b) - lsb(b)) : 0;

		// Center binds: Two pawns controlling the same central square
		b = shift_upleft<IK>(ourPawns) & shift_upright<IK>(ourPawns) & CenterBindMask;
		score += popcount(b) * CenterBind;

		if (meer_dan_een(e->vrijPionnen[IK]))
			score += maak_score(33, 128); // (k_score)

		Bitboard bb_begin_blocked = e->pionAanval[JIJ] & shift_up<IK>(ourPawns & TweedeRij);
		score -= popcount(bb_begin_blocked) * maak_score(11, 12); // geblokkeerde pionnen op beginpositie (k_score)

		return score;
	}


} // namespace

namespace Pionnen {

/// Pawns::init() initializes some tables needed by evaluation. Instead of using
/// hard-coded tables, when makes sense, we prefer to calculate them WHITEh a formula
/// to reduce independent parameters and to allow easier tuning and better insight.

void init()
{
	for (int n = 0; n < 17; n++)
		for (int afstand = 0; afstand < 8; afstand++)
			Score_K_Pion_afstand[n][afstand] = maak_score(0, (signed int)floor(sqrt((double)n) * 5.0 * afstand));

	for (int n = 0; n < 8; n++)
	{
		Score_PionSchild[n] = EvalWaarde(8 * PionSchild_Constanten[n]);
		Score_PionBestorming[n] = EvalWaarde(8 * PionStorm_Constanten[n]);
		Score_StormHalfOpenLijn[n] = EvalWaarde(9 * PionStorm_Constanten[n]);  // Score_PionBestorming + 1/8
		Score_Aanvalslijnen[n] = n * Score_PionBestorming[0];
	}

	const int Seed[RIJ_N]        = { 0,  0, 15, 10, 57,  75, 135, 0 };
	const int phalanxSeed[RIJ_N] = { 0, 10, 13, 33, 66, 105, 196, 0 };
  //const int Seed[RIJ_N]        = { 0,  0, 20, 15, 55,  75, 135, 0 };
  //const int phalanxSeed[RIJ_N] = { 0, 15, 10, 40, 70, 105, 195, 0 };

	for (int opposed = 0; opposed <= 1; ++opposed)
		for (int phalanx = 0; phalanx <= 1; ++phalanx)
			for (int apex = 0; apex <= 1; ++apex)
				for (Rij r = RIJ_2; r < RIJ_8; ++r)
				{
					int v = (phalanx ? phalanxSeed[r] : Seed[r]) >> opposed;
					v += (apex ? v / 2 : 0);
					Connected[opposed][phalanx][apex][r] = pawn_make_score(3 * v / 2, v);
				}
}


/// Pawns::probe() looks up the current position's pawns configuration in
/// the pawns hash table. It returns a pointer to the Entry if the position
/// is found. Otherwise a new Entry is computed and stored there, so we don't
/// have to recompute all when the same pawns configuration occurs again.

Tuple* probe(const Stelling& pos) {

  Sleutel64 key = pos.pion_sleutel();
  Tuple* e = pos.ti()->pionnenTabel[key];

  if (e->key == key)
      return e;

  e->key = key;

  Bitboard bb_pionlijnen[2];

  // bereken pionstelling
  // ====================
  Bitboard WPion = pos.stukken(WIT, PION);
  Bitboard ZPion = pos.stukken(ZWART, PION);

  bb_pionlijnen[WIT] = LijnVoorAchter(WPion);
  bb_pionlijnen[ZWART] = LijnVoorAchter(ZPion);

  e->pionAanval[WIT] = pion_aanval<WIT>(WPion);
  e->pionAanval[ZWART] = pion_aanval<ZWART>(ZPion);

  e->pionAanvalBereik[WIT] = LijnVoor(e->pionAanval[WIT]);
  e->pionAanvalBereik[ZWART] = LijnAchter(e->pionAanval[ZWART]);

  e->halfOpenLijnen[WIT] = (int)~bb_pionlijnen[WIT] & 0xFF;
  e->halfOpenLijnen[ZWART] = (int)~bb_pionlijnen[ZWART] & 0xFF;


#if 0
  e->score = evaluate<WIT>(pos, e, bb_pionlijnen);
  e->score -= evaluate<ZWART>(pos, e, bb_pionlijnen);
#else
  e->score = sf_evaluate<WIT>(pos, e);
  e->score -= sf_evaluate<ZWART>(pos, e);
#endif

  e->asymmetrie = popcount(e->halfOpenLijnen[WIT] ^ e->halfOpenLijnen[ZWART]);

  int lijnen = (bb_pionlijnen[WIT] | bb_pionlijnen[ZWART]) & 0xFF;
  if (lijnen)
	  lijnen = msb(lijnen) - lsb(lijnen);
  e->lijn_breedte = std::max(lijnen - 3, 0);  // waarde tussen 0 en 4

  // geen enkele open lijn -> conversie moeilijk
  // centrale lijn(en) open, geen halfopen lijnen -> conversie moeilijk
  e->conversie_moeilijk = !(e->halfOpenLijnen[WIT] & e->halfOpenLijnen[ZWART])
	  || ((e->halfOpenLijnen[WIT] & e->halfOpenLijnen[ZWART] & 0x3c) && !e->asymmetrie);

  // bereken de gemiddelde lijn van pionnen
  Bitboard bb_pionnen = pos.stukken(PION);
  e->nPionnen = popcount(bb_pionnen);
  if (bb_pionnen)
  {
	  int LijnSom = 0;
	  do
	  {
		  Veld veld = pop_lsb(&bb_pionnen);
		  LijnSom += veld & 7;
	  } while (bb_pionnen);
	  e->gemiddeldeLijn = LijnSom / e->nPionnen;
  }

  return e;
}

#if 0
template<Kleur IK>
Waarde sf_shelter_storm(const Stelling& pos, Veld veldK) {

	const Kleur JIJ = (IK == WIT ? ZWART : WIT);

	enum { NoFriendlyPawn, Unblocked, BlockedByPawn, BlockedByKing };

	Bitboard b = pos.stukken(PION) & (bb_rijen_voorwaarts(IK, rij(veldK)) | bb_rij(veldK));
	Bitboard ourPawns = b & pos.stukken(IK);
	Bitboard theirPawns = b & pos.stukken(JIJ);
	Waarde safety = MaxSafetyBonus;
	Lijn center = std::max(LIJN_B, std::min(LIJN_G, lijn(veldK)));

	for (Lijn f = center - Lijn(1); f <= center + Lijn(1); ++f)
	{
		b = ourPawns & bb_lijn(f);
		Rij rkUs = b ? relatieve_rij(IK, backmost_sq(IK, b)) : RIJ_1;

		b = theirPawns & bb_lijn(f);
		Rij rkThem = b ? relatieve_rij(IK, frontmost_sq(JIJ, b)) : RIJ_1;

		safety -= ShelterWeakness[std::min(f, LIJN_H - f)][rkUs]
			+ StormDanger
			[f == lijn(veldK) && rkThem == relatieve_rij(IK, veldK) + 1 ? BlockedByKing :
			rkUs == RIJ_1 ? NoFriendlyPawn :
			rkThem == rkUs + 1 ? BlockedByPawn : Unblocked]
		[std::min(f, LIJN_H - f)][rkThem];
	}

	return safety;
}
#endif


/// Entry::do_king_safety() calculates a bonus for king safety. It is called only
/// when king square changes, which is about 20% of total koning_veiligheid() calls.

template<Kleur IK>
Score Tuple::bereken_koning_veiligheid(const Stelling& pos)
{
	const Kleur JIJ = (IK == WIT ? ZWART : WIT);

	Veld veldK = pos.veld<KONING>(IK);
	koningVeld[IK] = veldK;
	rokadeMogelijkheden[IK] = pos.rokade_mogelijkheden(IK);

	EvalWaarde veilig_bonus = veiligheid_op_veld<IK>(pos, this, veldK);

	if (pos.rokade_mogelijk(IK == WIT ? WIT_KORT: ZWART_KORT))
		veilig_bonus = std::max(veilig_bonus, veiligheid_op_veld<IK>(pos, this, relatief_veld(IK, SQ_G1)));

	const Bitboard DERDE_VIERDE_RIJ = IK == WIT ? Rank3BB | Rank4BB : Rank6BB | Rank5BB;
	const Bitboard VIJFDE_RIJ = IK == WIT ? Rank5BB : Rank4BB;

	Bitboard aanvallende_pionnen = AanvalsLijnen[lijn(veldK)] & pos.stukken(JIJ, PION);

	safety[IK] = veilig_bonus / 220 * 8
		- 16 * popcount(aanvallende_pionnen & DERDE_VIERDE_RIJ)    // vijandelijke pionnen op onze 3de rij
		-  8 * popcount(aanvallende_pionnen & VIJFDE_RIJ);         // vijandelijke pionnen op onze 5de rij

#if 0
	// sf safety
	Waarde sf_bonus = sf_shelter_storm<IK>(pos, veldK);

	// If we can castle use the bonus after the castling if it is bigger
	if (pos.rokade_mogelijk(IK == WIT ? WIT_KORT : ZWART_KORT))
		sf_bonus = std::max(sf_bonus, sf_shelter_storm<IK>(pos, relatief_veld(IK, SQ_G1)));

	if (pos.rokade_mogelijk(IK == WIT ? WIT_LANG : ZWART_LANG)
		sf_bonus = std::max(sf_bonus, sf_shelter_storm<IK>(pos, relatief_veld(IK, SQ_C1)));

	sf_safety[IK] = sf_bonus;
#endif

	Score result = maak_score(veilig_bonus, 0);

	// K op eerste rij achter 3 pionnen -> malus voor mat achter de paaltjes
	Bitboard bb_Koning = pos.stukken(IK, KONING);
	const Bitboard EERSTE_RIJ = IK == WIT ? Rank1BB : Rank8BB;

	if (bb_Koning & EERSTE_RIJ)
	{
		Bitboard bb = shift_up<IK>(bb_Koning) | shift_upleft<IK>(bb_Koning) | shift_upright<IK>(bb_Koning);
		if (bb == (pos.stukken(IK, PION) & bb))
			result += maak_score(-63, -173);
	}

	// koning voor vijandelijke pion?
	if (pos.stukken(JIJ, PION) & shift_down<IK>(bb_Koning))
		result += maak_score(0, 27);

	if (nPionnen)
		result -= Score_K_Pion_afstand[nPionnen][abs(gemiddeldeLijn - lijn(pos.veld<KONING>(IK)))];

	return result;
}

// Explicit template instantiation
template Score Tuple::bereken_koning_veiligheid<WIT>(const Stelling& pos);
template Score Tuple::bereken_koning_veiligheid<ZWART>(const Stelling& pos);

} // namespace Pawns
