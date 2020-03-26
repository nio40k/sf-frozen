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

#include <algorithm> // For std::min
#include <cassert>
#include <cstring>   // For std::memset

#include "bitboard.h"
#include "material.h"
#include "thread.h"
#include "uci.h"


namespace {

  /// imbalance() calculates the imbalance by comparing the piece count of each
  /// piece type for both colors.
  #define mg_transform(mg,eg) ((105 * ScoreMultiply(mg) -  5 * ScoreMultiply(eg)) / 100)
  #define eg_transform(mg,eg) ((  1 * ScoreMultiply(mg) + 99 * ScoreMultiply(eg)) / 100)
  #define SCORE64(mg, eg) (((int64_t)(mg_transform(mg,eg)) << 32) + (int64_t)(eg_transform(mg,eg)))
  static const double PionFactor[5] = { 0.4, 0.55, 0.7, 0.8, 0.9 };

  EvalWaarde imbalance(int WPion, int WP, int WL, int WLL, int WLD, int WT, int WD,
	  int ZPion, int ZP, int ZL, int ZLL, int ZLD, int ZT, int ZD, SchaalFactor &conversion) {

	double conversie;
	const int MAX_FASE = 32;
	int fase = (WP + ZP) + (WL + ZL) + 3 * (WT + ZT) + 6 * (WD + ZD);
	fase = std::min(fase, MAX_FASE);

	int matIndexW = 6 * WP + 7 * WL + 10 * WT + 18 * WD;
	int matIndexZ = 6 * ZP + 7 * ZL + 10 * ZT + 18 * ZD;

	int64_t score = SCORE64(597, 972) * (WPion - ZPion);
	score += SCORE64(3210, 3510) * (WP - ZP);
	score += SCORE64(3320, 3600) * (WL - ZL);
	score += SCORE64(4770, 5740) * (WT - ZT);
	score += SCORE64(9940, 10950) * (WD - ZD);

	if (WLL && WLD)
		score += SCORE64(300, 510);
	if (ZLL && ZLD)
		score -= SCORE64(300, 510);

	if (WT > 1)
		score -= SCORE64(268, 164);
	if (ZT > 1)
		score += SCORE64(268, 164);

	score += SCORE64(33, 36) * (WP * (WPion - 5) - ZP * (ZPion - 5));
	score -= SCORE64(21, 35) * (WT * (WPion - 5) - ZT * (ZPion - 5));
	score += SCORE64(5, -22) * (WD * (WPion - 5) - ZD * (ZPion - 5));
	//if (WLL && WLD)
	//	score -= SCORE64(20, 22) * (WPion - 5);
	//if (ZLL && ZLD)
	//	score += SCORE64(20, 22) * (ZPion - 5);

	if (WD)
		score -= SCORE64(124, 95) * (WD + WT - 1);
	if (ZD)
		score += SCORE64(124, 95) * (ZD + ZT - 1);

	int WLichtStuk = WL + WP;
	int ZLichtStuk = ZL + ZP;

	if (WLichtStuk > ZLichtStuk)
	{
		if (WL > ZL)
			score += SCORE64(27, 50);
		if (WP > ZP)
			score -= SCORE64(27, 50);
	}
	else if (WLichtStuk < ZLichtStuk)
	{
		if (WL < ZL)
			score -= SCORE64(27, 50);
		if (WP < ZP)
			score += SCORE64(27, 50);
	}
	//if (WLL && WLD && !ZLichtStuk)
	//	score += SCORE64(30, 50);
	//if (ZLL && ZLD && !WLichtStuk)
	//	score -= SCORE64(30, 50);

	int WStukken = WD + WT + WLichtStuk;
	int ZStukken = ZD + ZT + ZLichtStuk;
	if (WStukken > ZStukken + 1)
		score += SCORE64(606, 222);
	else if (ZStukken > WStukken + 1)
		score -= SCORE64(606, 222);

	int WVal = matIndexW >> 1;
	int ZVal = matIndexZ >> 1;
	int matlBalance = WVal - ZVal;
	if (matlBalance > 0)
		score += SCORE64(0, 2) * (31 - ZVal);
	else if (matlBalance < 0)
		score -= SCORE64(0, 2) * (31 - WVal);

	conversie = 0.95 + 0.21 * fase / MAX_FASE;
	int matScore = (fase * (((score + 0x80000000) >> 32) & 0xFFFFFFFF) + (score & 0xFFFFFFFF) * (MAX_FASE - fase));
	matScore /= MAX_FASE;

	if (ZPion + WPion && !matIndexW && !matIndexZ)
	{
		// pionneneindspel
		conversie = 1.3;
	}
	if (WL == 1 && ZL == 1 && (WLL && ZLD || WLD && ZLL))
	{
		// ongelijke lopers
		if (WStukken == 1 && ZStukken == 1)
			conversie = 0.6;
		else
			conversie = conversie * 0.95;
	}
	if (WPion + ZPion)
	{
		if (matScore < 0)
		{
			if (ZPion <= 4)
				conversie = conversie * PionFactor[ZPion];
		}
		else if (WPion <= 4)
			conversie = conversie * PionFactor[WPion];
	}
	else
	{
		// zonder pionnen
		int matDifference = abs(matlBalance);
		if (matDifference <= 2)
			conversie = conversie * 0.1;
		else if (matDifference == 3)
			conversie = conversie * 0.2;
		else if (matDifference == 4)
			conversie = conversie * 0.5;
	}

	conversion = SchaalFactor(std::lround(conversie * MAX_FACTOR));
	return EvalWaarde(matScore);
  }

} // namespace

namespace Materiaal {

Waarde Tuple::waarde_uit_functie(const Stelling& pos) const
{
	return (*Threads.endgames.waarde_functies[waardeFunctieIndex])(pos);
}

SchaalFactor Tuple::schaalfactor_uit_functie(const Stelling & pos, Kleur c) const
{
	if (schaalFunctieIndex[c] >= 0)
	{
		SchaalFactor sf = (*Threads.endgames.factor_functies[schaalFunctieIndex[c]])(pos);
		if (sf != GEEN_FACTOR)
			return sf;
	}
	return SchaalFactor(factor[c]);
}


/// Material::probe() looks up the current position's material configuration in
/// the material hash table. It returns a pointer to the Entry if the position
/// is found. Otherwise a new Entry is computed and stored there, so we don't
/// have to recompute all when the same material configuration occurs again.

Tuple* probe(const Stelling& pos) {

  // index in materialTable is de sleutel met kleur lopers (want conversie-factoren hangen af van kleur lopers)
  // index in endgames is sleutel zonder kleur lopers (want de routines maken dit onderscheid zelf)
  Sleutel64 key = pos.materiaal_sleutel() ^ pos.loperkleur_sleutel();
  Tuple* e = pos.ti()->materiaalTabel[key];

  if (e->key == key)
      return e;

  std::memset(e, 0, sizeof(Tuple));
  e->key = key;
  e->factor[WIT] = e->factor[ZWART] = (uint8_t)NORMAAL_FACTOR;
  e->gamePhase = pos.partij_fase();
  e->conversie = MAX_FACTOR;
  e->waardeFunctieIndex = e->schaalFunctieIndex[WIT] = e->schaalFunctieIndex[ZWART] = -1;

  // Evaluate the material imbalance
  e->waarde = imbalance(
	  pos.aantal<PION>(WIT), pos.aantal<PAARD>(WIT), pos.aantal<LOPER>(WIT),
	  popcount(pos.stukken(WIT, LOPER) & ~DonkereVelden), popcount(pos.stukken(WIT, LOPER) & DonkereVelden),
	  pos.aantal<TOREN>(WIT), pos.aantal<DAME>(WIT),
	  pos.aantal<PION>(ZWART), pos.aantal<PAARD>(ZWART), pos.aantal<LOPER>(ZWART),
	  popcount(pos.stukken(ZWART, LOPER) & ~DonkereVelden), popcount(pos.stukken(ZWART, LOPER) & DonkereVelden),
	  pos.aantal<TOREN>(ZWART), pos.aantal<DAME>(ZWART), e->conversie);

  // Let's look if we have a specialized evaluation function for this particular
  // material configuration. Firstly we look for a fixed configuration one, then
  // for a generic one if the previous search failed.
  e->waardeFunctieIndex = Threads.endgames.probe_waarde(pos.materiaal_sleutel());
  if (e->waardeFunctieIndex >= 0)
      return e;

  for (Kleur c = WIT; c <= ZWART; ++c)
      if (!meer_dan_een(pos.stukken(~c)) && pos.niet_pion_materiaal(c) >= MATL_TOREN)
      {
          e->waardeFunctieIndex = c == WIT ? 0 : 1;  // endgameValue_KXK
          return e;
      }

  // OK, we didn't find any special evaluation function for the current material
  // configuration. Is there a suitable specialized scaling function?
  Kleur sterkeZijde;
  int sf = Threads.endgames.probe_schaalfactor(pos.materiaal_sleutel(), sterkeZijde);

  if (sf >= 0)
  {
      e->schaalFunctieIndex[sterkeZijde] = sf; // Only strong color assigned
	  return e;
  }

  // We didn't find any specialized scaling function, so fall back on generic
  // ones that refer to more than one material distribution. Note that in this
  // case we don't return after setting the function.
  for (Kleur c = WIT; c <= ZWART; ++c)
  {
    if (pos.niet_pion_materiaal(c) == MATL_LOPER && pos.aantal<LOPER>(c) == 1 && pos.aantal<PION>(c) >= 1)
        e->schaalFunctieIndex[c] = c == WIT ? 0 : 1;  // endgameScaleFactor_KBPsK

    else if (!pos.aantal<PION>(c) && pos.niet_pion_materiaal(c) == MATL_DAME && pos.aantal<DAME>(c) == 1
		&& pos.aantal<TOREN>(~c) == 1 && pos.aantal<PION>(~c) >= 1)
        e->schaalFunctieIndex[c] = c == WIT ? 2 : 3;  // endgameScaleFactor_KQKRPs
  }

  MateriaalWaarde npm_w = pos.niet_pion_materiaal(WIT);
  MateriaalWaarde npm_b = pos.niet_pion_materiaal(ZWART);

  if (npm_w + npm_b == MATL_0 && pos.stukken(PION)) // pionneneindspel, maar niet KPK
  {
      if (!pos.aantal<PION>(ZWART))
      {
          assert(pos.aantal<PION>(WIT) >= 2);
          e->schaalFunctieIndex[WIT] = 4;  // endgameScaleFactor_KPsK<WIT>
      }
      else if (!pos.aantal<PION>(WIT))
      {
          assert(pos.aantal<PION>(ZWART) >= 2);
          e->schaalFunctieIndex[ZWART] = 5;  // endgameScaleFactor_KPsK<ZWART>
      }
      else if (pos.aantal<PION>(WIT) == 1 && pos.aantal<PION>(ZWART) == 1)
      {
          // This is a special case because we set scaling functions
          // for both colors instead of only one.
          e->schaalFunctieIndex[WIT] = 6;    // endgameScaleFactor_KPKP<WIT>
          e->schaalFunctieIndex[ZWART] = 7;  // endgameScaleFactor_KPKP<ZWART>
      }
  }

  // Zero or just one pawn makes it difficult to win, even with a small material
  // advantage. This catches some trivial draws like KK, KBK and KNK and gives a
  // drawish scale factor for cases such as KRKBP and KmmKm (except for KBBKN).
  if (!pos.aantal<PION>(WIT) && npm_w - npm_b <= MATL_LOPER)
      e->factor[WIT] = uint8_t(npm_w < MATL_TOREN ? REMISE_FACTOR :
                                 npm_b <= MATL_LOPER ? SchaalFactor(6) : SchaalFactor(22));

  if (!pos.aantal<PION>(ZWART) && npm_b - npm_w <= MATL_LOPER)
      e->factor[ZWART] = uint8_t(npm_b <  MATL_TOREN ? REMISE_FACTOR :
                                 npm_w <= MATL_LOPER ? SchaalFactor(6) : SchaalFactor(22));

  if (pos.aantal<PION>(WIT) == 1 && npm_w - npm_b <= MATL_LOPER)
      e->factor[WIT] = (uint8_t) EEN_PION_FACTOR;

  if (pos.aantal<PION>(ZWART) == 1 && npm_b - npm_w <= MATL_LOPER)
      e->factor[ZWART] = (uint8_t) EEN_PION_FACTOR;

  e->conversie_is_geschat = true;

  return e;
}


} // namespace Material
