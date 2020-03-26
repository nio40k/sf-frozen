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

#include <cassert>
#include <iostream>

#include "movepick.h"
#include "thread.h"

namespace {

  // Our insertion sort, which is guaranteed to be stable, as it should be
  void insertion_sort(ZetEx* begin, ZetEx* end)
  {
    ZetEx tmp, *p, *q;

    for (p = begin + 1; p < end; ++p)
    {
        tmp = *p;
        for (q = p; q != begin && *(q-1) < tmp; --q)
            *q = *(q-1);
        *q = tmp;
    }
  }

  // pick_best() finds the best move in the range (begin, end) and moves it to
  // the front. It's faster than sorting all the moves in advance when there
  // are few moves, e.g., the possible captures.
  Zet vind_beste_zet(ZetEx* begin, ZetEx* end)
  {
	  ZetEx* best = std::max_element(begin, end);
	  Zet zet = best->zet;
	  *best = *begin;
      return zet;
  }

  const int PieceOrder[STUK_N] = { 
	  0, 6, 1, 2, 3, 4, 5, 0,
	  0, 6, 1, 2, 3, 4, 5, 0
  };

  const int CaptureSortValues[STUK_N] = {
	  0, 0, 198, 817, 836, 1270, 2521, 0,
	  0, 0, 198, 817, 836, 1270, 2521, 0
  };

  unsigned short crc16(const unsigned char* data_p, unsigned char length)
  {
	  unsigned char x;
	  unsigned short crc = 0xFFFF;

	  while (length--) {
		  x = crc >> 8 ^ *data_p++;
		  x ^= x >> 4;
		  crc = (crc << 8) ^ ((unsigned short)(x << 12)) ^ ((unsigned short)(x << 5)) ^ ((unsigned short)x);
	  }
	  return crc;
  }

  int hash_bitboard(const Bitboard bb)
  {
	  unsigned short crc = crc16((unsigned char *)(&bb), 8);
	  return int(crc);
  }

} // namespace


/// Constructors of the PikZet class. As arguments we pass information
/// to help it to return the (presumably) good moves first, to decide which
/// moves to return (in the quiescence search, for instance, we only want to
/// search captures, promotions, and some checks) and how important good move
/// ordering is at the current node.

namespace PikZet {

	void init_search(const Stelling& pos, Zet ttm, Diepte d) {

		assert(d >= PLY);
		StellingInfo* st = pos.info();
		st->mp_diepte = d;

		if (is_ok(st->vorigeZet))
		{
			Veld prevSq = naar_veld(st->vorigeZet);
			Zet* cm = pos.ti()->counterZetten.get(pos.stuk_op_veld(prevSq), prevSq);
			st->mp_counterZet = cm[0];
			//countermove = pos.legal(cm[0]) ? cm[0] : cm[1];
			//if (countermove == GEEN_ZET || countermove == ttm || !pos.pseudo_legal(countermove) || pos.capture(countermove))
			   // countermove = cm[1];
		}
		else
			st->mp_counterZet = GEEN_ZET;

		st->mp_etappe = pos.schaak_gevers() ? GA_UIT_SCHAAK : NORMAAL_ZOEKEN;
		st->mp_ttZet = ttm && pos.geldige_zet(ttm) ? ttm : GEEN_ZET;
		if (!st->mp_ttZet)
			++st->mp_etappe;
	}

	void init_qsearch(const Stelling& pos, Zet ttm, Diepte d, Veld s) {

		assert(d < PLY);
		StellingInfo* st = pos.info();

		if (pos.schaak_gevers())
			st->mp_etappe = GA_UIT_SCHAAK;

		else if (d >= QS_SCHAAK_DIEPTE)
			st->mp_etappe = QSEARCH_MET_SCHAAK;

		else if (d >= QS_SLAG_OP_VELD_DIEPTE + PLY)
			st->mp_etappe = QSEARCH_ZONDER_SCHAAK;

		else
		{
			st->mp_etappe = TERUGSLAG_GEN;
			st->mp_slagVeld = s;
			return;
		}

		st->mp_ttZet = ttm && pos.geldige_zet(ttm) ? ttm : GEEN_ZET;
		if (!st->mp_ttZet)
			++st->mp_etappe;
	}

	void init_probcut(const Stelling& pos, Zet ttm, SeeWaarde th) {

		assert(!pos.schaak_gevers());
		StellingInfo* st = pos.info();
		st->mp_threshold = th + SeeWaarde(1);

		st->mp_etappe = PROBCUT;

		// In ProbCut we generate captures with SEE higher than the given threshold
		st->mp_ttZet = ttm
			&& pos.geldige_zet(ttm)
			&& pos.is_slagzet(ttm)
			&& pos.see_test(ttm, st->mp_threshold) ? ttm : GEEN_ZET;

		if (!st->mp_ttZet)
			++st->mp_etappe;
	}

	/// score() assigns a numerical value to each move in a move list. The moves with
	/// highest values will be picked first.
	template<>
	void score<GEN_SLAG_OF_PROMOTIE>(const Stelling& pos) {
		// Winning and equal captures in the main search are ordered by MVV, preferring
		// captures near our home rank. Surprisingly, this appears to perform slightly
		// better than SEE-based move ordering: exchanging big pieces before capturing
		// a hanging piece probably helps to reduce the subtree size.
		// In the main search we want to push captures with negative SEE values to the
		// badCaptures[] array, but instead of doing it now we delay until the move
		// has been picked up, saving some SEE calls in case we get a cutoff.
		StellingInfo* st = pos.info();
		for (ZetEx *z = st->mp_huidigeZet; z < st->mp_eindeLijst; z++)
			z->waarde = SorteerWaarde(CaptureSortValues[pos.stuk_op_veld(naar_veld(z->zet))]
				- 200 * relatieve_rij(pos.aan_zet(), naar_veld(z->zet)));
	}

	template<>
	void score<GEN_RUSTIGE_ZETTEN>(const Stelling& pos) {

		const ZetWaardeStatistiek& history = pos.ti()->history;

		StellingInfo* st = pos.info();
		const CounterZetWaarden* cm = st->counterZetWaarden;
		const CounterZetWaarden* fm = (st - 1)->counterZetWaarden;
		const CounterZetWaarden* f2 = (st - 3)->counterZetWaarden;

		if (uintptr_t(cm) & uintptr_t(fm) & uintptr_t(f2))
		{
			for (ZetEx *z = st->mp_huidigeZet; z < st->mp_eindeLijst; z++)
			{
				int offset = ZetWaardeStatistiek::bereken_offset(pos.bewogen_stuk(z->zet), naar_veld(z->zet));
				z->waarde = history.valueAtOffset(offset)
					+ cm->valueAtOffset(offset)
					+ fm->valueAtOffset(offset)
					+ f2->valueAtOffset(offset);
			}
		}
		else
		{
			for (ZetEx *z = st->mp_huidigeZet; z < st->mp_eindeLijst; z++)
			{
				int offset = ZetWaardeStatistiek::bereken_offset(pos.bewogen_stuk(z->zet), naar_veld(z->zet));
				z->waarde = history.valueAtOffset(offset)
					+ (cm ? cm->valueAtOffset(offset) : SORT_ZERO)
					+ (fm ? fm->valueAtOffset(offset) : SORT_ZERO)
					+ (f2 ? f2->valueAtOffset(offset) : SORT_ZERO);
			}
		}
	}

	template<>
	void score<GEN_GA_UIT_SCHAAK>(const Stelling& pos) {
		// Try winning and equal captures ordered by MVV/LVA, then non-captures ordered
		// by history value, then bad captures and quiet moves with a negative SEE ordered
		// by SEE value.
		StellingInfo* st = pos.info();
		const ZetWaardeStatistiek& history = pos.ti()->evasionHistory;
		//const MaxWinstStats& gains = pos.ti()->maxWinst;

		for (ZetEx *z = st->mp_huidigeZet; z < st->mp_eindeLijst; z++)
		{
			if (pos.is_slagzet(z->zet))
				z->waarde = SorteerWaarde(CaptureSortValues[pos.stuk_op_veld(naar_veld(z->zet))]
					- PieceOrder[pos.bewogen_stuk(z->zet)]) + SORT_MAX;
			else
				z->waarde = history[pos.bewogen_stuk(z->zet)][naar_veld(z->zet)];
			//+ SorteerWaarde(gains.get(pos->bewogen_stuk(m), m) * (-4));
		}
	}


	/// geef_zet() is the most important method of the PikZet class. It returns
	/// a new pseudo legal move every time it is called, until there are no more moves
	/// left. It picks the move with the biggest value from a list of generated moves
	/// taking care not to return the ttMove if it has already been searched.

	Zet geef_zet(const Stelling& pos) {

		StellingInfo* st = pos.info();

		switch (st->mp_etappe) {

		case NORMAAL_ZOEKEN: case GA_UIT_SCHAAK:
		case QSEARCH_MET_SCHAAK: case QSEARCH_ZONDER_SCHAAK:
		case PROBCUT:
			st->mp_eindeLijst = (st - 1)->mp_eindeLijst;
			++st->mp_etappe;
			return st->mp_ttZet;

		case GOEDE_SLAGEN_GEN:
			st->mp_huidigeZet = (st - 1)->mp_eindeLijst;
			st->mp_eindeSlechteSlag = st->mp_huidigeZet;
			st->mp_eindeLijst = genereerZetten<GEN_SLAG_OF_PROMOTIE>(pos, st->mp_huidigeZet);
			score<GEN_SLAG_OF_PROMOTIE>(pos);
			st->mp_etappe = GOEDE_SLAGEN;

		case GOEDE_SLAGEN:
			while (st->mp_huidigeZet < st->mp_eindeLijst)
			{
				Zet zet = vind_beste_zet(st->mp_huidigeZet++, st->mp_eindeLijst);
				if (zet != st->mp_ttZet)
				{
					if (pos.see_test(zet, SEE_0))
						return zet;

					// Losing capture, move it to the tail of the array
					*st->mp_eindeSlechteSlag++ = zet;
				}
			}

			// first killer
			st->mp_etappe = KILLERS;
			{
				Zet zet = st->killers[0];
				if (zet && zet != st->mp_ttZet && pos.geldige_zet(zet) && !pos.is_slagzet(zet))
					return zet;
			}

		case KILLERS:
			// second killer
			st->mp_etappe = KILLERS1;
			{
				Zet zet = st->killers[1];
				if (zet && zet != st->mp_ttZet && pos.geldige_zet(zet) && !pos.is_slagzet(zet))
					return zet;
			}

		case KILLERS1:
			// counter move
			st->mp_etappe = RUSTIGE_ZETTEN_GEN;
			{
				Zet zet = st->mp_counterZet;
				if (zet && zet != st->mp_ttZet && zet != st->killers[0]
					&& zet != st->killers[1]
					&& pos.geldige_zet(zet) && !pos.is_slagzet(zet))
					return zet;
			}

		case RUSTIGE_ZETTEN_GEN:
			st->mp_huidigeZet = st->mp_eindeSlechteSlag;
			st->mp_eindeLijst = genereerZetten<GEN_RUSTIGE_ZETTEN>(pos, st->mp_huidigeZet);
			score<GEN_RUSTIGE_ZETTEN>(pos);
			if (st->mp_diepte < 3 * PLY)
			{
				// op lage diepte: sorteer enkel maar de positieve zetten; dus eerst een partitie maken
				ZetEx* eersteSlechteZet = std::partition(st->mp_huidigeZet, st->mp_eindeLijst, [](const ZetEx& m)
					{ return m.waarde > SORT_ZERO; });
				insertion_sort(st->mp_huidigeZet, eersteSlechteZet);
			}
			else
				insertion_sort(st->mp_huidigeZet, st->mp_eindeLijst);
			st->mp_etappe = RUSTIGE_ZETTEN;

		case RUSTIGE_ZETTEN:
			while (st->mp_huidigeZet < st->mp_eindeLijst)
			{
				Zet zet = *st->mp_huidigeZet++;
				if (zet != st->mp_ttZet
					&& zet != st->killers[0]
					&& zet != st->killers[1]
					&& zet != st->mp_counterZet)
					return zet;
			}

			st->mp_huidigeZet = (st - 1)->mp_eindeLijst;  // bad captures
			st->mp_eindeLijst = st->mp_eindeSlechteSlag;
			st->mp_etappe = SLECHTE_SLAGEN;

		case SLECHTE_SLAGEN:
			if (st->mp_huidigeZet < st->mp_eindeLijst)
				return *st->mp_huidigeZet++;
			return GEEN_ZET;

		case UIT_SCHAAK_GEN:
			st->mp_huidigeZet = (st - 1)->mp_eindeLijst;
			st->mp_eindeLijst = genereerZetten<GEN_GA_UIT_SCHAAK>(pos, st->mp_huidigeZet);
			score<GEN_GA_UIT_SCHAAK>(pos);
			st->mp_etappe = UIT_SCHAAK_LUS;

		case UIT_SCHAAK_LUS:
			while (st->mp_huidigeZet < st->mp_eindeLijst)
			{
				Zet zet = vind_beste_zet(st->mp_huidigeZet++, st->mp_eindeLijst);
				if (zet != st->mp_ttZet)
					return zet;
			}
			return GEEN_ZET;

		case QSEARCH_1:
			st->mp_huidigeZet = (st - 1)->mp_eindeLijst;
			st->mp_eindeLijst = genereerZetten<GEN_SLAG_OF_PROMOTIE>(pos, st->mp_huidigeZet);
			score<GEN_SLAG_OF_PROMOTIE>(pos);
			st->mp_etappe = QSEARCH_1_SLAGZETTEN;

		case QSEARCH_1_SLAGZETTEN:
			while (st->mp_huidigeZet < st->mp_eindeLijst)
			{
				Zet zet = vind_beste_zet(st->mp_huidigeZet++, st->mp_eindeLijst);
				if (zet != st->mp_ttZet)
					return zet;
			}

			st->mp_huidigeZet = (st - 1)->mp_eindeLijst;
			st->mp_eindeLijst = genereerZetten<GEN_RUSTIGE_SCHAAKS>(pos, st->mp_huidigeZet);
			st->mp_etappe = QSEARCH_SCHAAKZETTEN;

		case QSEARCH_SCHAAKZETTEN:
			while (st->mp_huidigeZet < st->mp_eindeLijst)
			{
				Zet zet = *st->mp_huidigeZet++;
				if (zet != st->mp_ttZet)
					return zet;
			}
			return GEEN_ZET;

		case QSEARCH_2:
			st->mp_huidigeZet = (st - 1)->mp_eindeLijst;
			st->mp_eindeLijst = genereerZetten<GEN_SLAG_OF_PROMOTIE>(pos, st->mp_huidigeZet);
			score<GEN_SLAG_OF_PROMOTIE>(pos);
			st->mp_etappe = QSEARCH_2_SLAGZETTEN;

		case QSEARCH_2_SLAGZETTEN:
			while (st->mp_huidigeZet < st->mp_eindeLijst)
			{
				Zet zet = vind_beste_zet(st->mp_huidigeZet++, st->mp_eindeLijst);
				if (zet != st->mp_ttZet)
					return zet;
			}
			return GEEN_ZET;

		case PROBCUT_GEN:
			st->mp_huidigeZet = (st - 1)->mp_eindeLijst;
			st->mp_eindeLijst = genereerZetten<GEN_SLAG_OF_PROMOTIE>(pos, st->mp_huidigeZet);
			score<GEN_SLAG_OF_PROMOTIE>(pos);
			st->mp_etappe = PROBCUT_SLAGZETTEN;

		case PROBCUT_SLAGZETTEN:
			while (st->mp_huidigeZet < st->mp_eindeLijst)
			{
				Zet zet = vind_beste_zet(st->mp_huidigeZet++, st->mp_eindeLijst);
				if (zet != st->mp_ttZet && pos.see_test(zet, st->mp_threshold))
					return zet;
			}
			return GEEN_ZET;

		case TERUGSLAG_GEN:
			st->mp_huidigeZet = (st - 1)->mp_eindeLijst;
			st->mp_eindeLijst = genereerSlagzettenOpVeld(pos, st->mp_huidigeZet, st->mp_slagVeld);
			score<GEN_SLAG_OF_PROMOTIE>(pos);
			st->mp_etappe = TERUGSLAG_ZETTEN;

		case TERUGSLAG_ZETTEN:
			while (st->mp_huidigeZet < st->mp_eindeLijst)
			{
				Zet zet = vind_beste_zet(st->mp_huidigeZet++, st->mp_eindeLijst);
				if (naar_veld(zet) == st->mp_slagVeld)
					return zet;
			}
			return GEEN_ZET;

		default:
			assert(false);
			return GEEN_ZET;
		}
	}
}


// SpecialKillerStats

int SpecialKillerStats::index_mijn_stukken(const Stelling &pos, Kleur c)
{
	return hash_bitboard(pos.stukken(c));
}

int SpecialKillerStats::index_jouw_stukken(const Stelling &pos, Kleur c, Veld to)
{
	return hash_bitboard(pos.stukken(c, stuk_type(pos.stuk_op_veld(to))));
}

