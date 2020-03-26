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

#ifndef MOVEPICK_H_INCLUDED
#define MOVEPICK_H_INCLUDED

#include <algorithm>
#include <cstring>

#include "movegen.h"
#include "position.h"
#include "search.h"
#include "types.h"


template<typename T, bool CM = false>
struct StukVeldStatistiek {

  const T* operator[](Stuk pc) const { return table[pc]; }
  T* operator[](Stuk pc) { return table[pc]; }
  void clear() { std::memset(table, 0, sizeof(table)); }

  static int bereken_offset(Stuk pc, Veld to) { return 64 * int(pc) + int(to); }
  T valueAtOffset(int offset) const { return *((T*)table + offset); }

  void updatePlus(int offset, SorteerWaarde v) {

	  T& elem = *((T*)table + offset);

	  //elem -= elem * int(v) / (CM ? 936 * 32 : 324 * 32);
	  elem -= elem * int(v) / (CM ? 3 * 8192 : 8192);
	  elem += v;
  }

  void updateMinus(int offset, SorteerWaarde v) {

	  T& elem = *((T*)table + offset);

	  //elem -= elem * int(v) / (CM ? 936 * 32 : 324 * 32);
	  elem -= elem * int(v) / (CM ? 3 * 8192 : 8192);
	  elem -= v;
  }

private:
  T table[STUK_N][VELD_N];
};

struct MoveStats {

	Zet* get(Stuk pc, Veld s) { return table[pc][s]; }
	void clear() { std::memset(table, 0, sizeof(table)); }
	void update(Stuk pc, Veld naar, Zet zet) {
		if (table[pc][naar][0] != zet)
		{
			table[pc][naar][1] = table[pc][naar][0];
			table[pc][naar][0] = zet;
		}
	}

private:
	Zet table[STUK_N][VELD_N][2];
};

struct MaxWinstStats {

	Waarde get(Stuk pc, Zet zet) const { return table[pc][zet & 0x0fff]; }
	void clear() { std::memset(table, 0, sizeof(table)); }
	void update(Stuk pc, Zet zet, Waarde gain) {
		Waarde* pGain = &table[pc][zet & 0x0fff];
		*pGain += Waarde((gain - *pGain + 8) >> 4);
	}

private:
	Waarde table[STUK_N][(int)VELD_N * (int)VELD_N];
};

struct SpecialKillerStats {

	static int index_mijn_stukken(const Stelling &pos, Kleur c);
	static int index_jouw_stukken(const Stelling &pos, Kleur c, Veld to);

	Zet get(Kleur c, int index) const { return table[c][index]; }
	void clear() { std::memset(table, 0, sizeof(table)); }
	void update(Kleur c, int index, Zet zet) {
		table[c][index] = zet;
	}
private:
	Zet table[KLEUR_N][65536];
};

typedef StukVeldStatistiek<SorteerWaarde, false> ZetWaardeStatistiek;
typedef StukVeldStatistiek<SorteerWaarde, true> CounterZetWaarden;
typedef StukVeldStatistiek<CounterZetWaarden> CounterZetHistoriek;


/// PikZet class is used to pick one pseudo legal move at a time from the
/// current position. The most important method is geef_zet(), which returns a
/// new pseudo legal move each time it is called, until there are no moves left,
/// when GEEN_ZET is returned. In order to improve the efficiency of the alpha
/// beta algorithm, PikZet attempts to return the moves which are most likely
/// to get a cut-off first.

inline PikZetEtappe& operator++(PikZetEtappe& d) { return d = PikZetEtappe(int(d) + 1); }

namespace PikZet {

	void init_search(const Stelling&, Zet, Diepte);
	void init_qsearch(const Stelling&, Zet, Diepte, Veld);
	void init_probcut(const Stelling&, Zet, SeeWaarde);

	Zet geef_zet(const Stelling& pos);

	template<ZetGeneratie> void score(const Stelling& pos);
};

#endif // #ifndef MOVEPICK_H_INCLUDED
