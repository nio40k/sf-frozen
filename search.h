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

#ifndef SEARCH_H_INCLUDED
#define SEARCH_H_INCLUDED

#include <atomic>

#include "misc.h"
#include "position.h"
#include "types.h"

template<typename T, bool CM> struct StukVeldStatistiek;
typedef StukVeldStatistiek<SorteerWaarde, true> CounterZetWaarden;

namespace Search {


/// RootMove struct is used for moves at the root of the tree. For each root move
/// we store a score and a PV (really a refutation in the case of moves which
/// fail low). Score is normally set at -HOOGSTE_WAARDE for all non-pv moves.

template <int Capacity>
struct VasteZettenLijst {

	int zetNummer;
	Zet zetten[Capacity];

	VasteZettenLijst() : zetNummer(0) {}
	void add(Zet zet) { if (zetNummer < Capacity) zetten[zetNummer++] = zet; }
	Zet& operator[](int index) { return zetten[index]; }
	const Zet& operator[](int index) const { return zetten[index]; }
	int size() const { return zetNummer; }
	void resize(int newSize) { zetNummer = newSize; }
	void clear() { zetNummer = 0; }
	bool empty() { return zetNummer == 0; }
	int find(Zet zet) {
		for (int i = 0; i < zetNummer; i++)
			if (zetten[i] == zet)
				return i;
		return -1;
	}
};

typedef VasteZettenLijst<MAX_PV> HoofdVariant;
typedef VasteZettenLijst<MAX_ZETTEN> MaxZettenLijst;

struct RootZet {

  RootZet() {}
  RootZet(Zet zet) { pv.zetNummer = 1; pv.zetten[0] = zet; }

  bool operator<(const RootZet& rootZet) const { return rootZet.score < score; } // Descending sort
  bool operator==(const Zet& zet) const { return pv[0] == zet; }
  bool vind_ponder_zet_in_tt(Stelling& pos);
  void vervolledig_pv_met_tt(Stelling& pos);

  Diepte diepte = DIEPTE_0;
  Waarde score = -HOOGSTE_WAARDE;
  Waarde vorigeRootScore = -HOOGSTE_WAARDE;
  Waarde startWaarde;
  HoofdVariant pv;
};

struct RootZetten {

	RootZetten() : zetAantal(0) {}

	int zetAantal, dummy;
	RootZet zetten[MAX_ZETTEN];

	void add(RootZet rootZet) { zetten[zetAantal++] = rootZet; }
	RootZet& operator[](int index) { return zetten[index]; }
	const RootZet& operator[](int index) const { return zetten[index]; }
	void clear() { zetAantal = 0; }
	int find(Zet zet) {
		for (int i = 0; i < zetAantal; i++)
			if (zetten[i].pv[0] == zet)
				return i;
		return -1;
	}
};


/// TijdLimiet struct stores information sent by GUI about available time to
/// search the current move, maximum depth/time, if we are in analysis mode or
/// if we have to ponder while it's our opponent's turn to move.

struct TijdLimiet {

  TijdLimiet() { // Init explicitly due to broken value-initialization of non POD in MSVC
    nodes = time[WIT] = time[ZWART] = inc[WIT] = inc[ZWART] =
    movestogo = depth = movetime = mate = infinite = ponder = 0;
  }

  bool gebruikt_tijd_berekening() const {
    return !(mate | movetime | depth | nodes | infinite);
  }

  TimePoint startTijd;
  int time[KLEUR_N], inc[KLEUR_N], movestogo, depth, movetime, mate, infinite, ponder;
  uint64_t nodes;
  MaxZettenLijst zoekZetten;
};

/// The SignalsType struct stores atomic flags updated during the search
/// typically in an async fashion e.g. to stop the search by the GUI.

struct SignalsType {
  std::atomic_bool stop, stopOnPonderhit;
};

extern SignalsType Signals;
extern TijdLimiet Limits;
extern bool Running;
void pas_tijd_aan_na_ponderhit();

void init();
void clear();
//template<bool Root = true> uint64_t perft(Position& pos, Depth depth);

} // namespace Search


typedef Waarde(*EGTB_probe_fx)(Stelling& pos, Waarde alfa, Waarde beta);

namespace Tablebases {

	extern int MaxPieces_wdl, MaxPieces_dtz, MaxPieces_dtm;
	extern EGTB_probe_fx EGTB_probe_wdl;
	extern EGTB_probe_fx EGTB_probe_dtz;
	extern EGTB_probe_fx EGTB_probe_dtm;
	extern bool UseRule50;
}

#endif // #ifndef SEARCH_H_INCLUDED
