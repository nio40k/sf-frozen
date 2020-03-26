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
#include <cassert>
#include <cmath>
#include <cstring>   // For std::memset
#include <iostream>
#include <sstream>
#include <vector>

#include "evaluate.h"
#include "misc.h"
#include "movegen.h"
#include "movepick.h"
#include "search.h"
#include "timeman.h"
#include "thread.h"
#include "tt.h"
#include "uci.h"


namespace Search {

  SignalsType Signals;
  TijdLimiet Limits;
  bool Running;
}

// diepte opgeteld bij EGTB_ProbeDepth
const Diepte egtb_zeer_nuttig = 0 * PLY;
const Diepte egtb_nuttig      = 5 * PLY;
const Diepte egtb_niet_nuttig = 10 * PLY;

  int TB_Cardinality;
  bool TB_RootInTB;
  Diepte TB_ProbeDepth;
  Waarde TB_Score;

int Tablebases::MaxPieces_wdl = 0;
int Tablebases::MaxPieces_dtz = 0;
int Tablebases::MaxPieces_dtm = 0;
EGTB_probe_fx Tablebases::EGTB_probe_wdl = NULL;
EGTB_probe_fx Tablebases::EGTB_probe_dtm = NULL;
EGTB_probe_fx Tablebases::EGTB_probe_dtz = NULL;
bool Tablebases::UseRule50;

using Eval::evaluate;
using namespace Search;

namespace {

  // Different node types, used as a template parameter
  enum NodeType { NonPV, PV };

  // Razoring and futility margin based on depth
#define V(x) maak_waarde(x)
  Waarde razer_margin(Diepte d) { return V(480); }
  const Waarde margin1[7] = { V(0), V(141), V(304), V(470), V(638), V(808), V(980) };
  Waarde futility_marge1(Diepte d) { return margin1[(unsigned int)d / PLY]; }
  Waarde futility_marge2(Diepte d) { return V(256) + V(200) * int((unsigned int)d / PLY); }
#undef V

  // Futility and reductions lookup tables, initialized at startup
  int FutilityMoveCounts[2][16];  // [improving][depth]
  int futility_move_count(Diepte d, bool improving) {
	  return FutilityMoveCounts[improving][(unsigned int)d / PLY];
  }
  Diepte LMReducties[2][2][64 * (int)PLY][64]; // [pv][improving][depth][moveNumber]

  // Countermove history increments
  SorteerWaarde counterZetBonus[MAX_PLY];
  SorteerWaarde counter_zet_bonus(Diepte d) { return counterZetBonus[(unsigned int)d / PLY]; }

  template <bool PvNode> Diepte lmr_reductie(bool i, Diepte d, int mn) {
    return LMReducties[PvNode][i][std::min((int)d, 64 * (int)PLY - 1)][std::min(mn, 63)];
  }

  // EasyMoveManager structure is used to detect an 'easy move'. When the PV is
  // stable across multiple search iterations, we can quickly return the best move.
  struct EasyMoveManager {

    void clear() {
      stableCnt = 0;
      expectedPosKey = 0;
      pv[0] = pv[1] = pv[2] = GEEN_ZET;
    }

    Zet get(Sleutel64 key) const {
      return expectedPosKey == key ? pv[2] : GEEN_ZET;
    }

    void update(Stelling& pos, const HoofdVariant& newPv) {

      assert(newPv.size() >= 3);

      // Keep track of how many times in a row the 3rd ply remains stable
      stableCnt = (newPv[2] == pv[2]) ? stableCnt + 1 : 0;

      if (newPv[0] != pv[0] || newPv[1] != pv[1] || newPv[2] != pv[2])
      {
		  pv[0] = newPv[0];
		  pv[1] = newPv[1];
		  pv[2] = newPv[2];

          pos.speel_zet(newPv[0]);
          pos.speel_zet(newPv[1]);
          expectedPosKey = pos.sleutel();
          pos.neem_zet_terug(newPv[1]);
          pos.neem_zet_terug(newPv[0]);
      }
    }

    int stableCnt;
    Sleutel64 expectedPosKey;
    Zet pv[3];
  };

  // Set of rows with half bits set to 1 and half to 0. It is used to allocate
  // the search depths across the threads.
  typedef std::vector<int> Row;

  const Row HalfDensity[] = {
    {0, 1},
    {1, 0},
    {0, 0, 1, 1},
    {0, 1, 1, 0},
    {1, 1, 0, 0},
    {1, 0, 0, 1},
    {0, 0, 0, 1, 1, 1},
    {0, 0, 1, 1, 1, 0},
    {0, 1, 1, 1, 0, 0},
    {1, 1, 1, 0, 0, 0},
    {1, 1, 0, 0, 0, 1},
    {1, 0, 0, 0, 1, 1},
    {0, 0, 0, 0, 1, 1, 1, 1},
    {0, 0, 0, 1, 1, 1, 1, 0},
    {0, 0, 1, 1, 1, 1, 0 ,0},
    {0, 1, 1, 1, 1, 0, 0 ,0},
    {1, 1, 1, 1, 0, 0, 0 ,0},
    {1, 1, 1, 0, 0, 0, 0 ,1},
    {1, 1, 0, 0, 0, 0, 1 ,1},
    {1, 0, 0, 0, 0, 1, 1 ,1},
  };

  const size_t HalfDensitySize = std::extent<decltype(HalfDensity)>::value;

  EasyMoveManager EasyMove;
  Waarde RemiseWaarde[KLEUR_N];
  uint64_t vorigeInfoTijd;
  StellingInfo* mainSearchStack;

  template <NodeType NT>
  Waarde search(Stelling& pos, Waarde alpha, Waarde beta, Diepte depth, bool cutNode);

  template <NodeType NT, bool staatSchaak>
  Waarde qsearch(Stelling& pos, Waarde alpha, Waarde beta, Diepte depth);

  Waarde waarde_voor_tt(Waarde v, int ply);
  Waarde waarde_uit_tt(Waarde v, int ply);
  void update_pv(Zet* pv, Zet move, Zet* childPv);
  void update_stats(const Stelling& pos, bool staatSchaak, Zet zet, Diepte diepte, Zet* rustigeZetten, int rustigAantal);
  void tijdscontrole();

  Diepte BerekenEgtbNut(const Stelling& pos);
  void FilterRootMoves(Stelling& pos, RootZetten& rootZetten);

  //const unsigned int k_PieceValue[2][8] = { { 0, 597, 3210, 3320, 4770, 9940, 0, 0 },{ 0, 972, 3510, 3600, 5740, 10950, 0, 0 } };
  //const int futilityDeltas[8] = { 140, 145, 155, 170, 155, 145, 0, 0 };
  //const int futilityPawnRank[8] = { 0, 500, 50, 0, 0, 0, 0, 0 };
  //const int futilityPawnCount[8] = { 0, 500, 50, 0, 0, 0, 0, 0 };
  //Value k_PieceValueFase[8][32];

#define V(x) maak_waarde(x)

  const Waarde QSearchFutilityValue[STUK_N] = {
	  V(0), V(0), V(258), V(896), V(907), V(1356), V(2658), V(0),
	  V(0), V(0), V(258), V(896), V(907), V(1356), V(2658), V(0)
  };

#undef V

  const Waarde LazyMarginQSearchLow = maak_waarde(600) + WAARDE_1;
  const Waarde LazyMarginQSearchHigh = maak_waarde(600);

} // namespace


/// Search::init() is called during startup to initialize various lookup tables

void Search::init() {

	std::memset(LMReducties, 0, sizeof(LMReducties));
    for (int d = PLY; d < 64 * PLY; ++d)
        for (int mc = 2; mc < 64; ++mc)
        {
			double r = log(double(d) / PLY) * log(mc) / 2 * int(PLY);
			if (r < 6.4) 
				continue;

			LMReducties[NonPV][1][d][mc] = Diepte(std::lround(r));
			//if (Reductions[NonPV][1][d][mc] < PLY)
			//	Reductions[NonPV][1][d][mc] = DIEPTE_0;

			LMReducties[NonPV][0][d][mc] = LMReducties[NonPV][1][d][mc];
			if (LMReducties[NonPV][0][d][mc] >= 2 * PLY)
				LMReducties[NonPV][0][d][mc] += PLY;

			LMReducties[PV][0][d][mc] = LMReducties[PV][1][d][mc] = std::max(LMReducties[NonPV][1][d][mc] - PLY, DIEPTE_0);

			//Reductions[NonPV][1][d][mc] = Depth(std::lround(r * 210 / 256));
			//Reductions[NonPV][0][d][mc] = Depth(std::lround(r * 300 / 256));
			//Reductions[PV][1][d][mc] = Depth(std::lround(r * 160 / 256));
			//Reductions[PV][0][d][mc] = Depth(std::lround(r * 190 / 256));

			//Reductions[NonPV][1][d][mc] = Depth(std::lround(r) - 1);
			//Reductions[NonPV][0][d][mc] = Depth(std::lround(r) + 2);
			//Reductions[PV][1][d][mc] = Depth(std::lround(r) - 7);
			//Reductions[PV][0][d][mc] = Depth(std::lround(r) - 4);

			//if (Reductions[NonPV][1][d][mc] < PLY) Reductions[NonPV][1][d][mc] = DIEPTE_0;
			//if (Reductions[NonPV][0][d][mc] < PLY) Reductions[NonPV][0][d][mc] = DIEPTE_0;
			//if (Reductions[PV][1][d][mc] < PLY) Reductions[PV][1][d][mc] = DIEPTE_0;
			//if (Reductions[PV][0][d][mc] < PLY) Reductions[PV][0][d][mc] = DIEPTE_0;

			//Reductions[NonPV][1][d][mc] = Depth(std::lround(r) + 0);
			//if (Reductions[NonPV][1][d][mc] < PLY)
			//	Reductions[NonPV][1][d][mc] = DIEPTE_0;

			//Reductions[NonPV][0][d][mc] = Reductions[NonPV][1][d][mc];
			//if (Reductions[NonPV][0][d][mc] >= PLY)
			//	Reductions[NonPV][0][d][mc] += Depth(1);

			//Reductions[PV][1][d][mc] = std::max(Reductions[NonPV][1][d][mc] - Depth(8), DIEPTE_0);
			//Reductions[PV][0][d][mc] = std::max(Reductions[NonPV][1][d][mc] - Depth(5), DIEPTE_0);
		}

  for (int d = 1; d < 16; ++d)
  {
      FutilityMoveCounts[0][d] = int(2.4 + 0.773 * pow(double(d) + 0.00, 1.8));
      FutilityMoveCounts[1][d] = int(2.9 + 1.045 * pow(double(d) + 0.49, 1.8));
  }

  // kwadratische functie met initiële helling  (1,1) -> (2,6) -> (3,13)
  for (int d = 0; d < MAX_PLY; ++d) {
	  //counterZetBonus[d] = SorteerWaarde((d * d + 2 * d - 2) * 32);
	  counterZetBonus[d] = SorteerWaarde(std::min(8192, 26 * d * d + 34 * d - 44));
	  //counterZetBonus[d] = SorteerWaarde(std::min(8192, std::max(0, TUNE_1 * d * d + TUNE_2 * d - TUNE_3)));
  }
}


/// Search::clear() resets search state to zero, to obtain reproducible results

void Search::clear() {

  if (!Options["Never Clear Hash"])
	  TT.wis();
  Threads.wis_counter_zet_geschiedenis();

  for (int i = 0; i < Threads.threadCount; ++i)
  {
	  Thread* th = Threads.threads[i];
	  th->ti->history.clear();
	  th->ti->evasionHistory.clear();
	  th->ti->maxWinstTabel.clear();
      th->ti->counterZetten.clear();
  }

  Threads.main()->vorigeRootScore = HOOGSTE_WAARDE;
  Threads.main()->vorigeRootDiepte = 999 * PLY;
  Threads.main()->snelleZetToegelaten = false;
}


/// Search::perft() is our utility to verify move generation. All the leaf nodes
/// up to the given depth are generated and counted, and the sum is returned.
/*
template<bool Root>
uint64_t Search::perft(Position& pos, Depth depth) {

  uint64_t cnt, nodes = 0;
  const bool leaf = (depth == 2 * PLY);

  for (const auto& m : MoveList<LEGALE_ZETTEN>(pos))
  {
      if (Root && depth <= PLY)
          cnt = 1, nodes++;
      else
      {
          pos.speel_zet(m);
          cnt = leaf ? MoveList<LEGALE_ZETTEN>(pos).size() : perft<false>(pos, depth - PLY);
          nodes += cnt;
          pos.neem_zet_terug(m);
      }
      if (Root)
          sync_cout << UCI::move(m, pos.is_chess960()) << ": " << cnt << sync_endl;
  }
  return nodes;
}

template uint64_t Search::perft<true>(Position&, Depth);
*/


void kies_niet_te_sterke_zet(Thread* thread)
{
	Waarde max_waarde, random_score;
	static PRNG rng(now()); // PRNG sequence should be non-deterministic

	int max_index = 0;
	if (Threads.speelSterkte <= 10)
		random_score = Waarde(2 * (200 - 10 * Threads.speelSterkte));
	else if (Threads.speelSterkte <= 30)
		random_score = Waarde(2 * (120 - 2 * Threads.speelSterkte));
	else if (Threads.speelSterkte <= 60)
		random_score = Waarde(2 * (90 - Threads.speelSterkte));
	else
		random_score = Waarde(2 * (60 - Threads.speelSterkte / 2));

	for (int PV_index = 0; PV_index < Threads.multiPV; PV_index++)
	{
		RootZet zet = thread->rootZetten[PV_index];
		if (zet.pv[0] == GEEN_ZET)
			break;
		Waarde waarde = zet.score;
		waarde += random_score * (rng.rand<unsigned>() & 1023) / 1024;
		if (PV_index == 0 || waarde > max_waarde)
		{
			max_waarde = waarde;
			max_index = PV_index;
		}
	}
	if (max_index != 0)
		std::swap(thread->rootZetten[0], thread->rootZetten[max_index]);
}


void Search::pas_tijd_aan_na_ponderhit() {

	Tijdscontrole.aanpassing_na_ponderhit();
}


/// MainThread::search() is called by the main thread when the program receives
/// the UCI 'go' command. It searches from the root position and outputs the "bestmove".

void MainThread::search() {

  Running = true;
  rootStelling->kopieer_stelling(Threads.rootStelling, nullptr, nullptr);
  const Kleur ik = rootStelling->aan_zet();
  Tijdscontrole.init(Limits, ik, rootStelling->partij_ply(), rootStelling->partij_fase());
  vorigeInfoTijd = 0;
  interruptTeller = 0;

  StellingInfo* st = rootStelling->info();

  // remise scores rekening houdend met contempt
  Threads.analyseModus = !Limits.gebruikt_tijd_berekening();
  Threads.tactischeModus = Options["Tactical Mode"];
  Threads.vijftigZettenAfstand = Options["FiftyMoveDistance"];
  Threads.vijftigZettenAfstand = std::min(50, std::max(Threads.vijftigZettenAfstand, rootStelling->vijftig_zetten_teller() / 2 + 5));
  Threads.rootKleur = ik;
  Threads.stukContempt = (Threads.analyseModus && !Options["Analysis Contempt"]) ? 0 : int(Options["Contempt"]);
  Threads.multiPV = Options["MultiPV"];
  Threads.multiPV_cp = Options["MultiPV_cp"];
  if (Threads.multiPV_cp > 0)
	  Threads.multiPV = 2;  // beginwaarde, wordt bijgewerkt door algoritme
  if (Options["UCI_LimitStrength"])
	  Threads.speelSterkte = UCI::sterkte_voor_elo(int(Options["UCI_Elo"]) - 200);
  else
	  Threads.speelSterkte = Options["Strength"];

  Threads.activeThreadCount = Threads.threadCount;
  Threads.speelSterkteMaxKnopen = 0;
  if (Threads.speelSterkte != 100)
  {
	  Threads.activeThreadCount = 1;
	  Threads.speelSterkteMaxKnopen = 600 << (Threads.speelSterkte / 5);
	  if (Threads.speelSterkte <= 10)
		  Threads.multiPV = 20 - Threads.speelSterkte / 2;
	  else
		  Threads.multiPV = 15 - Threads.speelSterkte / 8;
  }

  if (Threads.analyseModus)
  {
	  RemiseWaarde[ik] = REMISE_WAARDE;
	  RemiseWaarde[~ik] = REMISE_WAARDE;
  }
  else
  {
	  RemiseWaarde[ik] = REMISE_WAARDE - DEFAULT_DRAW_VALUE * rootStelling->partij_fase() / MIDDENSPEL_FASE;
	  RemiseWaarde[~ik] = REMISE_WAARDE + DEFAULT_DRAW_VALUE * rootStelling->partij_fase() / MIDDENSPEL_FASE;
  }

  TT.nieuwe_generatie();
  TB_RootInTB = false;
  Tablebases::UseRule50 = Options["EGTB Fifty Move Rule"];
  TB_ProbeDepth = Options["EGTB Probe Depth"] * PLY;
  TB_Cardinality = std::max(Tablebases::MaxPieces_wdl, std::max(Tablebases::MaxPieces_dtm, Tablebases::MaxPieces_dtz));

  rootZetten.zetAantal = 0;
  for (const auto& zet : ZettenLijst<GEN_LEGALE_ZETTEN>(*rootStelling))
	  if (Limits.zoekZetten.empty() || Limits.zoekZetten.find(zet) >= 0)
		  rootZetten.add(RootZet(zet));

#if 1
  // voor analyse van stellingen
  // verwijder alle posities die al niet tweemaal zijn voorgevallen
  // om remise door tweevoudige zetherhaling in de beginpositie te vermijden
  // goede test positie: 
  // position fen r5kr/2qp1pp1/2b1pn2/p1b4p/1pN1P3/2PB3P/PP1BQPP1/R3R1K1 w - - 1 18 moves a1c1 c5f8 b2b3 d7d5 e4d5 c6d5 
  // c4e3 d5c6 c1c2 c7b6 c3b4 a5b4 e1c1 c6b7 e3c4 b6c5 d2f4 c5d4 f4e3 d4d5 f2f3 b7a6 c1d1 a8e8 e2f2 d5b7 e3d4 h8h6 d4e3
  if (Threads.analyseModus)
  {
	  StellingInfo* st = rootStelling->info();
	  int e = std::min(st->remise50Zetten, st->afstandTotNullzet);
	  while (e > 0)
	  {
		  st--;
		  e--;
		  StellingInfo* stst = st;
		  bool found = false;
		  for (int i = 2; i <= e; i += 2)
		  {
			  stst = stst - 2;
			  if (stst->sleutel == st->sleutel)
			  {
				  found = true;
				  break;
			  }
		  }

		  // position has occurred only once, remove it from the list
		  if (!found)
			  st->sleutel = 0;
	  }
  }
#endif

  if (rootZetten.zetAantal == 0)
  {
      rootZetten.add(RootZet(GEEN_ZET));
	  rootZetten[0].score = rootStelling->schaak_gevers() ? -MAT_WAARDE : REMISE_WAARDE;
	  rootZetten[0].diepte = MAIN_THREAD_INC;
	  sync_cout << UCI::pv(*rootStelling, -HOOGSTE_WAARDE, HOOGSTE_WAARDE) << sync_endl;
  }
  else
  {
      if (    rootStelling->alle_stukken_aantal() <= TB_Cardinality
          && !rootStelling->rokade_mogelijk(ALLE_ROKADE)
		  && Threads.speelSterkte == 100)
      {
          // If the current root position is in the tablebases, then RootMoves
          // contains only moves that preserve the draw or the win.
          FilterRootMoves(*rootStelling, rootZetten);

          if (TB_RootInTB && rootZetten.zetAantal == 1)
		  {
			  rootZetten[0].score = TB_Score;
			  rootZetten[0].diepte = MAIN_THREAD_INC;
			  sync_cout << UCI::pv(*rootStelling, -HOOGSTE_WAARDE, HOOGSTE_WAARDE) << sync_endl;
			  goto NO_ANALYSIS;
		  }
	  }

      #ifdef TRACE_LOG
	      open_tracefile();
      #endif

	  Threads.multiPV = std::min(Threads.multiPV, rootZetten.zetAantal);

	  if (Threads.activeThreadCount > 1)
	  {
		  Threads.rootZetten = rootZetten;
		  Threads.rootStellingInfo = rootStelling->info();
	  }

	  for (int i = 1; i < Threads.activeThreadCount; ++i)
		  Threads.threads[i]->start_zoeken();

      Thread::search(); // Let's start searching!

      #ifdef TRACE_LOG
	      close_tracefile();
      #endif
  }
NO_ANALYSIS:

  // When we reach the maximum depth, we can arrive here without a raise of
  // Signals.stop. However, if we are pondering or in an infinite search,
  // the UCI protocol states that we shouldn't print the best move before the
  // GUI sends a "stop" or "ponderhit" command. We therefore simply wait here
  // until the GUI sends one of those commands (which also raises Signals.stop).
  if (!Signals.stop && (Limits.ponder || Limits.infinite))
  {
      Signals.stopOnPonderhit = true;
      wait(Signals.stop);
  }

  // Stop the threads if not already stopped
  Signals.stop = true;

  // Wait until all threads have finished
  for (int i = 1; i < Threads.activeThreadCount; ++i)
	  Threads.threads[i]->wait_for_search_finished();

  // Check if there are threads with a better score than main thread
  Thread* bestThread = this;

  if (Threads.speelSterkte != 100)
	  kies_niet_te_sterke_zet(this);

  else if (!this->snelleZetGespeeld
      && Threads.multiPV == 1
      && !Limits.depth
      &&  rootZetten[0].pv[0] != GEEN_ZET)
  {
	  for (int i = 1; i < Threads.activeThreadCount; ++i)
	  {
		  Thread* th = Threads.threads[i];
		  if (   th->afgewerkteDiepte > bestThread->afgewerkteDiepte
			  && th->rootZetten[0].score > bestThread->rootZetten[0].score)
			  bestThread = th;
	  }
  }

  // bij fail high toch een PV tonen
  if (bestThread->rootZetten[0].pv.size() == 1)
	  bestThread->rootZetten[0].vervolledig_pv_met_tt(*bestThread->rootStelling);

  vorigeRootScore = bestThread->rootZetten[0].score;
  vorigeRootDiepte = bestThread->rootZetten[0].diepte;

  if (bestThread != this || Tijdscontrole.verstreken() <= 102)
  {
	  // gebruik diepte van de main Thread (om geen sprongen te hebben in de output)
	  bestThread->rootZetten[0].diepte = rootZetten[0].diepte;
	  sync_cout << UCI::pv(*bestThread->rootStelling, -HOOGSTE_WAARDE, HOOGSTE_WAARDE) << sync_endl;
  }
  sync_cout << "bestmove " << UCI::zet(bestThread->rootZetten[0].pv[0], *rootStelling);

  if (bestThread->rootZetten[0].pv.size() > 1 || bestThread->rootZetten[0].vind_ponder_zet_in_tt(*rootStelling))
      std::cout << " ponder " << UCI::zet(bestThread->rootZetten[0].pv[1], *rootStelling);

  std::cout << sync_endl;

  Running = false;
}


// Thread::search() is the main iterative deepening loop. It calls search()
// repeatedly with increasing depth until the allocated thinking time has been
// consumed, the user stops the search, or the maximum search depth is reached.

void Thread::search() {

  Waarde besteWaarde, alfa, beta, delta_alfa, delta_beta;
  Zet snelleZet = GEEN_ZET;
  MainThread* mainThread = (this == Threads.main() ? Threads.main() : nullptr);
  //Depth LAST_SMART_FAIL_HIGH_DIEPTE = 15 * PLY;

  if (!mainThread)
  {
	  rootStelling->kopieer_stelling(Threads.rootStelling, this, Threads.rootStellingInfo);
	  rootZetten = Threads.rootZetten;
  }

  StellingInfo* st = rootStelling->info();

  std::memset(st + 1, 0, 2 * sizeof(StellingInfo));
  st->killers[0] = st->killers[1] = GEEN_ZET;
  st->vorigeZet = GEEN_ZET;
  (st - 2)->stellingWaarde = WAARDE_0;
  (st - 1)->stellingWaarde = WAARDE_0;
  (st - 1)->evalPositioneel = GEEN_EVAL;
  (st - 1)->zetNummer = 0;
  (st - 4)->counterZetWaarden = (st - 3)->counterZetWaarden = (st - 2)->counterZetWaarden = (st - 1)->counterZetWaarden = st->counterZetWaarden = nullptr;
  (st - 1)->mp_eindeLijst = rootStelling->ti()->zettenLijst;

  for (int n = 0; n <= MAX_PLY; n++)
  {
	  (st + n)->doeGeenVroegePruning = false;
	  (st + n)->uitgeslotenZet = GEEN_ZET;
	  (st + n)->lmr_reductie = 0;
	  (st + n)->ply = n + 1;
  }

  besteWaarde = delta_alfa = delta_beta = alfa = -HOOGSTE_WAARDE;
  beta = HOOGSTE_WAARDE;
  afgewerkteDiepte = 0 * PLY;

  if (mainThread)
  {
      snelleZet = EasyMove.get(rootStelling->sleutel());
      EasyMove.clear();
      mainThread->snelleZetGespeeld = mainThread->failedLow = false;
      mainThread->besteZetVerandert = 0;
	  mainThread->snelleZetGetest = false;

	  // set all st->pionSleutel values to 0 so that the maximum ply depth can be examined later for the "seldepth" UCI output
	  mainSearchStack = st;
	  for (int i = 1; i <= MAX_PLY; i++)
		  (mainSearchStack + i)->pionSleutel = 0;
  }

#if 1
  if (mainThread && !TB_RootInTB && !Limits.ponder && Threads.speelSterkte == 100 && !Threads.analyseModus
	  && mainThread->snelleZetToegelaten && mainThread->vorigeRootDiepte >= 12 * PLY && Threads.multiPV == 1)
  {
	  bool ttGevonden;
	  TTTuple* tte = TT.zoek(rootStelling->sleutel(), ttGevonden);
	  if (ttGevonden && tte->limiet() == EXACTE_WAARDE)
	  {
		  Waarde ttWaarde = ttGevonden ? waarde_uit_tt(tte->waarde(), st->ply) : GEEN_WAARDE;
		  Zet ttMove = tte->zet();
		  Diepte ttDiepte = tte->diepte();
		  
		  if (ttDiepte >= mainThread->vorigeRootDiepte - 3 * PLY
			  && ttMove
			  && rootStelling->legale_zet(ttMove)
			  && abs(ttWaarde) < WINST_WAARDE )
		  {
			  Diepte depth_singular = std::max(mainThread->vorigeRootDiepte / 2, mainThread->vorigeRootDiepte - 8 * PLY);
			  Waarde v_singular = ttWaarde - maak_waarde(128);
			  st->uitgeslotenZet = ttMove;
			  st->doeGeenVroegePruning = true;
			  Waarde v = ::search<NonPV>(*rootStelling, v_singular - WAARDE_1, v_singular, depth_singular, false);
			  st->uitgeslotenZet = GEEN_ZET;
			  st->doeGeenVroegePruning = false;

			  if (v < v_singular)
			  {
				  Signals.stop = true;
				  rootZetten[0].score = ttWaarde;
				  rootZetten[0].pv.resize(1);
				  rootZetten[0].pv[0] = ttMove;
				  rootZetten[0].vervolledig_pv_met_tt(*rootStelling);
				  rootZetten[0].diepte = ttDiepte;
				  mainThread->snelleZetToegelaten = false;
				  mainThread->snelleZetGespeeld = true;
				  EasyMove.clear();
				  afgewerkteDiepte = mainThread->vorigeRootDiepte - 2 * PLY;
				  sync_cout << UCI::pv(*rootStelling, -HOOGSTE_WAARDE, HOOGSTE_WAARDE) << sync_endl;
				  return;
			  }
		  }
	  }
  }
  if (mainThread)
	  mainThread->snelleZetToegelaten = true;
#endif

  if (Threads.tactischeModus)
  {
	  vorigeTactischeDiepte = 0 * PLY;

	  // bereken de initiele score van elke zet
	  st->mp_eindeLijst = (st - 1)->mp_eindeLijst;
	  Zet pv[MAX_PLY + 1];

	  for (int i = 0; i < rootZetten.zetAantal; i++)
	  {
		  Zet zet = rootZetten[i].pv[0];
		  rootStelling->speel_zet(zet);
		  (st + 1)->doeGeenVroegePruning = true;
		  (st + 1)->pv = pv;
		  (st + 1)->pv[0] = GEEN_ZET;
		  rootZetten[i].startWaarde = -::search<PV>(*rootStelling, -HOOGSTE_WAARDE, HOOGSTE_WAARDE, PLY, false);
		  (st + 1)->doeGeenVroegePruning = false;
		  rootStelling->neem_zet_terug(zet);
	  }
  }

  int rootIteration = 0;
  Diepte rootDiepte = PLY / 2;

  // Iterative deepening loop until requested to stop or the target depth is reached.
  while (++rootIteration < 100)
  {
	  rootDiepte += mainThread ? MAIN_THREAD_INC : OTHER_THREAD_INC;
	  //if (rootDepth < MAIN_THREAD_INC)
		 // rootDepth = MAIN_THREAD_INC;

	  if (mainThread)
	  {
		  if (Limits.depth && displayIteration(rootIteration - 1) >= Limits.depth)
			  Signals.stop = true;

		  if (Threads.speelSterkte != 100 && Threads.bezochte_knopen() >= Threads.speelSterkteMaxKnopen)
			  Signals.stop = true;
	  }

	  if (Signals.stop)
		  break;

      // Set up the new depths for the helper threads skipping on average every
      // 2nd ply (using a half-density matrix).
      if (!mainThread)
      {
          const Row& row = HalfDensity[(threadIndex - 1) % HalfDensitySize];
          if (row[(rootIteration + rootStelling->partij_ply()) % row.size()])
             continue;
      }

      // Age out PV variability metric
	  if (mainThread)
	  {
		  mainThread->besteZetVerandert /= 2;
		  mainThread->failedLow = false;
	  }

	  if (mainThread && Tijdscontrole.verstreken() > 1000)
		  sync_cout << "info depth " << displayIteration(rootIteration) << sync_endl;

      // Save the last iteration's scores before first PV line is searched and
      // all the move scores except the (new) PV are set to -HOOGSTE_WAARDE.
	  for (int i = 0; i < rootZetten.zetAantal; i++)
          rootZetten[i].vorigeRootScore = rootZetten[i].score;

      // MultiPV loop. We perform a full root search for each PV line
      for (actievePV = 0; actievePV < Threads.multiPV && !Signals.stop; ++actievePV)
      {
		  Zet prevBestMove = rootZetten[actievePV].pv[0];
		  int failHighCount = 0;

          // Reset aspiration window starting size
          if (rootDiepte >= 5 * PLY)
          {
              delta_alfa = maak_waarde(18);
			  delta_beta = maak_waarde(18);
			  alfa = std::max(rootZetten[actievePV].vorigeRootScore - maak_waarde(18), -HOOGSTE_WAARDE);
              beta  = std::min(rootZetten[actievePV].vorigeRootScore + maak_waarde(18), HOOGSTE_WAARDE);
          }  

          // Start with a small aspiration window and, in the case of a fail
          // high/low, re-search with a bigger window until we're not failing
          // high/low anymore.
          while (true)
          {
#if 1
			  if (alfa < -20 * WAARDE_PION)
				  alfa = -HOOGSTE_WAARDE;
			  if (beta > 20 * WAARDE_PION)
				  beta = HOOGSTE_WAARDE;
#endif

//#define SMART_FAIL_HIGH
#ifdef SMART_FAIL_HIGH
			  if (failHighCount == 2 && rootZetten[actievePV].pv[0] != prevBestMove && rootDiepte > LAST_SMART_FAIL_HIGH_DIEPTE)
			  {
				  LAST_SMART_FAIL_HIGH_DIEPTE = rootDiepte;
				  bool showOutput = true;// mainThread && Time.elapsed() > 4000;
				  if (showOutput)
					  sync_cout << "info string smart fail high - attempt reducing search depth to " << displayIteration(rootIteration) - 3 << sync_endl;

				  Zet m = rootZetten[actievePV].pv[0];
				  Zet pv[MAX_PLY + 1];
				  (st + 1)->pv = pv;

				  rootStelling->speel_zet(m);

				  for (int diepte_offset = 3; diepte_offset >= 1; --diepte_offset)
				  {
					  besteWaarde = -::search<PV>(*rootStelling, -beta, -alfa, rootDiepte - PLY - diepte_offset * PLY, false);

					  if (Signals.stop)
						  break;

					  if (besteWaarde > alfa)
					  {
						  if (showOutput)
							  sync_cout << "info string smart fail high successful" << sync_endl;
						  rootDiepte -= diepte_offset * PLY;
						  rootIteration -= diepte_offset;
						  break;
					  }
				  };

				  rootStelling->neem_zet_terug(m);
				  if (Signals.stop)
					  break;
			  }
#endif

			  tactischeModusGebruikt = false;

              besteWaarde = ::search<PV>(*rootStelling, alfa, beta, rootDiepte, false);

              // Bring the best move to the front. It is critical that sorting
              // is done with a stable algorithm because all the values but the
              // first and eventually the new best one are set to -HOOGSTE_WAARDE
              // and we want to keep the same order for all the moves except the
              // new PV that goes to the front. Note that in case of MultiPV
              // search the already searched PV lines are preserved.
              std::stable_sort(rootZetten.zetten + actievePV, rootZetten.zetten + rootZetten.zetAantal);

              // If search has been stopped, break immediately. Sorting and
              // writing PV back to TT is safe because RootMoves is still
              // valid, although it refers to the previous iteration.
              if (Signals.stop)
                  break;

			  if (tactischeModusGebruikt)
				  vorigeTactischeDiepte = rootDiepte;

			  bool ignoreFailHigh = !mainThread;

			  if (mainThread && besteWaarde >= beta
				  && rootZetten[actievePV].pv[0] == prevBestMove
				  && !Threads.analyseModus
				  && Tijdscontrole.verstreken() > Tijdscontrole.optimum() * 124 / 1024)
			  {
				  bool doEasyMove = rootZetten[0].pv[0] == snelleZet && mainThread->besteZetVerandert < 31;

				  if (doEasyMove)
					  ignoreFailHigh = true;

				  else if (Tijdscontrole.verstreken() > Tijdscontrole.optimum() * 420 / 1024)
				  {
					  const int F[] = { mainThread->failedLow, besteWaarde - mainThread->vorigeRootScore };
					  int improvingFactor = std::max(420, std::min(1304, 652 + 160 * F[0] - 12 * F[1]));
					  int unstablePvFactor = 1024 + mainThread->besteZetVerandert;
					  if (Tijdscontrole.verstreken() > Tijdscontrole.optimum() * unstablePvFactor / 1024 * improvingFactor / 1024)
						  ignoreFailHigh = true;
				  }
			  }

              // In case of failing low/high increase aspiration window and
              // re-search, otherwise exit the loop.
              if (besteWaarde <= alfa)
              {
                  beta = (alfa + beta) / 2;
                  alfa = std::max(besteWaarde - delta_alfa, -HOOGSTE_WAARDE);
				  failHighCount = 0;

                  if (mainThread)
                  {
                      mainThread->failedLow = true;
                      Signals.stopOnPonderhit = false;
                  }
              }
              else if (besteWaarde >= beta && !ignoreFailHigh)
              {
                  alfa = (alfa + beta) / 2;
                  beta = std::min(besteWaarde + delta_beta, HOOGSTE_WAARDE);
				  ++failHighCount;
              }
              else
                  break;

			  delta_alfa += delta_alfa / 4 + maak_waarde(5);
			  delta_beta += delta_beta / 4 + maak_waarde(5);

              assert(alfa >= -HOOGSTE_WAARDE && beta <= HOOGSTE_WAARDE);
          }

          // Sort the PV lines searched so far and update the GUI
          std::stable_sort(rootZetten.zetten + 0, rootZetten.zetten + actievePV + 1);

		  if (mainThread)
		  {
			  if (Threads.multiPV_cp > 0 && actievePV >= 1)
			  {
				  if (rootZetten[0].score - rootZetten[actievePV].score > 2 * Threads.multiPV_cp)
					  Threads.multiPV = actievePV + 1;
				  else
					  Threads.multiPV = actievePV + 2;
			  }

			  if (actievePV + 1 == Threads.multiPV && Tijdscontrole.verstreken() > 100 || Tijdscontrole.verstreken() > 4000)
				  sync_cout << UCI::pv(*rootStelling, alfa, beta) << sync_endl;
		  }
      }

      if (!Signals.stop)
		  afgewerkteDiepte = rootDiepte;

      if (!mainThread)
          continue;

      // Have we found a "mate in x"?
      if (   Limits.mate
          && besteWaarde >= LANGSTE_MAT_WAARDE
          && MAT_WAARDE - besteWaarde <= 2 * Limits.mate)
          Signals.stop = true;

	  if (!Threads.analyseModus && !Limits.ponder && besteWaarde > MAT_WAARDE - 32
			&& rootDiepte >= (MAT_WAARDE - besteWaarde + 10) * PLY)
		  Signals.stop = true;

	  if (!Threads.analyseModus && !Limits.ponder && besteWaarde < -MAT_WAARDE + 32
		  && rootDiepte >= (MAT_WAARDE + besteWaarde + 10) * PLY)
		  Signals.stop = true;

      // Do we have time for the next iteration? Can we stop searching now?
      if (!Threads.analyseModus)
      {
          if (!Signals.stop && !Signals.stopOnPonderhit)
          {
              // Stop the search if only one legal move is available, or if all
              // of the available time has been used, or if we matched an easyMove
              // from the previous search and just did a fast verification.
              const int F[] = { mainThread->failedLow,
                                besteWaarde - mainThread->vorigeRootScore };

			  int improvingFactor = std::max(420, std::min(1304, 652 + 160 * F[0] - 12 * F[1]));
              int unstablePvFactor = 1024 + mainThread->besteZetVerandert;

              bool doEasyMove =   rootZetten[0].pv[0] == snelleZet
                               && mainThread->besteZetVerandert < 31
                               && Tijdscontrole.verstreken() > Tijdscontrole.optimum() * 124 / 1024;

              if (   rootZetten.zetAantal == 1
				  || Tijdscontrole.verstreken() > Tijdscontrole.optimum() * unstablePvFactor / 1024 * improvingFactor / 1024
                  || (mainThread->snelleZetGespeeld = doEasyMove))
              {
                  // If we are allowed to ponder do not stop the search now but
                  // keep pondering until the GUI sends "ponderhit" or "stop".
                  if (Limits.ponder)
                      Signals.stopOnPonderhit = true;
                  else
                      Signals.stop = true;
#ifdef TRACE_TM
				  trace_tm_msg(1 + rootStelling->partij_ply() / 2, Time.verstreken(), Time.optimum(), mainThread->snelleZetGespeeld ? "Easy Move" : "");
#endif
			  }
#if 0
			  else if (rootIteration >= 12 && mainThread && !mainThread->snelleZetGetest
				  && (Time.verstreken() >= Time.optimum() / 4 || rootIteration >= mainThread->vorige_root_iteration - 1))
			  {
				  mainThread->snelleZetGetest = true;

				  int iteration_singular = std::max(rootIteration / 2, rootIteration - 8);
				  Waarde v_singular = besteWaarde - maak_waarde(400);
				  st->uitgeslotenZet = rootZetten[0].pv[0];
				  st->doeGeenVroegePruning = true;
				  Waarde v = ::search<NonPV>(rootStelling, v_singular-1, v_singular, rootDepthForIteration(iteration_singular), false);
				  st->uitgeslotenZet = GEEN_ZET;
				  st->doeGeenVroegePruning = false;

				  if (v < v_singular)
				  {
					  if (Limits.ponder)
						  Signals.stopOnPonderhit = true;
					  else
						  Signals.stop = true;
				  }
			  }
#endif
          }

          if (rootZetten[0].pv.size() >= 3)
              EasyMove.update(*rootStelling, rootZetten[0].pv);
          else
              EasyMove.clear();
      }
  }

  if (!mainThread)
      return;

  // Clear any candidate easy move that wasn't stable for the last search
  // iterations; the second condition prevents consecutive fast moves.
  if (EasyMove.stableCnt < 6 || mainThread->snelleZetGespeeld)
      EasyMove.clear();
}


namespace {

  // search<>() is the main search function for both PV and non-PV nodes

  template <NodeType NT>
  Waarde search(Stelling& pos, Waarde alfa, Waarde beta, Diepte diepte, bool cutNode) {

    const bool PvNode = NT == PV;

    assert(-HOOGSTE_WAARDE <= alfa && alfa < beta && beta <= HOOGSTE_WAARDE);
    assert(PvNode || (alfa == beta - 1));
    assert(diepte >= PLY && diepte < MAX_DIEPTE);

    Zet pv[MAX_PLY+1], rustigeZetten[64];
    TTTuple* ttt;
    Sleutel64 sleutel64;
    Zet ttZet, zet, besteZet;
    Diepte verlenging, nieuweDiepte, voorspeldeDiepte;
	Waarde besteWaarde, waarde, ttWaarde, eval;
	bool ttGevonden, staatSchaak, geeftSchaak, vooruitgang;
	bool slagOfPromotie, volledigZoekenNodig;
	Stuk bewogenStuk;
    int zetNummer, rustigeZetAantal;

#ifdef TRACE_LOG
	trace_msg("Search", diepte, alfa, beta);
#endif

	StellingInfo* st = pos.info();
	const bool rootKnoop = PvNode && st->ply == 1;

    // Step 1. Initialize node
    Thread* mijnThread = pos.mijn_thread();
    staatSchaak = pos.schaak_gevers();
    zetNummer = rustigeZetAantal = st->zetNummer = 0;
    besteWaarde = -HOOGSTE_WAARDE;

    // Check for the available remaining time
    if (mijnThread == Threads.main() && ++static_cast<MainThread*>(mijnThread)->interruptTeller >= 4096)
    {
        tijdscontrole();
		static_cast<MainThread*>(mijnThread)->interruptTeller = 0;
    }

    if (!rootKnoop)
    {
        // Step 2. Check for aborted search and immediate draw
        if (Signals.stop.load(std::memory_order_relaxed) || st->zetHerhaling || st->ply >= MAX_PLY)
            return st->ply >= MAX_PLY && !staatSchaak ? evaluate(pos, GEEN_WAARDE, GEEN_WAARDE)
                                                  : RemiseWaarde[pos.aan_zet()];

        // Step 3. Mate distance pruning. Even if we mate at the next move our score
        // would be at best mate_in(st->ply+1), but if alpha is already bigger because
        // a shorter mate was found upward in the tree then there is no need to search
        // because we will never beat the current alpha. Same logic but with reversed
        // signs applies also in the opposite condition of being mated instead of giving
        // mate. In this case return a fail-high score.
        alfa = std::max(staat_mat(st->ply), alfa);
        beta = std::min(geeft_mat(st->ply + 1), beta);
        if (alfa >= beta)
            return alfa;
    }

    assert(1 <= st->ply && st->ply < MAX_PLY);

    besteZet = GEEN_ZET;
	(st + 2)->killers[0] = (st + 2)->killers[1] = GEEN_ZET;

    // Step 4. Transposition table lookup. We don't want the score of a partial
    // search to overwrite a previous full search TT value, so we use a different
    // position key in case of an excluded move.
	sleutel64 = st->uitgeslotenZet ? st->sleutel ^ pos.uitzondering_sleutel(st->uitgeslotenZet) : st->sleutel;
	sleutel64 ^= pos.remise50_sleutel();
    ttt = TT.zoek(sleutel64, ttGevonden);
    ttWaarde = ttGevonden ? waarde_uit_tt(ttt->waarde(), st->ply) : GEEN_WAARDE;
    ttZet = rootKnoop ? mijnThread->rootZetten[mijnThread->actievePV].pv[0]
            : ttGevonden ? ttt->zet() : GEEN_ZET;

    // At non-PV nodes we check for an early TT cutoff
    if (  !PvNode
        && ttGevonden
        && ttt->diepte() >= diepte
        && ttWaarde != GEEN_WAARDE // Possible in case of TT access race
        && (ttWaarde >= beta ? (ttt->limiet() & ONDERGRENS)
                            : (ttt->limiet() & BOVENGRENS)))
    {
        // If ttMove is quiet, update killers, history, counter move on TT hit
        if (ttWaarde >= beta && ttZet)
            update_stats(pos, staatSchaak, ttZet, diepte, nullptr, 0);

        return ttWaarde;
    }

    // Step 4a. Tablebase probe
    if (!rootKnoop && TB_Cardinality && diepte >= TB_ProbeDepth && pos.materiaal_of_rokade_gewijzigd())
    {
		int aantalStukken = pos.alle_stukken_aantal();

        if (    aantalStukken <= TB_Cardinality
            && (/*aantalStukken < TB_Cardinality ||*/ diepte >= TB_ProbeDepth + BerekenEgtbNut(pos))
            && !pos.rokade_mogelijk(ALLE_ROKADE))
        {
			if (aantalStukken <= Tablebases::MaxPieces_wdl)
				waarde = Tablebases::EGTB_probe_wdl(pos, alfa, beta);
			else if (aantalStukken <= Tablebases::MaxPieces_dtm)
				waarde = Tablebases::EGTB_probe_dtm(pos, alfa, beta);
			else
				waarde = GEEN_WAARDE;

            if (waarde != GEEN_WAARDE)
            {
                ttt->bewaar(sleutel64, waarde_voor_tt(waarde, st->ply), EXACTE_WAARDE,
                          std::min(MAX_DIEPTE - PLY, diepte + 6 * PLY),
                          GEEN_ZET, GEEN_WAARDE, TT.generatie());

                return waarde;
            }
#if 0  
			// dit is Joseph Ellis manier om TB scores eventueel te laten overschrijven door een gevonden mat 
			// zie https://github.com/official-stockfish/Stockfish/compare/master...jhellis3:tb_fix_2
			if (waarde == REMISE_WAARDE
				|| (!ttGevonden || ((waarde < REMISE_WAARDE && ttWaarde > -WINST_WAARDE) || (waarde > REMISE_WAARDE && ttWaarde < WINST_WAARDE))))
			{
				waarde = waarde < REMISE_WAARDE ? -LANGSTE_MAT_WAARDE + PLY + st->ply
					: waarde >  REMISE_WAARDE ? LANGSTE_MAT_WAARDE - PLY - st->ply
					: REMISE_WAARDE;

				ttt->bewaar(sleutel64, waarde_voor_tt(waarde, st->ply),
					waarde > REMISE_WAARDE ? ONDERGRENS : waarde < REMISE_WAARDE ? BOVENGRENS : EXACTE_WAARDE,
					diepte, GEEN_ZET, GEEN_WAARDE, TT.generatie());

				if (waarde == REMISE_WAARDE)
					return waarde;
			}
#endif
		}
    }

    // Step 5. Evaluate the position statically
	if (staatSchaak)
    {
        st->stellingWaarde = eval = GEEN_WAARDE;
        goto moves_loop;
    }

    else if (ttGevonden)
    {
        // Never assume anything on values stored in TT
        if ((st->stellingWaarde = eval = ttt->eval()) == GEEN_WAARDE)
            eval = st->stellingWaarde = evaluate(pos, GEEN_WAARDE, GEEN_WAARDE);

        // Can ttWaarde be used as a better position evaluation?
        if (ttWaarde != GEEN_WAARDE)
            if (ttt->limiet() & (ttWaarde > eval ? ONDERGRENS : BOVENGRENS))
                eval = ttWaarde;
    }
    else
    {
        eval = st->stellingWaarde =
			st->vorigeZet != NULL_ZET ? evaluate(pos, GEEN_WAARDE, GEEN_WAARDE)
                                         : -(st - 1)->stellingWaarde + 2 * WAARDE_TEMPO;

		ttt->bewaar(sleutel64, GEEN_WAARDE, GEEN_LIMIET, GEEN_DIEPTE, GEEN_ZET,
			st->stellingWaarde, TT.generatie());
    }
#ifdef TRACE_LOG
	trace_eval(st->stellingWaarde);
#endif

	if (st->vorigeZet != NULL_ZET 
		&& st->stellingWaarde != GEEN_WAARDE && (st - 1)->stellingWaarde != GEEN_WAARDE 
		&& st->materiaalSleutel == (st - 1)->materiaalSleutel)
		pos.ti()->maxWinstTabel.update(pos.stuk_op_veld(naar_veld(st->vorigeZet)), st->vorigeZet, -st->stellingWaarde - (st - 1)->stellingWaarde + 2 * WAARDE_TEMPO);

    if (st->doeGeenVroegePruning)
		goto skip_early;

    // Step 6. Razoring (skipped when in check)
    if (   !PvNode
        &&  diepte < 4 * PLY
		&&  ttZet == GEEN_ZET
		&&  eval + razer_margin(diepte) <= alfa)
    {
        if (   diepte < 2 * PLY)
            return qsearch<NonPV, false>(pos, alfa, beta, DIEPTE_0);

        Waarde ralpha = alfa - razer_margin(diepte);
        Waarde v = qsearch<NonPV, false>(pos, ralpha, ralpha + WAARDE_1, DIEPTE_0);
        if (v <= ralpha)
            return v;
    }

	// Step 7. Futility pruning: child node (skipped when in check)
    if (   !rootKnoop
        &&  diepte < 7 * PLY
        &&  eval - futility_marge1(diepte) >= beta
        &&  eval < WINST_WAARDE  // Do not return unproven wins
        &&  pos.niet_pion_materiaal(pos.aan_zet()))
        return eval - futility_marge1(diepte);
        //return eval;

	// Step 8. Null move search with verification search (is omitted in PV nodes)
	if (   !PvNode
        &&  diepte >= 2 * PLY
		&&  eval >= beta + 2 * WAARDE_TEMPO
		&& (st->stellingWaarde >= beta || diepte >= 12 * PLY)
		&&  pos.niet_pion_materiaal(pos.aan_zet())
		&& (!Threads.analyseModus || diepte < 8 * PLY || Eval::minstens_twee_mobiele_stukken(pos))
		)   // goede test positie voor minstens_twee_mobiele_stukken: 8/6B1/p5p1/Pp4kp/1P5r/5P1Q/4q1PK/8 w - - 0 32 bm Qxh4
    {
		assert(eval - beta >= 0);

        // Null move dynamic reduction based on depth and value
		Diepte R;
		if (Threads.tactischeModus)
			R = diepte < 4 * PLY ? diepte :
			(540 + 66 * ((unsigned int)diepte / PLY)
				+ std::max(std::min(310 * (eval - beta) / maak_waarde(256) - 20 - 15 * cutNode - 15 * (ttZet != GEEN_ZET), 3 * 256), 0)
				) / 256 * PLY;
		else
			R = diepte < 4 * PLY ? diepte :
			(480 + 76 * ((unsigned int)diepte / PLY)
				+ std::max(std::min(310 * (eval - beta) / maak_waarde(256) - 20 - 15 * cutNode - 15 * (ttZet != GEEN_ZET), 3 * 256), 0)
				) / 256 * PLY;

		st->mp_eindeLijst = (st - 1)->mp_eindeLijst;
		pos.speel_null_zet();
		(st + 1)->doeGeenVroegePruning = true;
        waarde = diepte - R < PLY ? -qsearch<NonPV, false>(pos, -beta, -beta + WAARDE_1, DIEPTE_0)
                                  : -search<NonPV>(pos, -beta, -beta + WAARDE_1, diepte - R, !cutNode);
        (st + 1)->doeGeenVroegePruning = false;
        pos.neem_null_terug();

		if ((st - 1)->lmr_reductie && (waarde < beta - maak_waarde(125) || waarde < -LANGSTE_MAT_WAARDE))
		{
			if ((st - 1)->lmr_reductie <= 2 * PLY)
				return beta - WAARDE_1;
			diepte += 2 * PLY;

			//int delta = std::min(int((st - 1)->lmr_reductie), int(2 * PLY));
			//(st - 1)->lmr_reductie -= delta;
			//diepte += Diepte(delta);
	}

        if (waarde >= beta)
        {
            // Do not return unproven mate scores
            if (waarde >= LANGSTE_MAT_WAARDE)
                waarde = beta;

            if (diepte < 12 * PLY && abs(beta) < WINST_WAARDE)
                return waarde;

            // Do verification search at high depths
            st->doeGeenVroegePruning = true;
            Waarde v = diepte - R < PLY ? qsearch<NonPV, false>(pos, beta - WAARDE_1, beta, DIEPTE_0)
                                        : search<NonPV>(pos, beta - WAARDE_1, beta, diepte - R, false);
            st->doeGeenVroegePruning = false;

            if (v >= beta)
                return waarde;
        }
    }
	// Step 8.5. Probeer null move om een dreiging te achterhalen om eventueel LMR te verminderen
	/*
	else if (diepte >= 6 * PLY && eval >= beta && (st - 1)->lmr_reductie)
	{
		st->mp_eindeLijst = (st - 1)->mp_eindeLijst;
		pos.speel_null_zet();
		(st + 1)->doeGeenVroegePruning = true;
		Waarde nullBeta = beta - maak_waarde(300);
		waarde = -search<NonPV>(pos, -nullBeta, -nullBeta + WAARDE_1, diepte / 2 - 2 * PLY, false);
		(st + 1)->doeGeenVroegePruning = false;
		pos.neem_null_terug();

		if (waarde < nullBeta)
		{
			if ((st - 1)->lmr_reductie <= 2 * PLY)
				return beta - WAARDE_1;
			diepte += 2 * PLY;
		}
	}
	//*/

    // Step 9. ProbCut (skipped when in check)
    // If we have a very good capture (i.e. SEE > seeValues[captured_piece_type])
    // and a reduced search returns a value much above beta, we can (almost)
    // safely prune the previous move.
	if (   !PvNode
		&&  diepte >= 5 * PLY
        &&  abs(beta) < LANGSTE_MAT_WAARDE)
    {
		Waarde rbeta = beta + maak_waarde(200);
        Diepte rdepth = diepte - 4 * PLY;

        assert(rdepth >= PLY);
        assert(st->vorigeZet != GEEN_ZET);
        assert(st->vorigeZet != NULL_ZET);

		PikZet::init_probcut(pos, ttZet, pos.see_waarden()[pos.geslagen_stuk()]);

		while ((zet = PikZet::geef_zet(pos)) != GEEN_ZET)
		{
			//TT.prefetchEntry(pos.key_after(move));
			if (pos.legale_zet(zet))
			{
				pos.speel_zet(zet);
				waarde = -search<NonPV>(pos, -rbeta, -rbeta + WAARDE_1, rdepth, !cutNode);
				pos.neem_zet_terug(zet);
				if (waarde >= rbeta)
					return waarde;
			}
		}
	}

skip_early:

    // Step 10. Internal iterative deepening (skipped when in check)
	if (    diepte >= (PvNode ? 5 * PLY : 8 * PLY)
        && !ttZet
		//&& (!ttMove || (PvNode && tte->depth() < depth - 4 * PLY))
		//&& (PvNode || st->staticEval + 256 >= beta))
		&& (PvNode || cutNode || st->stellingWaarde + maak_waarde(128) >= beta))
    {
		Diepte d = diepte - 2 * PLY - (PvNode ? DIEPTE_0 : ((unsigned int)diepte / PLY) / 4 * PLY);
		st->doeGeenVroegePruning = true;
		search<NT>(pos, alfa, beta, d, !PvNode && cutNode);
        st->doeGeenVroegePruning = false;

        ttt = TT.zoek(sleutel64, ttGevonden);
        ttZet = ttGevonden ? ttt->zet() : GEEN_ZET;
    }

moves_loop: // When in check search starts from here

	const CounterZetWaarden* cmh = st->counterZetWaarden;
	const CounterZetWaarden* fmh = (st - 1)->counterZetWaarden;
	const CounterZetWaarden* fmh2 = (st - 3)->counterZetWaarden;

	PikZet::init_search(pos, ttZet, diepte);
    waarde = besteWaarde; // Workaround a bogus 'uninitialized' warning under gcc
    vooruitgang = st->stellingWaarde >= (st - 2)->stellingWaarde
            /* || st->stellingWaarde == GEEN_WAARDE Already implicit in the previous condition */
               ||(st - 2)->stellingWaarde == GEEN_WAARDE;

	int LateMoveCount = diepte < 16 * PLY ? futility_move_count(diepte, vooruitgang) : 999;

    // Step 11. Loop through moves
    // Loop through all pseudo-legal moves until no moves remain or a beta cutoff occurs
    while ((zet = PikZet::geef_zet(pos)) != GEEN_ZET)
    {
      assert(is_ok(zet));
	  assert(stuk_kleur(pos.bewogen_stuk(zet)) == pos.aan_zet());

      if (zet == st->uitgeslotenZet)
          continue;

      // At root obey the "searchmoves" option and skip moves not listed in Root
      // Move List. As a consequence any illegal move is also skipped. In MultiPV
      // mode we also skip PV moves which have been already searched.
	  if (rootKnoop && mijnThread->rootZetten.find(zet) < mijnThread->actievePV)
		  continue;

      st->zetNummer = ++zetNummer;

	  if (rootKnoop && mijnThread == Threads.main() && Tijdscontrole.verstreken() > 4000)
          sync_cout << "info currmove " << UCI::zet(zet, pos)
                    << " currmovenumber " << zetNummer + mijnThread->actievePV << sync_endl;

      if (PvNode)
          (st + 1)->pv = nullptr;

      verlenging = DIEPTE_0;
      slagOfPromotie = pos.slag_of_promotie(zet);
	  bewogenStuk = pos.bewogen_stuk(zet);

      geeftSchaak = zet_type(zet) == NORMAAL && !pos.aftrek_schaak_mogelijk()
                  ? st->schaakVelden[stuk_type(bewogenStuk)] & naar_veld(zet)
                  : pos.geeft_schaak(zet);

      // Step 12. Extensions
	  if (   geeftSchaak
		  && zetNummer < LateMoveCount
		  && (st->mp_etappe == GOEDE_SLAGEN || pos.see_test(zet, SEE_0)))
          verlenging = PLY;
	  //else if (PvNode && move == ttMove && slagOfPromotie && st->ply * 4 * PLY <= depth)
		 // verlenging = PLY / 2;

	  if (rootKnoop && Threads.tactischeModus)
	  {
		  if (slagOfPromotie || geeftSchaak || pos.vooruitgeschoven_pion(zet))
			  verlenging = PLY;
	  }

      // Singular extension search. If all moves but one fail low on a search of
      // (alpha-s, beta-s), and just one fails high on (alpha, beta), then that move
      // is singular and should be extended. To verify this we do a reduced search
      // on all the other moves but the ttMove and if the result is lower than
      // ttWaarde minus a margin then we extend the ttMove.
      if (    zet == ttZet
		  &&  diepte >= 8 * PLY
		  && (ttt->limiet() & ONDERGRENS)
		  && ttt->diepte() >= diepte - 3 * PLY
		  && verlenging < PLY
		  && !rootKnoop
		  /*  &&  ttWaarde != GEEN_WAARDE Already implicit in the next condition */
		  &&  abs(ttWaarde) < WINST_WAARDE
		  && !st->uitgeslotenZet // Recursive singular search is not allowed
          &&  pos.legale_zet(zet))
      {
		  // bewaar PikZet state
		  Zet cm = st->mp_counterZet;

		  Waarde rBeta = ttWaarde - maak_waarde(2 * ((unsigned int)diepte / PLY));
		  Diepte rDepth = (diepte / PLY) / 2 * PLY;
          st->uitgeslotenZet = zet;
          st->doeGeenVroegePruning = true;

		  waarde = search<NonPV>(pos, rBeta - WAARDE_1, rBeta, rDepth, !PvNode && cutNode);
		  if (waarde < rBeta)
			  verlenging = PLY;

		  st->doeGeenVroegePruning = false;
          st->uitgeslotenZet = GEEN_ZET;

		  // herstel PikZet state
		  PikZet::init_search(pos, ttZet, diepte);
		  st->mp_counterZet = cm;
		  ++st->mp_etappe;
		  // zetNummer terugzetten (hier altijd 1)
		  st->zetNummer = zetNummer;
	  }

      // Update the current move (this must be done after singular extension search)
      nieuweDiepte = diepte - PLY + verlenging;

	  // Step 13. Pruning at shallow depth
      if (   !(rootKnoop | slagOfPromotie | geeftSchaak)
		  && besteWaarde > -LANGSTE_MAT_WAARDE
		  && !pos.vooruitgeschoven_pion(zet))
      {
		  // Move count based pruning
          if (zetNummer >= LateMoveCount)
              continue;

		  // Countermoves based pruning
		  if (diepte < 6 * PLY
			  && st->mp_etappe > RUSTIGE_ZETTEN_GEN)
		  {
			  int offset = CounterZetWaarden::bereken_offset(bewogenStuk, naar_veld(zet));
			  if (   (!cmh  || cmh->valueAtOffset(offset) < SORT_ZERO)
				  && (!fmh  || fmh->valueAtOffset(offset) < SORT_ZERO)
				  && (cmh && fmh || !fmh2 || fmh2->valueAtOffset(offset) < SORT_ZERO))
				  continue;

			  if (pos.ti()->maxWinstTabel.get(bewogenStuk, zet) < maak_waarde(-55) - maak_waarde(15) * int((unsigned int)diepte / PLY))
				  continue;
		  }

		  voorspeldeDiepte = std::max(nieuweDiepte - lmr_reductie<PvNode>(vooruitgang, diepte, zetNummer), DIEPTE_0);
		  // gemiddelde waarden: <depth>=37  <nieuweDiepte>=29  <voorspeldeDiepte>=20

		  // Futility pruning: parent node
		  if (voorspeldeDiepte < 7 * PLY
			  // volgende test is nooit waar bij staatSchaak
			  && st->stellingWaarde + futility_marge2(voorspeldeDiepte) <= alfa)
			  continue;

          // Prune moves with negative SEE at low depths
		  if (voorspeldeDiepte < 4 * PLY && !pos.see_test(zet, SEE_0))
              continue;
      }
	  else if (!rootKnoop
		  && diepte < 3 * PLY
		  && besteWaarde > -LANGSTE_MAT_WAARDE)
	  {
		  if (st->mp_etappe != GOEDE_SLAGEN && !pos.see_test(zet, SEE_PAARD - SEE_LOPER))
			  continue;
	  }

      // Speculative prefetch as early as possible
	  TT.prefetch_tuple(pos.sleutel_na_zet(zet));

      // Check for legality just before making the move
      if (!rootKnoop && !pos.legale_zet(zet))
      {
          --zetNummer;
          continue;
      }

      // Step 14. Make the move
      pos.speel_zet(zet, geeftSchaak);

	  // Step 14.5. Tactische modus
	  //*
	  if (rootKnoop && zetNummer > 1 && Threads.tactischeModus && diepte >= 12 * PLY && abs(alfa) < WINST_WAARDE
		  && diepte > mijnThread->vorigeTactischeDiepte)
	  {
		  mijnThread->tactischeModusGebruikt = true;

		  bool tactische_zet = slagOfPromotie || geeftSchaak;
		  if (!tactische_zet)
		  {
			  Waarde alfa_dreiging = alfa + 5 * WAARDE_PION / 4;

			  st++;
			  st->mp_eindeLijst = (st - 1)->mp_eindeLijst;
			  pos.speel_null_zet();
			  (st + 1)->doeGeenVroegePruning = true;
			  waarde = search<NonPV>(pos, alfa_dreiging, alfa_dreiging + WAARDE_1, 6 * PLY, false);
			  (st + 1)->doeGeenVroegePruning = false;
			  pos.neem_null_terug();
			  st--;

			  tactische_zet = (waarde > alfa_dreiging);
		  }

		  if (tactische_zet)
		  {
			  Diepte diepte_red = diepte - diepte / 8;
			  waarde = mijnThread->rootZetten[mijnThread->rootZetten.find(zet)].startWaarde;

			  Waarde score_offset = 8 * (alfa - waarde) / (diepte / PLY + 16);
			  do
			  {
				  if (Signals.stop.load(std::memory_order_relaxed))
					  return WAARDE_0;

				  Waarde alfa_red = waarde + score_offset;
				  if (alfa_red >= alfa && diepte_red <= diepte)
					  break;
				  if (alfa_red >= alfa || diepte_red >= diepte + diepte / 8)
					  alfa_red = alfa;

				  (st + 1)->doeGeenVroegePruning = true;
				  waarde = -search<NonPV>(pos, -alfa_red - WAARDE_1, -alfa_red, diepte_red, true);
				  (st + 1)->doeGeenVroegePruning = false;

				  if (waarde <= alfa_red)
					  break;
				  if (waarde > alfa && diepte_red >= diepte)
				  {
					  (st + 1)->pv = pv;
					  (st + 1)->pv[0] = GEEN_ZET;
					  waarde = -search<PV>(pos, -beta, -alfa, diepte_red, false);
					  goto einde_zet;
				  }
				  diepte_red += PLY;
			  } while (true);
		  }
	  }
	  //*/

	  // Step 15. Reduced depth search (LMR). If the move fails high it will be
      // re-searched at full depth.
      if (    diepte >= 3 * PLY
          &&  zetNummer > 1
          && !slagOfPromotie
		  && (!Threads.tactischeModus || diepte < 12 * PLY || st->ply > 3))
      {
          Diepte r = lmr_reductie<PvNode>(vooruitgang, diepte, zetNummer);

          // Increase reduction for cut nodes
		  // hier: 30% cutNode bij !PvNode;
          if (!PvNode && cutNode)
              r += 2 * PLY;

          // Decrease reduction for moves that escape a capture.
		  // hier: 6% ontwijkzetten
		  if (stuk_type(bewogenStuk) >= PAARD
			  && !pos.see_test(maak_zet(naar_veld(zet), van_veld(zet)), SEE_0))
			  r -= 2 * PLY;

		  // Decrease/increase reduction for moves with a good/bad history
		  int offset = ZetWaardeStatistiek::bereken_offset(bewogenStuk, naar_veld(zet));
		  SorteerWaarde val = staatSchaak ? pos.ti()->evasionHistory.valueAtOffset(offset) : pos.ti()->history.valueAtOffset(offset);
		  val +=   (cmh ? cmh->valueAtOffset(offset) : SORT_ZERO)
			     + (fmh ? fmh->valueAtOffset(offset) : SORT_ZERO)
			     + (fmh2 ? fmh2->valueAtOffset(offset) : SORT_ZERO);
		  //r -= (int)val / 2560 * (PLY / 8);
		  //r -= (int)val / 2100 * (PLY / 8);
		  r -= (int)val / 2048 * (PLY / 8);

		  r = std::max(r, DIEPTE_0);
          Diepte d = std::max(nieuweDiepte - r, PLY);
		  st->lmr_reductie = nieuweDiepte - d;

          waarde = -search<NonPV>(pos, -(alfa + WAARDE_1), -alfa, d, true);

		  volledigZoekenNodig = (waarde > alfa && st->lmr_reductie != 0);
		  st->lmr_reductie = 0;
		  //(st + 1)->doeGeenVroegePruning = true;
	  }
      else
          volledigZoekenNodig = !PvNode || zetNummer > 1;

      // Step 16. Full depth search when LMR is skipped or fails high
      if (volledigZoekenNodig)
          waarde = nieuweDiepte <   PLY ?
                            geeftSchaak ? -qsearch<NonPV,  true>(pos, -(alfa + WAARDE_1), -alfa, DIEPTE_0)
                                       : -qsearch<NonPV, false>(pos, -(alfa + WAARDE_1), -alfa, DIEPTE_0)
                                       : - search<NonPV>(pos, -(alfa + WAARDE_1), -alfa, nieuweDiepte, PvNode || !cutNode);

	  (st + 1)->doeGeenVroegePruning = false;

      // For PV nodes only, do a full PV search on the first move or after a fail
      // high (in the latter case search only if value < beta), otherwise let the
      // parent node fail low with value <= alpha and try another move.
      if (PvNode && (zetNummer == 1 || (waarde > alfa && (rootKnoop || waarde < beta))))
      {
          (st + 1)->pv = pv;
          (st + 1)->pv[0] = GEEN_ZET;

          waarde = nieuweDiepte <   PLY ?
                            geeftSchaak ? -qsearch<PV,  true>(pos, -beta, -alfa, DIEPTE_0)
                                       : -qsearch<PV, false>(pos, -beta, -alfa, DIEPTE_0)
                                       : - search<PV>(pos, -beta, -alfa, nieuweDiepte, false);
      }

  einde_zet:

      // Step 17. Undo move
      pos.neem_zet_terug(zet);

      assert(waarde > -HOOGSTE_WAARDE && waarde < HOOGSTE_WAARDE);

      // Step 18. Check for a new best move
      // Finished searching the move. If a stop occurred, the return value of
      // the search cannot be trusted, and we return immediately without
      // updating best move, PV and TT.
      if (Signals.stop.load(std::memory_order_relaxed))
          return WAARDE_0;

      if (rootKnoop)
      {
		  RootZet& rootZet = mijnThread->rootZetten.zetten[mijnThread->rootZetten.find(zet)];

          // PV move or new best move ?
          if (zetNummer == 1 || waarde > alfa)
          {
              rootZet.score = waarde;
			  rootZet.pv.resize(1);
			  rootZet.diepte = diepte;

              assert((st + 1)->pv);

              for (Zet* zet = (st + 1)->pv; *zet != GEEN_ZET; ++zet)
                  rootZet.pv.add(*zet);

              // We record how often the best move has been changed in each
              // iteration. This information is used for time management: When
              // the best move changes frequently, we allocate some more time.
              if (zetNummer > 1 && mijnThread == Threads.main())
                  static_cast<MainThread*>(mijnThread)->besteZetVerandert += 1024;

			  if (Tijdscontrole.verstreken() > 4000 && mijnThread == Threads.main())
				  sync_cout << UCI::pv(pos, alfa, beta) << sync_endl;
		  }
          else
              // All other moves but the PV are set to the lowest value: this is
              // not a problem when sorting because the sort is stable and the
              // move position in the list is preserved - just the PV is pushed up.
              rootZet.score = -HOOGSTE_WAARDE;
      }

	  //dbg_hit_on(mp.get_stage(), value > alpha);
      if (waarde > besteWaarde)
      {
          besteWaarde = waarde;

          if (waarde > alfa)
          {
			  // If there is an easy move for this position, clear it if unstable
              if (    PvNode
                  &&  mijnThread == Threads.main()
                  &&  EasyMove.get(pos.sleutel())
                  && (zet != EasyMove.get(pos.sleutel()) || zetNummer > 1))
                  EasyMove.clear();

              besteZet = zet;

              if (PvNode && !rootKnoop) // Update pv even in fail-high case
                  update_pv(st->pv, zet, (st + 1)->pv);

              if (PvNode && waarde < beta) // Update alpha! Always alpha < beta
                  alfa = waarde;
              else
              {
                  assert(waarde >= beta); // Fail high
                  break;
              }
		  }
      }

      if (!slagOfPromotie && zet != besteZet && rustigeZetAantal < 64)
          rustigeZetten[rustigeZetAantal++] = zet;
    }

    // The following condition would detect a stop only after move loop has been
    // completed. But in this case besteWaarde is valid because we have fully
    // searched our subtree, and we can anyhow save the result in TT.
    /*
       if (Signals.stop)
        return REMISE_WAARDE;
    */

    // Step 20. Check for mate and stalemate
    // All legal moves have been searched and if there are no legal moves, it
    // must be a mate or a stalemate. If we are in a singular extension search then
    // return a fail low score.
    //if (!zetNummer)
	if (besteWaarde == -HOOGSTE_WAARDE)
        besteWaarde = st->uitgeslotenZet ? alfa
                   : staatSchaak ? staat_mat(st->ply) : RemiseWaarde[pos.aan_zet()];

    // Quiet best move: update killers, history and countermoves
    else if (besteZet)
        update_stats(pos, staatSchaak, besteZet, diepte, rustigeZetten, rustigeZetAantal);

    // Bonus for prior countermove that caused the fail low
    else if (    diepte >= 3 * PLY
             && !staatSchaak
             && !pos.geslagen_stuk()
             && is_ok(st->vorigeZet))
    {
		Veld prevSq = naar_veld(st->vorigeZet);

		if (diepte < 18 * PLY)
		{
			SorteerWaarde bonus = counter_zet_bonus(diepte);
			int offset = CounterZetWaarden::bereken_offset(pos.stuk_op_veld(prevSq), prevSq);

			if ((st - 1)->counterZetWaarden)
				(st - 1)->counterZetWaarden->updatePlus(offset, bonus);

			if ((st - 2)->counterZetWaarden)
				(st - 2)->counterZetWaarden->updatePlus(offset, bonus);

			if ((st - 4)->counterZetWaarden)
				(st - 4)->counterZetWaarden->updatePlus(offset, bonus);
		}
    }

	ttt = TT.zoek(sleutel64, ttGevonden);
    ttt->bewaar(sleutel64, waarde_voor_tt(besteWaarde, st->ply),
              besteWaarde >= beta ? ONDERGRENS :
			  PvNode && besteZet ? EXACTE_WAARDE : BOVENGRENS,
              diepte, besteZet, st->stellingWaarde, TT.generatie());

    assert(besteWaarde > -HOOGSTE_WAARDE && besteWaarde < HOOGSTE_WAARDE);

    return besteWaarde;
  }


  // qsearch() is the quiescence search function, which is called by the main
  // search function when the remaining depth is zero (or, to be more precise,
  // less than PLY).

  template <NodeType NT, bool staatSchaak>
  Waarde qsearch(Stelling& pos, Waarde alfa, Waarde beta, Diepte diepte) {

    const bool PvNode = NT == PV;

    assert(staatSchaak == !!pos.schaak_gevers());
    assert(alfa >= -HOOGSTE_WAARDE && alfa < beta && beta <= HOOGSTE_WAARDE);
    assert(PvNode || (alfa == beta - 1));
    assert(diepte < PLY);

    Zet pv[MAX_PLY+1];
    TTTuple* ttt;
    Sleutel64 sleutel64;
    Zet ttZet, zet, besteZet;
    Waarde besteWaarde, waarde, ttWaarde, futilityWaarde, futilityBasis, origAlfa;
    bool ttGevonden, geeftSchaak, evasionPrunable;
    Diepte ttDiepte;

#ifdef TRACE_LOG
	trace_msg("QSearch", diepte, alfa, beta);
#endif
	StellingInfo* st = pos.info();

	if (PvNode)
    {
        origAlfa = alfa; // To flag EXACTE_WAARDE when eval above alpha and no available moves
        (st + 1)->pv = pv;
        st->pv[0] = GEEN_ZET;
    }

    besteZet = GEEN_ZET;

    // Check for an instant draw or if the maximum ply has been reached
    if (st->zetHerhaling || st->ply >= MAX_PLY)
        return st->ply >= MAX_PLY && !staatSchaak ? evaluate(pos, GEEN_WAARDE, GEEN_WAARDE)
                                              : RemiseWaarde[pos.aan_zet()];

    assert(0 <= st->ply && st->ply < MAX_PLY);

    // Decide whether or not to include checks: this fixes also the type of
    // TT entry depth that we are going to use. Note that in qsearch we use
    // only two types of depth in TT: QS_SCHAAK_DIEPTE or QS_ZONDER_SCHAAK_DIEPTE.
    ttDiepte = (staatSchaak || diepte >= QS_SCHAAK_DIEPTE) ? QS_SCHAAK_DIEPTE
                                                  : QS_ZONDER_SCHAAK_DIEPTE;

    // Transposition table lookup
    sleutel64 = pos.sleutel();
	sleutel64 ^= pos.remise50_sleutel();
	ttt = TT.zoek(sleutel64, ttGevonden);
    ttZet = ttGevonden ? ttt->zet() : GEEN_ZET;
    ttWaarde = ttGevonden ? waarde_uit_tt(ttt->waarde(), st->ply) : GEEN_WAARDE;

    if (  !PvNode
        && ttGevonden
        && ttt->diepte() >= ttDiepte
        && ttWaarde != GEEN_WAARDE // Only in case of TT access race
        && (ttWaarde >= beta ? (ttt->limiet() &  ONDERGRENS)
                             : (ttt->limiet() &  BOVENGRENS)))
    {
        return ttWaarde;
    }

    // Evaluate the position statically
    if (staatSchaak)
    {
		st->stellingWaarde = GEEN_WAARDE;
        besteWaarde = futilityBasis = -HOOGSTE_WAARDE;
    }
    else
    {
		if (ttGevonden)
        {
            // Never assume anything on values stored in TT
            if ((st->stellingWaarde = besteWaarde = ttt->eval()) == GEEN_WAARDE)
				st->stellingWaarde = besteWaarde = evaluate(pos, PvNode ? GEEN_WAARDE : alfa - LazyMarginQSearchLow,
					PvNode ? GEEN_WAARDE : beta + LazyMarginQSearchHigh);

            // Can ttWaarde be used as a better position evaluation?
            if (ttWaarde != GEEN_WAARDE)
                if (ttt->limiet() & (ttWaarde > besteWaarde ? ONDERGRENS : BOVENGRENS))
                    besteWaarde = ttWaarde;
        }
        else
			st->stellingWaarde = besteWaarde =
			st->vorigeZet != NULL_ZET ? evaluate(pos, PvNode ? GEEN_WAARDE : alfa - LazyMarginQSearchLow,
				PvNode ? GEEN_WAARDE : beta + LazyMarginQSearchHigh)
                                             : -(st - 1)->stellingWaarde + 2 * WAARDE_TEMPO;
#ifdef TRACE_LOG
		trace_eval(st->stellingWaarde);
#endif

        // Stand pat. Return immediately if static value is at least beta
        if (besteWaarde >= beta)
        {
            if (!ttGevonden)
                ttt->bewaar(pos.sleutel(), waarde_voor_tt(besteWaarde, st->ply), ONDERGRENS,
					GEEN_DIEPTE, GEEN_ZET, st->stellingWaarde, TT.generatie());

            return besteWaarde;
        }

        if (PvNode && besteWaarde > alfa)
            alfa = besteWaarde;

		futilityBasis = besteWaarde;
    }

    // Initialize a PikZet object for the current position, and prepare
    // to search the moves. Because the depth is <= 0 here, only captures,
    // queen promotions and checks (only if depth >= QS_SCHAAK_DIEPTE) will
    // be generated.
	PikZet::init_qsearch(pos, ttZet, diepte, naar_veld(st->vorigeZet));

    // Loop through the moves until no moves remain or a beta cutoff occurs
    while ((zet = PikZet::geef_zet(pos)) != GEEN_ZET)
    {
      assert(is_ok(zet));

      geeftSchaak = zet_type(zet) == NORMAAL && !pos.aftrek_schaak_mogelijk()
                  ? st->schaakVelden[stuk_type(pos.stuk_op_veld(van_veld(zet)))] & naar_veld(zet)
                  : pos.geeft_schaak(zet);

      // Futility pruning
      if (   !staatSchaak
          && !geeftSchaak
          &&  futilityBasis > -WINST_WAARDE
          && !pos.vooruitgeschoven_pion(zet))
      {
          assert(zet_type(zet) != ENPASSANT); // Due to !pos.vooruitgeschoven_pion

		  futilityWaarde = futilityBasis + maak_waarde(128) + QSearchFutilityValue[pos.stuk_op_veld(naar_veld(zet))];

		  //StukType capture = type_of(pos.piece_on(naar_veld(move)));
		  //futilityValue = futilityBase + futilityDeltas[capture] + k_PieceValueFase[capture][pos.partij_fase()];
		  //if (capture == PION)
			 // futilityValue += futilityPawnRank[relatieve_rij(pos.aan_zet(), naar_veld(move))] + futilityPawnCount[pos.count<PION>(~pos.aan_zet())];

          if (futilityWaarde <= alfa)
          {
              besteWaarde = std::max(besteWaarde, futilityWaarde);
              continue;
          }

		  if (futilityBasis + maak_waarde(128) <= alfa && !pos.see_test(zet, SeeWaarde(1)))
          {
              besteWaarde = std::max(besteWaarde, futilityBasis + maak_waarde(128));
              continue;
          }
      }

      // Detect non-capture evasions that are candidates to be pruned
      evasionPrunable =    staatSchaak
                       &&  besteWaarde > -LANGSTE_MAT_WAARDE
                       && !pos.is_slagzet(zet);

	  // Don't search moves with negative SEE values
	  if (  (!staatSchaak || evasionPrunable)
          && zet_type(zet) != PROMOTIE
		  &&  !pos.see_test(zet, SEE_0))
          continue;

      // Speculative prefetch as early as possible
	  TT.prefetch_tuple(pos.sleutel_na_zet(zet));

      // Check for legality just before making the move
      if (!pos.legale_zet(zet))
          continue;

      // Make and search the move
      pos.speel_zet(zet, geeftSchaak);
      waarde = geeftSchaak ? -qsearch<NT,  true>(pos, -beta, -alfa, diepte - PLY)
                           : -qsearch<NT, false>(pos, -beta, -alfa, diepte - PLY);
      pos.neem_zet_terug(zet);

      assert(waarde > -HOOGSTE_WAARDE && waarde < HOOGSTE_WAARDE);

      // Check for a new best move
      if (waarde > besteWaarde)
      {
          besteWaarde = waarde;

          if (waarde > alfa)
          {
              if (PvNode) // Update pv even in fail-high case
                  update_pv(st->pv, zet, (st + 1)->pv);

              if (PvNode && waarde < beta) // Update alpha here!
              {
                  alfa = waarde;
                  besteZet = zet;
              }
              else // Fail high
              {
				  ttt = TT.zoek(sleutel64, ttGevonden);
                  ttt->bewaar(sleutel64, waarde_voor_tt(waarde, st->ply), ONDERGRENS,
                            ttDiepte, zet, st->stellingWaarde, TT.generatie());

                  return waarde;
              }
          }
       }
    }

    // All legal moves have been searched. A special case: If we're in check
    // and no legal moves were found, it is checkmate.
    if (staatSchaak && besteWaarde == -HOOGSTE_WAARDE)
        return staat_mat(st->ply); // Plies to mate from the root

	ttt = TT.zoek(sleutel64, ttGevonden);
	ttt->bewaar(sleutel64, waarde_voor_tt(besteWaarde, st->ply),
              PvNode && besteWaarde > origAlfa ? EXACTE_WAARDE : BOVENGRENS,
              ttDiepte, besteZet, st->stellingWaarde, TT.generatie());

    assert(besteWaarde > -HOOGSTE_WAARDE && besteWaarde < HOOGSTE_WAARDE);

    return besteWaarde;
  }


  // waarde_voor_tt() adjusts a mate score from "plies to mate from the root" to
  // "plies to mate from the current position". Non-mate scores are unchanged.
  // The function is called before storing a value in the transposition table.

  Waarde waarde_voor_tt(Waarde v, int ply) {

    assert(v != GEEN_WAARDE);

    return  v >= LANGSTE_MAT_WAARDE  ? Waarde(v + ply)
          : v <= -LANGSTE_MAT_WAARDE ? Waarde(v - ply) : v;
  }


  // waarde_uit_tt() is the inverse of waarde_voor_tt(): It adjusts a mate score
  // from the transposition table (which refers to the plies to mate/be mated
  // from current position) to "plies to mate/be mated from the root".

  Waarde waarde_uit_tt(Waarde v, int ply) {

    return  v == GEEN_WAARDE         ? GEEN_WAARDE
          : v >= LANGSTE_MAT_WAARDE  ? Waarde(v - ply)
          : v <= -LANGSTE_MAT_WAARDE ? Waarde(v + ply) : v;
  }


  // update_pv() adds current move and appends child pv[]

  void update_pv(Zet* pv, Zet zet, Zet* childPv) {

	  *pv++ = zet;
	  if (childPv)
		  while (*childPv != GEEN_ZET)
			  *pv++ = *childPv++;
	  *pv = GEEN_ZET;
  }


  // update_stats() updates killers, history, countermove and countermove plus
  // follow-up move history when a new quiet best move is found.

  void update_stats(const Stelling& pos, bool staatSchaak, Zet zet,
                    Diepte diepte, Zet* rustigeZetten, int rustigAantal) {

	StellingInfo* st = pos.info();

#if 0
	uint32_t hash_two_ply = (uint32_t)pos.two_ply_key();
	uint32_t counter_value = (hash_two_ply & 0xffff0000) | (zet & 0xffff);
	uint32_t *counter_lijst = pos.mijn_thread()->counter_tabel[hash_two_ply & counter_tabel_mask];
	if (counter_lijst[0] != counter_value)
	{
		if (counter_lijst[1] != counter_value)
		{
			if (counter_lijst[2] != counter_value)
				counter_lijst[3] = counter_lijst[2];
			counter_lijst[2] = counter_lijst[1];
		}
		counter_lijst[1] = counter_lijst[0];
		counter_lijst[0] = counter_value;
	}
#endif

	Veld prevSq = naar_veld(st->vorigeZet);
	CounterZetWaarden* cmh  = st->counterZetWaarden;
	CounterZetWaarden* fmh  = (st - 1)->counterZetWaarden;
	CounterZetWaarden* fmh2 = (st - 3)->counterZetWaarden;
    ThreadInfo* ti = pos.ti();
	ZetWaardeStatistiek& history = staatSchaak ? ti->evasionHistory : ti->history;
	
	if (!pos.slag_of_promotie(zet))
	{
		if (st->killers[0] != zet)
		{
			st->killers[1] = st->killers[0];
			st->killers[0] = zet;
		}

		if (cmh)
			ti->counterZetten.update(pos.stuk_op_veld(prevSq), prevSq, zet);

		if (diepte < 18 * PLY)
		{
			SorteerWaarde bonus = counter_zet_bonus(diepte);
			int offset = ZetWaardeStatistiek::bereken_offset(pos.bewogen_stuk(zet), naar_veld(zet));
			history.updatePlus(offset, bonus);
			if (cmh)
				cmh->updatePlus(offset, bonus);

			if (fmh)
				fmh->updatePlus(offset, bonus);

			if (fmh2)
				fmh2->updatePlus(offset, bonus);

			// Decrease all the other played quiet moves
			for (int i = 0; i < rustigAantal; ++i)
			{
				int offset = ZetWaardeStatistiek::bereken_offset(pos.bewogen_stuk(rustigeZetten[i]), naar_veld(rustigeZetten[i]));
				history.updateMinus(offset, bonus);
				if (cmh)
					cmh->updateMinus(offset, bonus);

				if (fmh)
					fmh->updateMinus(offset, bonus);

				if (fmh2)
					fmh2->updateMinus(offset, bonus);
			}
		}
	}

	// Extra penalty for a quiet TT move in previous ply when it gets refuted
	if ((st - 1)->zetNummer == 1 && !pos.geslagen_stuk())
	{
		if (diepte < 18 * PLY)
		{
			SorteerWaarde bonus = counter_zet_bonus(diepte + PLY);
			int offset = ZetWaardeStatistiek::bereken_offset(pos.stuk_op_veld(prevSq), prevSq);

			if ((st - 1)->counterZetWaarden)
				(st - 1)->counterZetWaarden->updateMinus(offset, bonus);

			if ((st - 2)->counterZetWaarden)
				(st - 2)->counterZetWaarden->updateMinus(offset, bonus);

			if ((st - 4)->counterZetWaarden)
				(st - 4)->counterZetWaarden->updateMinus(offset, bonus);
		}
	}
  }


  // check_time() is used to print debug info and, more importantly, to detect
  // when we are out of available time and thus stop the search.

  void tijdscontrole() {

    int64_t verstreken = Tijdscontrole.verstreken();

    if (verstreken - vorigeInfoTijd >= 1000)
    {
        vorigeInfoTijd = (verstreken + 100) / 1000 * 1000;
        //dbg_print();
		uint64_t nodes = Threads.bezochte_knopen();
		uint64_t nps = verstreken ? nodes / verstreken * 1000 : 0;
		uint64_t tbHits = Threads.tb_hits();
		sync_cout << "info time " << verstreken << " nodes " << nodes << " nps " << nps
			<< " tbhits " << tbHits << " hashfull " << TT.bereken_hashfull() << sync_endl;
    }

    // An engine may not stop pondering until told so by the GUI
    if (Limits.ponder)
        return;

    if (   (Limits.gebruikt_tijd_berekening() && verstreken > Tijdscontrole.maximum() - 10)
        || (Limits.movetime && verstreken >= Limits.movetime)
        || (Limits.nodes && Threads.bezochte_knopen() >= Limits.nodes))
            Signals.stop = true;
  }

} // namespace


/// UCI::pv() formats PV information according to the UCI protocol. UCI requires
/// that all (if any) unsearched PV lines are sent using a previous search score.

std::string UCI::pv(const Stelling& pos, Waarde alpha, Waarde beta) {

  std::stringstream ss;
  int elapsed = (int)Tijdscontrole.verstreken() + 1;
  const RootZetten& rootZetten = pos.mijn_thread()->rootZetten;
  int PVIdx = pos.mijn_thread()->actievePV;
  int multiPV = std::min(Threads.multiPV, rootZetten.zetAantal);
  uint64_t bezochte_knopen = Threads.bezochte_knopen();
  uint64_t tbHits = Threads.tb_hits();
  int hashFull = elapsed > 1000 ? TT.bereken_hashfull() : 0;

  for (int i = 0; i < multiPV; ++i)
  {
	  int iter = rootZetten[i].diepte / MAIN_THREAD_INC;
	  if (iter < 1)
          continue;
	  iter = displayIteration(iter);

      Waarde v = i <= PVIdx ? rootZetten[i].score : rootZetten[i].vorigeRootScore;

	  //if (Threads.stukContempt && abs(v) < WINST_WAARDE)
		 // v -= 5 * pos.contempt_aantal(Threads.rootKleur) / 4;

      bool tb = TB_RootInTB && abs(v) < MAT_WAARDE - MAX_PLY;
      v = tb ? TB_Score : v;

      if (ss.rdbuf()->in_avail()) // Not at first line
          ss << "\n";

	  // set st->pionSleutel variables to 0 so that the maximum ply depth can be examined later for the "seldepth" UCI output
	  int selDepth = 0;
	  if (mainSearchStack)
		  for (selDepth = 0; selDepth < MAX_PLY; selDepth++)
			  if ((mainSearchStack + selDepth)->pionSleutel == 0)
				  break;

      ss << "info multipv " << i + 1
		 << " depth "    << iter
         << " seldepth " << selDepth
         << " score "    << UCI::waarde(v);

      if (!tb && i == PVIdx)
          ss << (v >= beta ? " lowerbound" : v <= alpha ? " upperbound" : "");

      ss << " time "     << elapsed
         << " nodes "    << bezochte_knopen
         << " nps "      << bezochte_knopen / elapsed * 1000
	     << " tbhits "   << tbHits;

      if (hashFull)
          ss << " hashfull " << hashFull;

      ss << " pv";

	  // limit the length of the pv (the final moves usually are poor anyways)
	  int pvLength = rootZetten[i].pv.size();
	  if (pvLength > iter)
		  pvLength = std::max(iter, pvLength - 4);
	  for (int n = 0; n < pvLength; n++)
		  ss << " " << UCI::zet(rootZetten[i].pv[n], pos);
  }

  return ss.str();
}


/// RootMove::extract_ponder_from_tt() is called in case we have no ponder move
/// before exiting the search, for instance, in case we stop the search during a
/// fail high at root. We try hard to have a ponder move to return to the GUI,
/// otherwise in case of 'ponder on' we have nothing to think on.

bool RootZet::vind_ponder_zet_in_tt(Stelling& pos)
{
    bool ttGevonden;

    assert(pv.size() == 1);
	if (!pv[0])
		return false;

    pos.speel_zet(pv[0]);
    TTTuple* tte = TT.zoek(pos.sleutel() ^ pos.remise50_sleutel(), ttGevonden);

    if (ttGevonden)
    {
        Zet zet = tte->zet(); // Local copy to be SMP safe
		if (ZettenLijst<GEN_LEGALE_ZETTEN>(pos).contains(zet))
			pv.add(zet);
    }
	pos.neem_zet_terug(pv[0]);

    return pv.size() > 1;
}

void RootZet::vervolledig_pv_met_tt(Stelling& pos)
{
	Sleutel64 keys[MAX_PLY], key;
	int aantal = 0;
	bool ttGevonden;

	assert(pv.size() == 1);
	Zet zet = pv[0];
	keys[aantal++] = pos.sleutel();

	while (true)
	{
		pos.speel_zet(zet);
		key = pos.sleutel();

		bool repeated = false;
		for (int i = aantal - 2; i >= 0; i -= 2)
			if (keys[i] == key)
			{
				repeated = true;
				break;
			}
		if (repeated)
			break;

		keys[aantal++] = key;

		TTTuple* tte = TT.zoek(pos.sleutel() ^ pos.remise50_sleutel(), ttGevonden);
		if (!ttGevonden)
			break;
		zet = tte->zet();
		if (!zet || !ZettenLijst<GEN_LEGALE_ZETTEN>(pos).contains(zet))
			break;
		pv.add(zet);
	}
	for (int i = pv.size(); i > 0; )
		pos.neem_zet_terug(pv[--i]);
}


namespace {

Diepte BerekenEgtbNut(const Stelling& pos)
{
	Diepte result = egtb_niet_nuttig;

	int TotAantal = pos.alle_stukken_aantal();
	const int WD = pos.aantal<DAME>(WIT);
	const int ZD = pos.aantal<DAME>(ZWART);
	const int WT = pos.aantal<TOREN>(WIT);
	const int ZT = pos.aantal<TOREN>(ZWART);
	const int WL = pos.aantal<LOPER>(WIT);
	const int ZL = pos.aantal<LOPER>(ZWART);
	const int WP = pos.aantal<PAARD>(WIT);
	const int ZP = pos.aantal<PAARD>(ZWART);
	const int WPion = pos.aantal<PION>(WIT);
	const int ZPion = pos.aantal<PION>(ZWART);
	if (TotAantal == 5)
	{
		if (
			 WD == 1 && ZD == 1 && WPion + ZPion == 1  // KQPKQ
			|| WT == 1 && ZT == 1 && WPion + ZPion == 1  // KRPKR
			|| WP == 2 && ZPion == 1 // KNNKP wit
			|| ZP == 2 && WPion == 1 // KNNKP zwart
			)
			result = egtb_zeer_nuttig;
		else if (
			WD == 1 && ZT == 1 && ZPion == 1  // KQKRP wit
			|| ZD == 1 && WT == 1 && WPion == 1  // KQKRP zwart
			|| WL + WP == 1 && ZL + ZP == 1 && WPion + ZPion == 1  // KBPKB, KNPKN, KBPKN, KNPKB
			|| (WPion == 2 && ZPion == 1 || WPion == 1 && ZPion == 2)  // KPPKP
			)
			result = egtb_nuttig;
	}
	else if (TotAantal == 6)
	{
		if (
			WD == 1 && ZD == 1 && (WPion == 2 || ZPion == 2)  // KQPPKQ
			|| WT == 1 && ZT == 1 && (WPion == 2 || ZPion == 2)  // KRPPKR
			)
			result = egtb_zeer_nuttig;
		else if (
			WD == 1 && ZD == 1 && WPion + ZPion == 2  // KQPKQP
			|| WT == 1 && ZT == 1 && WPion + ZPion == 2  // KRPKRP
			|| WL + WP == 1 && ZL + ZP == 1 && WPion + ZPion == 2  // KBPPKB, KBPKBP, KNPPKN, KNPKNP, KBPPKN, KBPKNP, KNPPKB
			|| (WT == 1 && ZL == 1 || WL == 1 && ZT == 1) && WPion == 1 && ZPion == 1  // KRPKBP
			|| (WT == 1 && ZP == 1 || WP == 1 && ZT == 1) && WPion == 1 && ZPion == 1  // KRPKNP
			|| WD == 1 && ZT == 1 && ZPion == 2  // KQKRPP wit
			|| ZD == 1 && WT == 1 && WPion == 2  // KQKRPP zwart
			|| WD == 1 && ZL == 1 && ZP == 1 && ZPion == 1  // KQKBNP wit
			|| ZD == 1 && WL == 1 && WP == 1 && WPion == 1  // KQKBNP wit
			)
			result = egtb_nuttig;
	}
	return result;
}

void FilterRootMoves(Stelling& pos, RootZetten& rootZetten)
{
	int aantal_stukken = pos.alle_stukken_aantal();

	if (aantal_stukken <= Tablebases::MaxPieces_dtm)
		TB_Score = Tablebases::EGTB_probe_dtm(pos, -MAT_WAARDE, MAT_WAARDE);
	else if (aantal_stukken <= Tablebases::MaxPieces_dtz)
		TB_Score = Tablebases::EGTB_probe_dtz(pos, -MAT_WAARDE, MAT_WAARDE);
	TB_RootInTB = TB_Score != GEEN_WAARDE;
	if (!TB_RootInTB)
		return;
	if (TB_Score < -LANGSTE_MAT_WAARDE)
	{
		// verloren stelling, doe een normale Search
		TB_Cardinality = 0;
	}
	else if (TB_Score > LANGSTE_MAT_WAARDE)
	{
		// winnende stelling -> kies de zet met de laagste DTM of DTZ
		Waarde beste_waarde = -HOOGSTE_WAARDE;
		int beste_index = -1;

		for (int i = 0; i < rootZetten.zetAantal; i++)
		{
			Zet zet = rootZetten[i].pv[0];
			pos.speel_zet(zet);
			if (! pos.info()->zetHerhaling)
			{
				Waarde EGTB_score;
				if (aantal_stukken <= Tablebases::MaxPieces_dtm)
					EGTB_score = Tablebases::EGTB_probe_dtm(pos, -MAT_WAARDE, MAT_WAARDE);
				else if (pos.vijftig_zetten_teller() == 0)
				{
					// pionzet of slagzet die wint => DTZ = 0 !
					EGTB_score = Tablebases::EGTB_probe_wdl(pos, -MAT_WAARDE, MAT_WAARDE);
					if (EGTB_score != GEEN_WAARDE && -EGTB_score > LANGSTE_MAT_WAARDE)
						EGTB_score = -Waarde(LANGSTE_MAT_WAARDE + 500);
					// eigenlijk kan hier de lus gestopt worden, dit is de beste zet
				}
				else
				{
					EGTB_score = Tablebases::EGTB_probe_dtz(pos, -MAT_WAARDE, MAT_WAARDE);
					// een winnende zet kan een drievoudige zetherhaling zijn, test dit hier...
					//if (EGTB_score != GEEN_WAARDE && -EGTB_score > LANGSTE_MAT_WAARDE
					//	&& Heeft_ZetHerhaling<$$THREAD_ID$$ JIJ>($$C_STELLING_C$$))
					//	EGTB_score = REMISE_WAARDE;
				}
				if (EGTB_score != GEEN_WAARDE && -EGTB_score > beste_waarde)
				{
					beste_waarde = -EGTB_score;
					beste_index = i;
				}
			}
			pos.neem_zet_terug(zet);
		}

		if (beste_waarde != GEEN_WAARDE)
		{
			RootZet rootZet = rootZetten[beste_index];
			rootZet.score = beste_waarde;
			rootZetten.zetAantal = 0;
			rootZetten.add(rootZet);
			TB_Cardinality = 0;
		}
	}
	else
	{
		// behoud enkel zetten die dezelfde uitkomst behouden als in de root
		int newSize = 0;
		for (int i = 0; i < rootZetten.zetAantal; i++)
		{
			Zet zet = rootZetten[i].pv[0];
			pos.speel_zet(zet);

			Waarde EGTB_score;
			if (pos.info()->zetHerhaling)
				EGTB_score = REMISE_WAARDE;
			else if (aantal_stukken <= Tablebases::MaxPieces_dtm)
				EGTB_score = Tablebases::EGTB_probe_dtm(pos, -MAT_WAARDE, MAT_WAARDE);
			else
				EGTB_score = Tablebases::EGTB_probe_dtz(pos, -MAT_WAARDE, MAT_WAARDE);

			pos.neem_zet_terug(zet);
			
			if (EGTB_score != GEEN_WAARDE &&
				(TB_Score > LANGSTE_MAT_WAARDE && -EGTB_score < LANGSTE_MAT_WAARDE
					|| -EGTB_score < -LANGSTE_MAT_WAARDE))
			{
				// verwijder deze zet
			}
			else
				rootZetten.zetten[newSize++] = rootZetten.zetten[i];
		}
		if (newSize > 0)
		{
			rootZetten.zetAantal = newSize;
			TB_Cardinality = 0;
		}
	}

}

}