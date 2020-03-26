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

#ifndef THREAD_H_INCLUDED
#define THREAD_H_INCLUDED

//#define USE_NUMA
#define USE_LARGE_PAGES
#ifdef USE_NUMA
  // minstens Windows 7 nodig voor SetThreadGroupAffinity
  #undef _WIN32_WINNT
  #define _WIN32_WINNT 0x0601
#endif

#include <atomic>
#include <condition_variable>
#include <mutex>
#include <thread>

#include "material.h"
#include "movepick.h"
#include "pawns.h"
#include "position.h"
#include "search.h"
#include "thread_win32.h"

const int MAX_THREADS = 80;
const int MAX_NUMA_NODES = 16;

void *alloc_large_page_mem(size_t size, bool *large_page, bool exit_on_error);

struct NumaInfo {
	CounterZetHistoriek counterZetStatistieken;
};

struct ThreadInfo {
	Stelling rootStelling;
	StellingInfo stellingInfo[1024];
	ZetEx zettenLijst[8192];
	ZetWaardeStatistiek history;
	ZetWaardeStatistiek evasionHistory;
	MaxWinstStats maxWinstTabel;
	MoveStats counterZetten;
	Materiaal::Tabel materiaalTabel;
	Pionnen::Tabel pionnenTabel;
};

class Thread {

  std::thread nativeThread;
  Mutex mutex;
  ConditionVariable sleepCondition;
  bool exit, searching;
  int threadIndex;

public:
  Thread();
  virtual ~Thread();
  virtual void search();
  void idle_loop();
  void start_zoeken(bool resume = false);
  void wait_for_search_finished();
  void wait(std::atomic_bool& b);

  ThreadInfo* ti;
  NumaInfo* ni;
  Stelling* rootStelling;

  Search::RootZetten rootZetten;
  Diepte afgewerkteDiepte, vorigeTactischeDiepte;
  bool tactischeModusGebruikt;
  int actievePV;
};


/// MainThread is a derived class with a specific overload for the main thread

struct MainThread : public Thread {
  virtual void search();

  bool snelleZetGespeeld, failedLow;
  int besteZetVerandert;
  Waarde vorigeRootScore;
  int interruptTeller;
  bool snelleZetGetest;
  Diepte vorigeRootDiepte;
  bool snelleZetToegelaten;
};


/// ThreadPool struct handles all the threads-related stuff like init, starting,
/// parking and, most importantly, launching a thread. All the access to threads
/// data is done through this class.

struct MY_GROUP_AFFINITY {
	uint64_t Mask;
	uint16_t Group;
	uint16_t Reserved[3];
};

struct ThreadPool {

  void init(); // No constructor and destructor, threads rely on globals that should
  void exit(); // be initialized and valid during the whole thread lifetime.

  int threadCount;
  Thread* threads[MAX_THREADS];
  MainThread* main() { return static_cast<MainThread*>(threads[0]); }
  void begin_zoek_opdracht(const Stelling&, const Search::TijdLimiet&);
  void verander_thread_aantal();
  uint64_t bezochte_knopen();
  uint64_t tb_hits();
  void wis_counter_zet_geschiedenis();

  int activeThreadCount; // kan verschillend zijn, bij SpeelSterkte < 100 wordt maar 1 thread gebruikt
  Kleur rootKleur;
  int stukContempt;
  Eindspelen endgames;
  Stelling* rootStelling;
  Search::RootZetten rootZetten;
  StellingInfo* rootStellingInfo;
  bool analyseModus;
  bool tactischeModus;
  int vijftigZettenAfstand;
  int multiPV, multiPV_cp;
  int speelSterkte;
  uint64_t speelSterkteMaxKnopen;
private:

#ifdef USE_NUMA
  MY_GROUP_AFFINITY NUMA_affinities[MAX_NUMA_NODES];
  int NUMA_offset = 0;
  void init_NUMA();
  int getNumaNodeAffinities();
public:
  unsigned long NUMA_highest_node;
  NumaInfo* NUMA_data[MAX_NUMA_NODES];
  int NUMA_node_for_thread[MAX_THREADS];
  void setCurrentThreadAffinity(int thread_id);
#endif
};

extern ThreadPool Threads;

#endif // #ifndef THREAD_H_INCLUDED
