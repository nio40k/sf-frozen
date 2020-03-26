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

#include <algorithm> // For std::count
#include <cassert>
#include <iostream>
#include <iomanip>

#include "movegen.h"
#include "search.h"
#include "thread.h"
#include "uci.h"

ThreadPool Threads; // Global object
NumaInfo GlobalData;

#if defined(USE_NUMA) || defined(USE_LARGE_PAGES)
  #ifndef NOMINMAX
    #define NOMINMAX // Disable macros min() and max()
  #endif
  #define WIN32_LEAN_AND_MEAN
  #include <windows.h>
  #undef WIN32_LEAN_AND_MEAN
  #undef NOMINMAX
#endif

/// Thread constructor launches the thread and then waits until it goes to sleep
/// in idle_loop().

Thread::Thread() {

  exit = false;
  threadIndex = Threads.threadCount; // Start from 0

  std::unique_lock<Mutex> lk(mutex);
  searching = true;
  nativeThread = std::thread(&Thread::idle_loop, this);
  sleepCondition.wait(lk, [&]{ return !searching; });
}


/// Thread destructor waits for thread termination before returning

Thread::~Thread() {

  mutex.lock();
  exit = true;
  sleepCondition.notify_one();
  mutex.unlock();
  nativeThread.join();
}


/// Thread::wait_for_search_finished() waits on sleep condition
/// until not searching

void Thread::wait_for_search_finished() {

  std::unique_lock<Mutex> lk(mutex);
  sleepCondition.wait(lk, [&]{ return !searching; });
}


/// Thread::wait() waits on sleep condition until condition is true

void Thread::wait(std::atomic_bool& condition) {

  std::unique_lock<Mutex> lk(mutex);
  sleepCondition.wait(lk, [&]{ return bool(condition); });
}


/// Thread::start_zoeken() wakes up the thread that will start the search

void Thread::start_zoeken(bool resume) {

  std::unique_lock<Mutex> lk(mutex);

  if (!resume)
      searching = true;

  sleepCondition.notify_one();
}


/// Thread::idle_loop() is where the thread is parked when it has no work to do

void Thread::idle_loop() {

  void* p = NULL;
#ifdef USE_NUMA
  if (Threads.NUMA_highest_node)
  {
	  Threads.setCurrentThreadAffinity(threadIndex);
	  int numa_node = Threads.NUMA_node_for_thread[threadIndex];
	  ni = Threads.NUMA_data[numa_node];
	  p = VirtualAllocExNuma(GetCurrentProcess(), NULL, sizeof(ThreadInfo), MEM_COMMIT | MEM_RESERVE, PAGE_READWRITE, numa_node);
  }
  else
#endif
  {
	  ni = &GlobalData;
	  p = _aligned_malloc(sizeof(ThreadInfo), 64);
  }
  std::memset(p, 0, sizeof(ThreadInfo));
  ti = new (p) ThreadInfo;

  rootStelling = &ti->rootStelling;

  while (!exit)
  {
      std::unique_lock<Mutex> lk(mutex);

      searching = false;

      while (!searching && !exit)
      {
          sleepCondition.notify_one(); // Wake up any waiting thread
          sleepCondition.wait(lk);
      }

      lk.unlock();

      if (!exit)
          search();
  }
#ifdef USE_NUMA
  if (Threads.NUMA_highest_node)
	  VirtualFree(p, 0, MEM_RELEASE);
  else
#endif
	  _aligned_free(p);
}


/// ThreadPool::init() creates and launches requested threads that will go
/// immediately to sleep. We cannot use a constructor because Threads is a
/// static object and we need a fully initialized engine at this point due to
/// allocation of Endgames in the Thread constructor.

void ThreadPool::init() {

  //printf("Stelling: %zd bytes\n", sizeof(Stelling));
  //printf("StellingInfo: %zd bytes\n", sizeof(StellingInfo));
  //printf("StellingInfo.Key: %zd\n", offsetof(struct StellingInfo, key));

#ifdef USE_NUMA
  init_NUMA();
  if (Threads.NUMA_highest_node)
	  for (unsigned int i = 0; i <= NUMA_highest_node; i++)
		  NUMA_data[i] = (NumaInfo*)VirtualAllocExNuma(GetCurrentProcess(), NULL, sizeof(NumaInfo), MEM_COMMIT | MEM_RESERVE, PAGE_READWRITE, i);
#endif

  threads[0] = new MainThread;
  threadCount = 1;
  endgames.init();
  verander_thread_aantal();
  vijftigZettenAfstand = 50;
  multiPV = 1;
}


/// ThreadPool::exit() terminates threads before the program exits. Cannot be
/// done in destructor because threads must be terminated before deleting any
/// static objects while still in main().

void ThreadPool::exit() {

  while (threadCount > 0)
	  delete threads[--threadCount];

#ifdef USE_NUMA
  if (Threads.NUMA_highest_node)
	  for (unsigned int i = 0; i <= NUMA_highest_node; i++)
		  VirtualFree(NUMA_data[i], 0, MEM_RELEASE);
#endif
}


/// ThreadPool::lees_uci_opties() updates internal threads parameters from the
/// corresponding UCI options and creates/destroys threads to match requested
/// number. Thread objects are dynamically allocated.

void ThreadPool::verander_thread_aantal() {

  size_t uci_threads = Options["Threads"];

  assert(uci_threads > 0);

  while (threadCount < uci_threads)
	  threads[threadCount++] = new Thread;

  while (threadCount > uci_threads)
	  delete threads[--threadCount];

  sync_cout << "info string " << uci_threads << " thread" << (uci_threads > 1 ? "s" : "") << " used" << sync_endl;
}


/// ThreadPool::bezochte_knopen() returns the number of nodes searched

uint64_t ThreadPool::bezochte_knopen() {

  uint64_t nodes = 0;
  for (int i = 0; i < activeThreadCount; ++i)
      nodes += threads[i]->rootStelling->bezochte_knopen();
  nodes += nodes / 7;
  return nodes;
}

uint64_t ThreadPool::tb_hits() {

	uint64_t hits = 0;
	for (int i = 0; i < activeThreadCount; ++i)
		hits += threads[i]->rootStelling->tb_hits();
	return hits;
}

/// ThreadPool::start_thinking() wakes up the main thread sleeping in idle_loop()
/// and starts a new search, then returns immediately.

void ThreadPool::begin_zoek_opdracht(const Stelling& pos,
                                const Search::TijdLimiet& limits) {

  main()->wait_for_search_finished();

  Search::Signals.stopOnPonderhit = Search::Signals.stop = false;
  Search::Limits = limits;

  rootStelling = (Stelling *)&pos;

  main()->start_zoeken();
}


void ThreadPool::wis_counter_zet_geschiedenis()
{
#ifdef USE_NUMA
	if (NUMA_highest_node)
	{
		for (unsigned int i = 0; i <= NUMA_highest_node; i++)
			NUMA_data[i]->CounterMoveHistory.clear();
	}
	else
#endif
		GlobalData.counterZetStatistieken.clear();
}


#ifdef USE_NUMA

int lees_systeem_core_aantal(int& processorCoreCount, int& logicalProcessorCount)
{
	// see https://msdn.microsoft.com/en-us/library/windows/desktop/aa363804(v=vs.85).aspx

	PSYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX buffer = NULL;
	DWORD returnLength = 0;

	bool rc = GetLogicalProcessorInformationEx(RelationProcessorCore, NULL, &returnLength);
	if (GetLastError() != ERROR_INSUFFICIENT_BUFFER)
		return 0;
	buffer = (PSYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX)malloc(returnLength);
	rc = GetLogicalProcessorInformationEx(RelationProcessorCore, buffer, &returnLength);
	if (!rc)
	{
		free(buffer);
		return 0;
	}

	logicalProcessorCount = 0;
	processorCoreCount = 0;
	DWORD byteOffset = 0;
	while (byteOffset < returnLength)
	{
		PSYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX ptr = PSYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX((char *)buffer + byteOffset);
		processorCoreCount++;
		for (int i = 0; i < ptr->Processor.GroupCount; i++)
			// A hyperthreaded core supplies more than one logical processor.
			logicalProcessorCount += popcount(ptr->Processor.GroupMask[i].Mask);
		byteOffset += buffer->Size;
	}
	free(buffer);
	return 1;
}

int ThreadPool::getNumaNodeAffinities()
{
	PSYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX buffer = NULL;
	DWORD returnLength = 0;

	bool rc = GetLogicalProcessorInformationEx(RelationNumaNode, NULL, &returnLength);
	if (GetLastError() != ERROR_INSUFFICIENT_BUFFER)
		return 0;
	buffer = (PSYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX)malloc(returnLength);
	rc = GetLogicalProcessorInformationEx(RelationNumaNode, buffer, &returnLength);
	if (!rc)
	{
		free(buffer);
		return 0;
	}

	DWORD byteOffset = 0;
	while (byteOffset < returnLength)
	{
		PSYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX ptr = PSYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX((char *)buffer + byteOffset);
		if (ptr->NumaNode.NodeNumber < MAX_NUMA_NODES)
		{
			NUMA_affinities[ptr->NumaNode.NodeNumber] = *(MY_GROUP_AFFINITY*)(&ptr->NumaNode.GroupMask);
			sync_cout << "info string NUMA node " << ptr->NumaNode.NodeNumber
				<< " in group " << ptr->NumaNode.GroupMask.Group
				<< " processor mask " << std::hex << std::setw(16) << std::setfill('0') << ptr->NumaNode.GroupMask.Mask << std::dec << sync_endl;
		}
		byteOffset += buffer->Size;
	}
	free(buffer);
	return 1;
}

void ThreadPool::init_NUMA()
{
	typedef BOOL(WINAPI *LPFN_GNHNN) (PULONG);

	NUMA_highest_node = 0;
	if (!GetNumaHighestNodeNumber(&NUMA_highest_node))
		return;
	if (!NUMA_highest_node)
		return;
	if (NUMA_highest_node >= MAX_NUMA_NODES)
		NUMA_highest_node = MAX_NUMA_NODES - 1;
	sync_cout << "info string NUMA configuration with " << NUMA_highest_node + 1 << " node(s), offset " << NUMA_offset << sync_endl;

	int processorCoreCount, logicalProcessorCount;
	if (!getNumaNodeAffinities() || !lees_systeem_core_aantal(processorCoreCount, logicalProcessorCount))
	{
		sync_cout << "info string Error while reading Logical Processor Information" << sync_endl;
		NUMA_highest_node = 0;
		return;
	};
	sync_cout << "info string " << processorCoreCount << " cores with " << logicalProcessorCount << " logical processors detected" << sync_endl;

	const bool HyperThreading = (logicalProcessorCount == 2 * processorCoreCount);
	unsigned int HT_mul = HyperThreading ? 2 : 1;

	int ThreadCount = std::min(logicalProcessorCount, MAX_THREADS);
	for (int i = 0; i < ThreadCount; i++)
	{
		NUMA_node_for_thread[i] = (HT_mul * (NUMA_highest_node + 1) * i / logicalProcessorCount + NUMA_offset) % (NUMA_highest_node + 1);
		//sync_cout << "info string thread " << i << " on node " << NUMA_node_for_thread[i] << sync_endl;
	}
}

void ThreadPool::setCurrentThreadAffinity(int thread_id)
{
	int node = NUMA_node_for_thread[thread_id];
	if (SetThreadGroupAffinity(GetCurrentThread(), (CONST GROUP_AFFINITY *)&NUMA_affinities[node], NULL))
		//sync_cout << "info string set affinity for thread " << thread_id
		//	<< " to group " << NUMA_affinities[node].Group
		//	<< " mask " << std::hex << std::setw(16) << std::setfill('0') << NUMA_affinities[node].Mask << std::dec << sync_endl
		;
	else
		sync_cout << "info string Error while setting affinity for thread " << thread_id << sync_endl;
}

#endif


#ifdef USE_LARGE_PAGES
enum
{
	privilege_unknown,
	privilege_no,
	privilege_yes
};

static int allow_lock_mem = privilege_unknown;

static SIZE_T large_page_size = 0;

bool SetLockMemoryPrivilege()
{
	TOKEN_PRIVILEGES tp;
	LUID luid;
	HANDLE hToken;

	large_page_size = GetLargePageMinimum();
	if (!large_page_size)
		return FALSE;

	if (!LookupPrivilegeValue(NULL, SE_LOCK_MEMORY_NAME, &luid))
		return FALSE;

	if (!OpenProcessToken(GetCurrentProcess(), TOKEN_ADJUST_PRIVILEGES, &hToken))
		return FALSE;

	tp.PrivilegeCount = 1;
	tp.Privileges[0].Luid = luid;
	tp.Privileges[0].Attributes = SE_PRIVILEGE_ENABLED;

	if (!AdjustTokenPrivileges(hToken, FALSE, &tp, 0, NULL, NULL))
	{
		CloseHandle(hToken);
		return FALSE;
	}

	if (GetLastError() == ERROR_NOT_ALL_ASSIGNED)
	{
		CloseHandle(hToken);
		return FALSE;
	}

	CloseHandle(hToken);

	return TRUE;
}
#endif

void *alloc_large_page_mem(size_t size, bool *large_page, bool exit_on_error)
{
	void *result;
#ifdef USE_LARGE_PAGES
	if (allow_lock_mem == privilege_unknown)
		if (SetLockMemoryPrivilege())
			allow_lock_mem = privilege_yes;
		else
			allow_lock_mem = privilege_no;

	if (allow_lock_mem == privilege_yes)
	{
		SIZE_T al_size = (size + large_page_size - 1) & ~(large_page_size - 1);
		result = VirtualAlloc(NULL, al_size, MEM_COMMIT | MEM_LARGE_PAGES, PAGE_READWRITE);
		if (result)
		{
			*large_page = TRUE;
			return result;
		}
	}
#endif

	*large_page = false;
	result = _aligned_malloc(size, 64);

	if (!result && exit_on_error)
	{
		printf("ERROR: Unable to allocate memory\n");
		fflush(stdout);
		exit(1);
	}
	return result;
}

void free_large_page_mem(void *p, bool large_page)
{
#ifdef USE_LARGE_PAGES
	if (large_page)
	{
		VirtualFree(p, 0, MEM_RELEASE);
		return;
	}
#endif

	_aligned_free(p);
}
