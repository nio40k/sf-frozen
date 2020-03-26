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

#include <fstream>
#include <iomanip>
#include <iostream>
#include <sstream>

#include "misc.h"
#include "thread.h"

using namespace std;

namespace {

/// Version number. If Version is left empty, then compile date in the format
/// DD-MM-YY and show in engine_info.
const string Version = "";

/// Our fancy logging facility. The trick here is to replace cin.rdbuf() and
/// cout.rdbuf() with two Tie objects that tie cin and cout to a file stream. We
/// can toggle the logging of std::cout and std:cin at runtime whilst preserving
/// usual I/O functionality, all without changing a single line of code!
/// Idea from http://groups.google.com/group/comp.lang.c++/msg/1d941c0f26ea0d81

struct Tie: public streambuf { // MSVC requires split streambuf for cin and cout

  Tie(streambuf* b, streambuf* l) : buf(b), logBuf(l) {}

  int sync() { return logBuf->pubsync(), buf->pubsync(); }
  int overflow(int c) { return log(buf->sputc((char)c), "<< "); }
  int underflow() { return buf->sgetc(); }
  int uflow() { return log(buf->sbumpc(), ">> "); }

  streambuf *buf, *logBuf;

  int log(int c, const char* prefix) {

    static int last = '\n'; // Single log file

    if (last == '\n')
        logBuf->sputn(prefix, 3);

    return last = logBuf->sputc((char)c);
  }
};

class Logger {

  Logger() : in(cin.rdbuf(), file.rdbuf()), out(cout.rdbuf(), file.rdbuf()) {}
 ~Logger() { start(""); }

  ofstream file;
  Tie in, out;

public:
  static void start(const std::string& fname) {

    static Logger l;

    if (!fname.empty() && !l.file.is_open())
    {
        l.file.open(fname, ifstream::out);
        cin.rdbuf(&l.in);
        cout.rdbuf(&l.out);
    }
    else if (fname.empty() && l.file.is_open())
    {
        cout.rdbuf(l.out.buf);
        cin.rdbuf(l.in.buf);
        l.file.close();
    }
  }
};

#define YEAR ((((__DATE__ [7] - '0') * 10 + (__DATE__ [8] - '0')) * 10 + (__DATE__ [9] - '0')) * 10 + (__DATE__ [10] - '0'))

#define MONTH (__DATE__ [2] == 'n' ? (__DATE__ [1] == 'a' ? 0 : 5) \
: __DATE__ [2] == 'b' ? 1 \
: __DATE__ [2] == 'r' ? (__DATE__ [0] == 'M' ? 2 : 3) \
: __DATE__ [2] == 'y' ? 4 \
: __DATE__ [2] == 'l' ? 6 \
: __DATE__ [2] == 'g' ? 7 \
: __DATE__ [2] == 'p' ? 8 \
: __DATE__ [2] == 't' ? 9 \
: __DATE__ [2] == 'v' ? 10 : 11)

#define DAY ((__DATE__ [4] == ' ' ? 0 : __DATE__ [4] - '0') * 10 + (__DATE__ [5] - '0'))

#if defined(CHESSKING)
#define CHESSOK
#if defined(PREMIUM)
#define VERSION_NAME " for Chess King Pro"
#else
#define VERSION_NAME " for Chess King"
#endif
#elif defined(IDEA)
#define CHESSOK
#define VERSION_NAME " for IDeA"
#elif defined(AQUARIUM) || defined(CHESS_ASSISTANT)
#define CHESSOK
#ifdef PREMIUM
#define VERSION_NAME " Pro"
#else
#define VERSION_NAME ""
#endif
#else
#ifdef PREMIUM
#define VERSION_NAME " Pro"
#else
#define VERSION_NAME ""
#endif
#endif

#if defined(CHESSKING)
#define PROGRAM_NAME "Houdini"
#else
#define PROGRAM_NAME "Houdini"
#endif

#if defined(IS_64BIT)
//#define PLATFORM_NAME "x64"
#define PLATFORM_NAME ""
#else
#define PLATFORM_NAME "w32"
#endif

#ifdef __ANDROID__
#define NAME PROGRAM_NAME
#else
#define NAME PROGRAM_NAME VERSION_NAME " " PLATFORM_NAME
#endif

const int name_offsets[14] =
{
	97 * ('R' - '§'), 97 * ('o' - 'R'), 97 * ('b' - 'o'), 97 * ('e' - 'b'), 97 * ('r' - 'e'), 97 * ('t' - 'r'), 97 * (' ' - 't'),
	97 * ('H' - ' '), 97 * ('o' - 'H'), 97 * ('u' - 'o'), 97 * ('d' - 'u'), 97 * ('a' - 'd'), 97 * ('r' - 'a'), 97 * ('t' - 'r')
};

void write_own_name(char *mijn_naam)
{
	// onnozele functie om hex editing van de EXE file moeilijker te maken
	char c = '§';
	for (int n = 0; n < 14; n++)
	{
		c += name_offsets[n] / 97;
		mijn_naam[n] = c;
	}
	mijn_naam[14] = 0;
}

} // namespace

/// engine_info() returns the full name of the current Stockfish version. This
/// will be either "Stockfish <Tag> DD-MM-YY" (where DD-MM-YY is the date when
/// the program was compiled) or "Stockfish <Version>", depending on whether
/// Version is empty.

const string engine_info(bool to_uci) {
#if 1
	stringstream ss;
	char mijn_naam[15];
	write_own_name(mijn_naam);

	ss << NAME << YEAR << setw(2) << setfill('0') << MONTH+1 << setw(2) << DAY << endl;
	ss << (to_uci ? "id author " : "(c) 2016 ") << mijn_naam << endl;
	return ss.str();
#else
  const string months("Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec");
  string month, day, year;
  stringstream ss, date(__DATE__); // From compiler, format is "Sep 21 2008"

  ss << "Stockfish " << Version << setfill('0');

  if (Version.empty())
  {
      date >> month >> day >> year;
      ss << setw(2) << day << setw(2) << (1 + months.find(month) / 4) << year.substr(2);
  }

  ss << (Is64Bit ? " 64" : "")
     << (HasPext ? " BMI2" : (HasPopCnt ? " POPCNT" : ""))
     << (to_uci  ? "\nid author ": " by ")
     << "T. Romstad, M. Costalba, J. Kiiski, G. Linscott";
  return ss.str();
#endif
}


/// Debug functions used mainly to collect run-time statistics
const size_t MAX_STATS = 32;
static int64_t hits[MAX_STATS][2], means[MAX_STATS][2], squares[MAX_STATS][2], freq[MAX_STATS], tot_freq;

void dbg_hit_on(int n, bool b) { if (n >= 0 && n < MAX_STATS) { ++hits[n][0]; if (b) ++hits[n][1]; } }
void dbg_hit_on(bool c, int n, bool b) { if (n >= 0 && n < MAX_STATS) { if (c) dbg_hit_on(n, b); } }
void dbg_mean_of(int n, int v) { if (n >= 0 && n < MAX_STATS) { ++means[n][0]; means[n][1] += v; } }
void dbg_square_of(int n, int v) { if (n >= 0 && n < MAX_STATS) { ++squares[n][0]; squares[n][1] += v*v; } }
void dbg_freq(int n) { ++tot_freq;  if (n >= 0 && n < MAX_STATS) { ++freq[n]; } }

void dbg_print() {

  for (int n = 0; n < MAX_STATS; n++) if (hits[n][0])
      cerr << "Trace " << std::setw(2) << n << " Total " << std::setw(10) << hits[n][0] << " Hits " << std::setw(10) << hits[n][1]
           << " hit rate " << std::setw(5) << std::fixed << std::setprecision(1) << 100.0 * hits[n][1] / hits[n][0] << " %" << endl;

  for (int n = 0; n < MAX_STATS; n++) if (means[n][0])
      cerr << "Trace " << n << " Total " << means[n][0] << " Mean "
	  << std::fixed << std::setprecision(1) << (double)means[n][1] / means[n][0] << endl;

  for (int n = 0; n < MAX_STATS; n++) if (squares[n][0])
	  cerr << "Trace " << n << " Total " << squares[n][0] << " Average Square "
	  << std::fixed << std::setprecision(1) << sqrt((double)squares[n][1] / squares[n][0]) << endl;

  if (tot_freq) for (int n = 0; n < MAX_STATS; n++) if (freq[n])
	  cerr << "Trace " << std::setw(2) << n << " Hits " << std::setw(10) << freq[n]
	  << " hit rate " << std::setw(5) << std::fixed << std::setprecision(1) << 100.0 * freq[n] / tot_freq << " %" << endl;
}


/// Used to serialize access to std::cout to avoid multiple threads writing at
/// the same time.

std::ostream& operator<<(std::ostream& os, SyncCout sc) {

  static Mutex mutex;

  if (sc == IO_LOCK)
      mutex.lock();

  if (sc == IO_UNLOCK)
      mutex.unlock();

  return os;
}


/// Trampoline helper to avoid moving Logger to misc.h
void start_logger(const std::string& fname) { Logger::start(fname); }


/// prefetch() preloads the given address in L1/L2 cache. This is a non-blocking
/// function that doesn't stall the CPU waiting for data to be loaded from memory,
/// which can be quite slow.
#ifdef NO_PREFETCH

void prefetch(void*) {}

#else

void prefetch(void* addr) {

#  if defined(__INTEL_COMPILER)
   // This hack prevents prefetches from being optimized away by
   // Intel compiler. Both MSVC and gcc seem not be affected by this.
   __asm__ ("");
#  endif

#  if defined(__INTEL_COMPILER) || defined(_MSC_VER)
  _mm_prefetch((char*)addr, _MM_HINT_T0);
#  else
  __builtin_prefetch(addr);
#  endif
}

void prefetch2(void* addr) {

#  if defined(__INTEL_COMPILER)
	// This hack prevents prefetches from being optimized away by
	// Intel compiler. Both MSVC and gcc seem not be affected by this.
	__asm__("");
#  endif

#  if defined(__INTEL_COMPILER) || defined(_MSC_VER)
	_mm_prefetch((char*)addr, _MM_HINT_T0);
	_mm_prefetch((char*)addr + 64, _MM_HINT_T0);
#  else
	__builtin_prefetch(addr);
	__builtin_prefetch(addr + 64);
#  endif
}

#endif
