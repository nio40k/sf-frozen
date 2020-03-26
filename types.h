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

#ifndef TYPES_H_INCLUDED
#define TYPES_H_INCLUDED

//#define TRACE_LOG
#ifdef TRACE_LOG
void open_tracefile();
void close_tracefile();
void trace_do_move(int zet);
void trace_cancel_move();
void trace_eval(int waarde);
void trace_msg(const char *MSG, int diepte, int alfa, int beta, bool force = false);
void trace_reset_indent();
#endif

//#define TRACE_TM
#ifdef TRACE_TM
void open_tracefile();
void close_tracefile();
void trace_tm_msg(int MoveNumber, int elapsed, int optimum, const char *MSG);
#endif

/// When compiling with provided Makefile (e.g. for Linux and OSX), configuration
/// is done automatically. To get started type 'make help'.
///
/// When Makefile is not used (e.g. with Microsoft Visual Studio) some switches
/// need to be set manually:
///
/// -DNDEBUG      | Disable debugging mode. Always use this for release.
///
/// -DNO_PREFETCH | Disable use of prefetch asm-instruction. You may need this to
///               | run on some very old machines.
///
/// -DUSE_POPCNT  | Add runtime support for use of popcnt asm-instruction. Works
///               | only in 64-bit mode and requires hardware with popcnt support.
///
/// -DUSE_PEXT    | Add runtime support for use of pext asm-instruction. Works
///               | only in 64-bit mode and requires hardware with pext support.

#include <cassert>
#include <cctype>
#include <climits>
#include <cstdint>
#include <cstdlib>
#include <cmath>

#if defined(_MSC_VER)
// Disable some silly and noisy warning from MSVC compiler
#pragma warning(disable: 4127) // Conditional expression is constant
#pragma warning(disable: 4146) // Unary minus operator applied to unsigned type
#pragma warning(disable: 4800) // Forcing value to bool 'true' or 'false'
#pragma warning(disable: 4996) // The function or variable may be unsafe
#endif

/// Predefined macros hell:
///
/// __GNUC__           Compiler is gcc, Clang or Intel on Linux
/// __INTEL_COMPILER   Compiler is Intel
/// _MSC_VER           Compiler is MSVC or Intel on Windows
/// _WIN32             Building on Windows (any)
/// _WIN64             Building on Windows 64 bit

#if defined(_WIN64) && defined(_MSC_VER) // No Makefile used
#  include <intrin.h> // Microsoft header for _BitScanForward64()
#  define IS_64BIT
#endif

#if defined(USE_POPCNT) && (defined(__INTEL_COMPILER) || defined(_MSC_VER))
#  include <nmmintrin.h> // Intel and Microsoft header for _mm_popcnt_u64()
#endif

#if !defined(NO_PREFETCH) && (defined(__INTEL_COMPILER) || defined(_MSC_VER))
#  include <xmmintrin.h> // Intel and Microsoft header for _mm_prefetch()
#endif

#if defined(USE_PEXT)
#  include <immintrin.h> // Header for _pext_u64() intrinsic
#  define pext(bezet, mask) _pext_u64(bezet, mask)
#else
#  define pext(bezet, mask) (0)
#endif

#ifdef USE_POPCNT
const bool HasPopCnt = true;
#else
const bool HasPopCnt = false;
#endif

#ifdef USE_PEXT
const bool HasPext = true;
#else
const bool HasPext = false;
#endif

#ifdef IS_64BIT
const bool Is64Bit = true;
#else
const bool Is64Bit = false;
#endif

typedef uint64_t Sleutel64;
typedef uint64_t Bitboard;

const int MAX_ZETTEN = 220;
const int MAX_PLY = 128;
const int MAX_PV = 63;

/// A move needs 16 bits to be stored
///
/// bit  0- 5: destination square (from 0 to 63)
/// bit  6-11: origin square (from 0 to 63)
/// bit 12-13: promotion piece type - 2 (from PAARD-2 to DAME-2)
/// bit 14-15: special move flag: promotion (1), en passant (2), castling (3)
/// NOTE: EN-PASSANT bit is set only when a pawn can be captured
///
/// Special cases are GEEN_ZET and NULL_ZET. We can sneak these in because in
/// any normal move destination square is always different from origin square
/// while GEEN_ZET and NULL_ZET have the same origin and destination square.

enum Zet : unsigned int {
  GEEN_ZET,
  NULL_ZET = 65
};

enum ZetType {
  NORMAAL,
  PROMOTIE  = 1 << 14,
  ENPASSANT = 2 << 14,
  ROKADE    = 3 << 14
};

enum Kleur {
  WIT, ZWART, KLEUR_N = 2
};

enum RokadeMogelijkheid {
  GEEN_ROKADE,
  WIT_KORT = 1,
  WIT_LANG = 2,
  ZWART_KORT = 4,
  ZWART_LANG = 8,
  ALLE_ROKADE = WIT_KORT | WIT_LANG | ZWART_KORT | ZWART_LANG,
  ROKADE_MOGELIJK_N = 16
};

enum PartijFase {
  EINDSPEL_FASE,
  MIDDENSPEL_FASE = 26,
};

enum SchaalFactor {
  REMISE_FACTOR   = 0,
  EEN_PION_FACTOR = 75,
  NORMAAL_FACTOR  = 100,
  MAX_FACTOR      = 200,
  GEEN_FACTOR     = 255
};

enum SorteerWaarde : int {
	SORT_ZERO = 0,
	SORT_MAX = 999999
};

enum EvalWaarde : int {
	EVAL_0 = 0,
	REMISE_EVAL = 0,
	GEEN_EVAL = 199999,
	PION_EVAL  = 100 * 16,
	LOPER_EVAL = 360 * 16
};

enum MateriaalWaarde : int {
	MATL_0 = 0,
	MATL_PAARD = 41,
	MATL_LOPER = 42,
	MATL_TOREN = 64,
	MATL_DAME  = 127,
};

enum SeeWaarde : int {
	SEE_0 = 0,
	SEE_PION  = 100,
	SEE_PAARD = 325,
	SEE_LOPER = 350,
	SEE_TOREN = 500,
	SEE_DAME  = 950
};

enum Waarde : int {
  WAARDE_0 = 0,
  WAARDE_1 = 1,
  REMISE_WAARDE  = 0,
  WINST_WAARDE   = 15000,
  LANGSTE_MAT_WAARDE = 32000,
  MAT_WAARDE         = 32700,
  HOOGSTE_WAARDE     = 32750,
  GEEN_WAARDE        = 32750,
};

inline Waarde maak_waarde(int x) {
	return Waarde(x * 4 / 5);
}

#define DEFAULT_DRAW_VALUE maak_waarde(30)
#define WAARDE_TEMPO maak_waarde(30)
#define VALUE_AVERAGE_PAWN maak_waarde(198)
#define WAARDE_PION maak_waarde(250)

enum StukType : uint8_t {
  GEEN_STUKTYPE, KONING, PION, PAARD, LOPER, TOREN, DAME,
  ALLE_STUKKEN = 0, STUKKEN_ZONDER_KONING = 7,
  STUKTYPE_N = 8
};

enum Stuk : uint8_t {
  GEEN_STUK,
  W_KONING = 1, W_PION, W_PAARD, W_LOPER, W_TOREN, W_DAME,
  Z_KONING = 9, Z_PION, Z_PAARD, Z_LOPER, Z_TOREN, Z_DAME,
  STUK_N = 16
};

const MateriaalWaarde MateriaalWaarden[STUK_N] = {
	MATL_0, MATL_0, MATL_0, MATL_PAARD, MATL_LOPER, MATL_TOREN, MATL_DAME, MATL_0,
	MATL_0, MATL_0, MATL_0, MATL_PAARD, MATL_LOPER, MATL_TOREN, MATL_DAME, MATL_0
};

inline Waarde WaardeVanMateriaal(MateriaalWaarde v) { return Waarde(maak_waarde(20) * (int)v); }

const int StukFase[STUK_N] = { 
	0, 0, 0, 1, 1, 3, 6, 0,
	0, 0, 0, 1, 1, 3, 6, 0
};
const int StukContempt[STUK_N] = { 
	0, 0, 1, 2, 2, 4, 6, 0,
	0, 0, 1, 2, 2, 4, 6, 0
};

const int MATERIAAL_HASH_GROOTTE = 16384;
const int PION_HASH_GROOTTE = 16384;

#define COMPOUND_PLY

#ifdef COMPOUND_PLY
  inline int displayIteration(int iter) { return iter; }
  const int DEFAULT_BENCHMARK_DEPTH = 12;
#else
  inline int displayIteration(int iter) { return iter - iter / 7; }
  const int DEFAULT_BENCHMARK_DEPTH = 12;
#endif

enum Diepte {

#ifdef COMPOUND_PLY
  PLY = 8,
  MAIN_THREAD_INC = 9,
  OTHER_THREAD_INC = 8,
#else
  PLY = 1,
  MAIN_THREAD_INC = 1,
  OTHER_THREAD_INC = 1,
#endif

  DIEPTE_0 = 0,
  QS_SCHAAK_DIEPTE        =  0,
  QS_ZONDER_SCHAAK_DIEPTE = -1 * int(PLY),
  QS_SLAG_OP_VELD_DIEPTE  = -5 * int(PLY),
  GEEN_DIEPTE = -6 * int(PLY),
  MAX_DIEPTE = MAX_PLY * int(PLY)
};

enum Veld : int8_t {
  SQ_A1, SQ_B1, SQ_C1, SQ_D1, SQ_E1, SQ_F1, SQ_G1, SQ_H1,
  SQ_A2, SQ_B2, SQ_C2, SQ_D2, SQ_E2, SQ_F2, SQ_G2, SQ_H2,
  SQ_A3, SQ_B3, SQ_C3, SQ_D3, SQ_E3, SQ_F3, SQ_G3, SQ_H3,
  SQ_A4, SQ_B4, SQ_C4, SQ_D4, SQ_E4, SQ_F4, SQ_G4, SQ_H4,
  SQ_A5, SQ_B5, SQ_C5, SQ_D5, SQ_E5, SQ_F5, SQ_G5, SQ_H5,
  SQ_A6, SQ_B6, SQ_C6, SQ_D6, SQ_E6, SQ_F6, SQ_G6, SQ_H6,
  SQ_A7, SQ_B7, SQ_C7, SQ_D7, SQ_E7, SQ_F7, SQ_G7, SQ_H7,
  SQ_A8, SQ_B8, SQ_C8, SQ_D8, SQ_E8, SQ_F8, SQ_G8, SQ_H8,
  SQ_NONE = 127,

  VELD_N = 64,

  BOVEN =  8,
  ONDER = -8,
  LINKS = -1,
  RECHTS = 1,

  RECHTSBOVEN = (int8_t)BOVEN + RECHTS,
  RECHTSONDER = (int8_t)ONDER + RECHTS,
  LINKSONDER = (int8_t)ONDER + LINKS,
  LINKSBOVEN = (int8_t)BOVEN + LINKS
};

enum Lijn {
  LIJN_A, LIJN_B, LIJN_C, LIJN_D, LIJN_E, LIJN_F, LIJN_G, LIJN_H, LIJN_N
};

enum Rij {
  RIJ_1, RIJ_2, RIJ_3, RIJ_4, RIJ_5, RIJ_6, RIJ_7, RIJ_8, RIJ_N
};

enum PikZetEtappe {
	NORMAAL_ZOEKEN, GOEDE_SLAGEN_GEN, GOEDE_SLAGEN, KILLERS, KILLERS1, RUSTIGE_ZETTEN_GEN, RUSTIGE_ZETTEN, SLECHTE_SLAGEN,
	GA_UIT_SCHAAK, UIT_SCHAAK_GEN, UIT_SCHAAK_LUS,
	QSEARCH_MET_SCHAAK, QSEARCH_1, QSEARCH_1_SLAGZETTEN, QSEARCH_SCHAAKZETTEN,
	QSEARCH_ZONDER_SCHAAK, QSEARCH_2, QSEARCH_2_SLAGZETTEN,
	PROBCUT, PROBCUT_GEN, PROBCUT_SLAGZETTEN,
	TERUGSLAG_GEN, TERUGSLAG_ZETTEN
};

/// Score enum stores a middlegame and an endgame value in a single integer
/// (enum). The least significant 16 bits are used to store the endgame value
/// and the upper 16 bits are used to store the middlegame value.
enum Score : int { SCORE_ZERO };

#if 1
inline int ScoreMultiply(int x) { return x * 8 / 5; }
//inline Waarde ValueFromEval(EvalWaarde v) { return maak_waarde(v * 5 / 32); }
inline Waarde ValueFromEval(EvalWaarde v) { return Waarde(v / 8); }
#else
inline int ScoreMultiply(int x) { return x; }
inline Value ValueFromEval(EvalWaarde v) { return maak_waarde(v / 4); }
#endif

inline Score maak_score(int mg, int eg) {
  return Score((ScoreMultiply(mg) << 16) + ScoreMultiply(eg));
}

inline Score remake_score(EvalWaarde mg, EvalWaarde eg) {
	return Score(((int)mg << 16) + (int)eg);
}

/// Extracting the signed lower and upper 16 bits is not so trivial because
/// according to the standard a simple cast to short is implementation defined
/// and so is a right shift of a signed integer.
inline EvalWaarde mg_waarde(Score s) {

  union { uint16_t u; int16_t s; } mg = { uint16_t(unsigned(s + 0x8000) >> 16) };
  return EvalWaarde(mg.s);
}

inline EvalWaarde eg_waarde(Score s) {

  union { uint16_t u; int16_t s; } eg = { uint16_t(unsigned(s)) };
  return EvalWaarde(eg.s);
}

#define ENABLE_BASE_OPERATORS_ON(T)                             \
inline T operator+(T d1, T d2) { return T(int(d1) + int(d2)); } \
inline T operator-(T d1, T d2) { return T(int(d1) - int(d2)); } \
inline T operator*(int i, T d) { return T(i * int(d)); }        \
inline T operator*(T d, int i) { return T(int(d) * i); }        \
inline T operator*(unsigned int i, T d) { return T(i * int(d)); }        \
inline T operator*(T d, unsigned int i) { return T(int(d) * i); }        \
inline T operator-(T d) { return T(-int(d)); }                  \
inline T& operator+=(T& d1, T d2) { return d1 = d1 + d2; }      \
inline T& operator-=(T& d1, T d2) { return d1 = d1 - d2; }      \
inline T& operator*=(T& d, int i) { return d = T(int(d) * i); }

#define ENABLE_FULL_OPERATORS_ON(T)                             \
ENABLE_BASE_OPERATORS_ON(T)                                     \
inline T& operator++(T& d) { return d = T(int(d) + 1); }        \
inline T& operator--(T& d) { return d = T(int(d) - 1); }        \
inline T operator/(T d, int i) { return T(int(d) / i); }        \
inline int operator/(T d1, T d2) { return int(d1) / int(d2); }  \
inline T& operator/=(T& d, int i) { return d = T(int(d) / i); }

ENABLE_FULL_OPERATORS_ON(Waarde)
ENABLE_FULL_OPERATORS_ON(StukType)
ENABLE_FULL_OPERATORS_ON(Stuk)
ENABLE_FULL_OPERATORS_ON(Kleur)
ENABLE_FULL_OPERATORS_ON(Diepte)
ENABLE_FULL_OPERATORS_ON(Veld)
ENABLE_FULL_OPERATORS_ON(Lijn)
ENABLE_FULL_OPERATORS_ON(Rij)
ENABLE_FULL_OPERATORS_ON(SorteerWaarde)
ENABLE_FULL_OPERATORS_ON(EvalWaarde)

ENABLE_BASE_OPERATORS_ON(Score)
ENABLE_BASE_OPERATORS_ON(MateriaalWaarde)
ENABLE_BASE_OPERATORS_ON(SeeWaarde)

#undef ENABLE_FULL_OPERATORS_ON
#undef ENABLE_BASE_OPERATORS_ON

/// Only declared but not defined. We don't want to multiply two scores due to
/// a very high risk of overflow. So user should explicitly convert to integer.
inline Score operator*(Score s1, Score s2);

/// Division of a Score must be handled separately for each term
inline Score operator/(Score s, int i) {
  return remake_score(mg_waarde(s) / i, eg_waarde(s) / i);
}

inline Kleur operator~(Kleur c) {
  return Kleur(c ^ 1);
}

inline Veld operator~(Veld s) {
  return Veld(s ^ 56); // Vertical flip SQ_A1 -> SQ_A8
}

inline Waarde geeft_mat(int ply) {
  return Waarde(MAT_WAARDE - ply);
}

inline Waarde staat_mat(int ply) {
  return Waarde(-MAT_WAARDE + ply);
}

inline Veld maak_veld(Lijn f, Rij r) {
  return Veld((r << 3) + f);
}

inline Stuk maak_stuk(Kleur c, StukType pt) {
  return Stuk((c << 3) + pt);
}

inline StukType stuk_type(Stuk pc) {
  return StukType(pc & 7);
}

inline Kleur stuk_kleur(Stuk pc) {
  assert(pc != GEEN_STUK);
  return Kleur(pc >> 3);
}

inline bool is_ok(Veld s) {
  return s >= SQ_A1 && s <= SQ_H8;
}

inline Lijn lijn(Veld s) {
  return Lijn(s & 7);
}

inline Rij rij(Veld s) {
  return Rij(s >> 3);
}

inline Veld relatief_veld(Kleur c, Veld s) {
  return Veld(s ^ (c * 56));
}

inline Rij relatieve_rij(Kleur c, Rij r) {
  return Rij(r ^ (c * 7));
}

inline Rij relatieve_rij(Kleur c, Veld s) {
  return relatieve_rij(c, rij(s));
}

inline bool verschillende_kleur(Veld s1, Veld s2) {
  int s = int(s1) ^ int(s2);
  return ((s >> 3) ^ s) & 1;
}

inline Veld pion_vooruit(Kleur c) {
  return c == WIT ? BOVEN : ONDER;
}

inline Veld van_veld(Zet zet) {
  return Veld((zet >> 6) & 0x3F);
}

inline Veld naar_veld(Zet zet) {
  return Veld(zet & 0x3F);
}

inline ZetType zet_type(Zet zet) {
  return ZetType(zet & (3 << 14));
}

inline StukType promotie_stuk(Zet zet) {
  return StukType(((zet >> 12) & 3) + PAARD);
}

inline Zet maak_zet(Veld van, Veld naar) {
  return Zet(naar + (van << 6));
}

template<ZetType T>
inline Zet maak_zet(Veld van, Veld naar, StukType pt = PAARD) {
  return Zet(naar + (van << 6) + T + ((pt - PAARD) << 12));
}

inline bool is_ok(Zet zet) {
  return zet != GEEN_ZET && zet != NULL_ZET;
}

#endif // #ifndef TYPES_H_INCLUDED
