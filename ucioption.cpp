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

#define NALIMOV

#include <algorithm>
#include <cassert>
#include <ostream>
#include <iostream>

#include "misc.h"
#include "search.h"
#include "thread.h"
#include "tt.h"
#include "uci.h"
#include "syzygy/tbprobe.h"
#include "evaluate.h"
#ifdef NALIMOV
#include "egtb_nalimovprobe.h"
#endif

using std::string;

UCI::OptionsMap Options; // Global object
int TUNE_1, TUNE_2, TUNE_3, TUNE_4, TUNE_5, TUNE_6, TUNE_7, TUNE_8, TUNE_9, TUNE_10;
int TUNE_11, TUNE_12, TUNE_13, TUNE_14, TUNE_15, TUNE_16, TUNE_17, TUNE_18, TUNE_19, TUNE_20;

extern void Syzygy_init(const std::string& path);

namespace UCI {

/// 'On change' actions, triggered by an option's value change
void on_clear_hash(const Optie&) { Search::clear(); sync_cout << "info string Hash cleared" << sync_endl; }
void on_hash_size(const Optie& o) { TT.verander_grootte(o); }
void on_logger(const Optie& o) { start_logger(o); }
void on_threads(const Optie&) { Threads.verander_thread_aantal(); }
void on_syzygy_path(const Optie& o) { Syzygy_init(o); }
#ifdef NALIMOV
void on_nalimov_path(const Optie& o) { Nalimov_init(o); }
void on_nalimov_cache(const Optie& o) { Nalimov_setCache(o, true); }
#endif
void on_50move_distance(const Optie& o) { Stelling::init_hash_move50(o); }

void on_save_hash_to_file(const Optie& o) {
	if (TT.save_to_file(Options["Hash File"]))
		sync_cout << "info string Hash Saved to File" << sync_endl;
	else
		sync_cout << "info string Failure to Save Hash to File" << sync_endl;
}

void on_load_hash_from_file(const Optie& o) {
	if (!Options["Never Clear Hash"])
	{
		Options["Never Clear Hash"] = true;
		sync_cout << "info string Enabling Never Clear Hash option, please enable this option in the Houdini configuration" << sync_endl;
	}
	if (TT.load_from_file(Options["Hash File"]))
		sync_cout << "info string Hash Loaded from File" << sync_endl;
	else
		sync_cout << "info string Failure to Load Hash from File" << sync_endl;
}

int sterkte_voor_elo(int elo)
{
	if (elo <= 1200)
		return 0;
	else if (elo <= 1800)
		return (elo - 1200) / 30;
	else if (elo <= 2000)
		return (elo - 1800) / 20 + 20;
	else if (elo <= 2300)
		return (elo - 2000) / 15 + 30;
	else if (elo <= 3000)
		return (elo - 2300) / 14 + 50;
	else
		return 100;
}

int elo_voor_sterkte(int sterkte)
{
	if (sterkte == 0)
		return 1200;
	else if (sterkte <= 20)
		return 1200 + 30 * sterkte;
	else if (sterkte <= 30)
		return 1800 + 20 * (sterkte - 20);
	else if (sterkte <= 50)
		return 2000 + 15 * (sterkte - 30);
	else if (sterkte < 100)
		return 2300 + 14 * (sterkte - 50);
	else
		return 3000;
}

void on_uci_elo(const Optie& o)
{
	int elo = int(o);
	elo = std::min(std::max(elo, 1400), 3200);
	if (elo < 3200)
		sync_cout << "info string UCI_Elo " << elo << " is translated to Strength " << sterkte_voor_elo(elo - 200) << sync_endl;
}

void on_strength(const Optie& o)
{
	int strength = int(o);
	strength = std::min(std::max(strength, 0), 100);
	if (strength < 100)
		sync_cout << "info string Strength " << strength << " approximately corresponds to " << elo_voor_sterkte(strength) + 200 << " Elo" << sync_endl;
}

void on_tune1(const Optie& o) { TUNE_1 = o; Eval::init_tune(); }
void on_tune2(const Optie& o) { TUNE_2 = o; Eval::init_tune(); }
void on_tune3(const Optie& o) { TUNE_3 = o; Eval::init_tune(); }
void on_tune4(const Optie& o) { TUNE_4 = o; Eval::init_tune(); }
void on_tune5(const Optie& o) { TUNE_5 = o; Eval::init_tune(); }
void on_tune6(const Optie& o) { TUNE_6 = o; Eval::init_tune(); }
void on_tune7(const Optie& o) { TUNE_7 = o; Eval::init_tune(); }
void on_tune8(const Optie& o) { TUNE_8 = o; Eval::init_tune(); }
void on_tune9(const Optie& o) { TUNE_9 = o; Eval::init_tune(); }
void on_tune10(const Optie& o) { TUNE_10 = o; Eval::init_tune(); }
void on_tune11(const Optie& o) { TUNE_11 = o; Eval::init_tune(); }
void on_tune12(const Optie& o) { TUNE_12 = o; Eval::init_tune(); }
void on_tune13(const Optie& o) { TUNE_13 = o; Eval::init_tune(); }
void on_tune14(const Optie& o) { TUNE_14 = o; Eval::init_tune(); }
void on_tune15(const Optie& o) { TUNE_15 = o; Eval::init_tune(); }
void on_tune16(const Optie& o) { TUNE_16 = o; Eval::init_tune(); }
void on_tune17(const Optie& o) { TUNE_17 = o; Eval::init_tune(); }
void on_tune18(const Optie& o) { TUNE_18 = o; Eval::init_tune(); }
void on_tune19(const Optie& o) { TUNE_19 = o; Eval::init_tune(); }
void on_tune20(const Optie& o) { TUNE_20 = o; Eval::init_tune(); }


/// Our case insensitive less() function as required by UCI protocol
bool CaseInsensitiveLess::operator() (const string& s1, const string& s2) const {

  return std::lexicographical_compare(s1.begin(), s1.end(), s2.begin(), s2.end(),
         [](char c1, char c2) { return tolower(c1) < tolower(c2); });
}


/// init() initializes the UCI options to their hard-coded default values

void init(OptionsMap& o) {

  const int MaxHashMB = Is64Bit ? 128 * 1024 : 1024;

  o["Tactical Mode"]         << Optie(false);
  o["Hash"]                  << Optie(128, 1, MaxHashMB, on_hash_size);
  o["Clear Hash"]            << Optie(on_clear_hash);
  o["Threads"]               << Optie(1, 1, MAX_THREADS, on_threads);
  o["Ponder"]                << Optie(false);
  o["Contempt"]              << Optie(0, 0, 10);
  o["Analysis Contempt"]     << Optie(false);
  o["MultiPV"]               << Optie(1, 1, 220);
  o["MultiPV_cp"]			 << Optie(0, 0, 999);
  o["SyzygyPath"]            << Optie("<empty>", on_syzygy_path);
  o["EGTB Probe Depth"]      << Optie(5, -10, 99);
  o["EGTB Fifty Move Rule"]  << Optie(true);
#ifdef NALIMOV
  o["NalimovPath"]			 << Optie("<empty>", on_nalimov_path);
  o["NalimovCache"]			 << Optie(32, 4, 1024, on_nalimov_cache);
#endif
#ifdef NUMA
  o["NUMA Offset"]           << Optie(0, 0, 63);
  o["NUMA Enabled"]			 << Optie(true);
#endif
  o["Strength"]              << Optie(100, 0, 100, on_strength);
  o["UCI_LimitStrength"]     << Optie(false);
  o["UCI_Elo"]				 << Optie(3200, 1400, 3200, on_uci_elo);

  o["UCI_Chess960"]			 << Optie(false);
  o["Never Clear Hash"]      << Optie(false);
  o["Hash File"]             << Optie("<empty>");
  o["Save Hash to File"]     << Optie(on_save_hash_to_file);
  o["Load Hash from File"]   << Optie(on_load_hash_from_file);
#ifdef LEARNING
  o["Learning File"]         << Optie("<empty>");
  o["Learning Threshold"]    << Optie(10, -100, 200);
  o["Learning"]				 << Optie(false);
#endif
  o["FiftyMoveDistance"]	 << Optie(50, 5, 50, on_50move_distance);

  o["Logfile"]        << Optie("", on_logger);
  //o["Tune1"] << Optie(256, -99999, 99999, on_tune1);
  //o["Tune2"] << Optie(256, -99999, 99999, on_tune2);
  //o["Tune3"] << Optie(256, -99999, 99999, on_tune3);
  //o["Tune4"] << Optie(256, -99999, 99999, on_tune4);
  //o["Tune5"] << Optie(256, -99999, 99999, on_tune5);
  //o["Tune6"] << Optie(256, -99999, 99999, on_tune6);
  //o["Tune7"] << Optie(256, -99999, 99999, on_tune7);
  //o["Tune8"] << Optie(256, -99999, 99999, on_tune8);
  //o["Tune9"] << Optie(256, -99999, 99999, on_tune9);
  //o["Tune10"] << Optie(256, -99999, 99999, on_tune10);
  //o["Tune11"] << Optie(256, -99999, 99999, on_tune11);
  //o["Tune12"] << Optie(256, -99999, 99999, on_tune12);
  //o["Tune13"] << Optie(256, -99999, 99999, on_tune13);
  //o["Tune14"] << Optie(256, -99999, 99999, on_tune14);
  //o["Tune15"] << Optie(256, -99999, 99999, on_tune15);
  //o["Tune16"] << Optie(256, -99999, 99999, on_tune16);
  //o["Tune17"] << Optie(256, -99999, 99999, on_tune17);
  //o["Tune18"] << Optie(256, -99999, 99999, on_tune18);
  //o["Tune19"] << Optie(256, -99999, 99999, on_tune19);
  //o["Tune20"] << Optie(256, -99999, 99999, on_tune20);
  TUNE_1 = TUNE_2 = TUNE_3 = TUNE_4 = TUNE_5 = TUNE_6 = TUNE_7 = TUNE_8 = TUNE_9 = TUNE_10 = 256;
  TUNE_11 = TUNE_12 = TUNE_13 = TUNE_14 = TUNE_15 = TUNE_16 = TUNE_17 = TUNE_18 = TUNE_19 = TUNE_20 = 256;
}


/// operator<<() is used to print all the options default values in chronological
/// insertion order (the idx field) and in the format defined by the UCI protocol.

std::ostream& operator<<(std::ostream& os, const OptionsMap& om) {

  for (size_t idx = 0; idx < om.size(); ++idx)
      for (const auto& it : om)
          if (it.second.idx == idx)
          {
              const Optie& o = it.second;
              os << "option name " << it.first << " type " << o.type;

			  if (o.type == "spin")
				  os << " min " << o.min << " max " << o.max;

			  if (o.type != "button")
                  os << " default " << o.defaultValue;

			  os << std::endl;
              break;
          }

  return os;
}


/// Optie class constructors and conversion operators

Optie::Optie(const char* v, OnChange f) : type("string"), min(0), max(0), on_change(f)
{ defaultValue = currentValue = v; }

Optie::Optie(bool v, OnChange f) : type("check"), min(0), max(0), on_change(f)
{ defaultValue = currentValue = (v ? "true" : "false"); }

Optie::Optie(OnChange f) : type("button"), min(0), max(0), on_change(f)
{}

Optie::Optie(int v, int minv, int maxv, OnChange f) : type("spin"), min(minv), max(maxv), on_change(f)
{ defaultValue = currentValue = std::to_string(v); }

Optie::operator int() const {
  assert(type == "check" || type == "spin");
  return (type == "spin" ? stoi(currentValue) : currentValue == "true");
}

Optie::operator std::string() const {
  assert(type == "string");
  return currentValue;
}


/// operator<<() inits options and assigns idx in the correct printing order

void Optie::operator<<(const Optie& o) {

  static size_t insert_order = 0;

  *this = o;
  idx = insert_order++;
}


/// operator=() updates currentValue and triggers on_change() action. It's up to
/// the GUI to check for option's limits, but we could receive the new value from
/// the user by console window, so let's check the bounds anyway.

Optie& Optie::operator=(const string& v) {

  assert(!type.empty());

  if (   (type != "button" && v.empty())
      || (type == "check" && v != "true" && v != "false")
      || (type == "spin" && (stoi(v) < min || stoi(v) > max)))
      return *this;

  if (type != "button")
      currentValue = v;

  if (on_change)
      on_change(*this);

  return *this;
}

} // namespace UCI
