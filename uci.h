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

#ifndef UCI_H_INCLUDED
#define UCI_H_INCLUDED

#include <map>
#include <string>

#include "types.h"

class Stelling;

namespace UCI {

class Optie;

/// Custom comparator because UCI options should be case insensitive
struct CaseInsensitiveLess {
  bool operator() (const std::string&, const std::string&) const;
};

/// Our options container is actually a std::map
typedef std::map<std::string, Optie, CaseInsensitiveLess> OptionsMap;

/// Optie class implements an option as defined by UCI protocol
class Optie {

  typedef void (*OnChange)(const Optie&);

public:
  Optie(OnChange = nullptr);
  Optie(bool v, OnChange = nullptr);
  Optie(const char* v, OnChange = nullptr);
  Optie(int v, int min, int max, OnChange = nullptr);

  Optie& operator=(const std::string&);
  void operator<<(const Optie&);
  operator int() const;
  operator std::string() const;

private:
  friend std::ostream& operator<<(std::ostream&, const OptionsMap&);

  int min, max;
  size_t idx;
  std::string defaultValue, currentValue, type;
  OnChange on_change;
};

void init(OptionsMap&);
void loop(int argc, char* argv[]);
std::string waarde(Waarde v);
std::string veld(Veld veld);
std::string zet(Zet zet, const Stelling& pos);
std::string pv(const Stelling& pos, Waarde alfa, Waarde beta);
Zet zet_van_string(const Stelling& pos, std::string& str);
int sterkte_voor_elo(int elo);

} // namespace UCI

extern UCI::OptionsMap Options;
extern int TUNE_1, TUNE_2, TUNE_3, TUNE_4, TUNE_5, TUNE_6, TUNE_7, TUNE_8, TUNE_9, TUNE_10;
extern int TUNE_11, TUNE_12, TUNE_13, TUNE_14, TUNE_15, TUNE_16, TUNE_17, TUNE_18, TUNE_19, TUNE_20;

#endif // #ifndef UCI_H_INCLUDED
