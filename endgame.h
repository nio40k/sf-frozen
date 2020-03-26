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

#ifndef ENDGAME_H_INCLUDED
#define ENDGAME_H_INCLUDED

#include <map>

#include "position.h"
#include "types.h"

typedef Waarde (*eindspel_waarde_fx)(const Stelling& pos);
typedef SchaalFactor (*eindspel_schaalfactor_fx)(const Stelling& pos);

class Eindspelen {

  void add_waarde(char* pieces, eindspel_waarde_fx, eindspel_waarde_fx);
  void add_schaalfactor(char* pieces, eindspel_schaalfactor_fx, eindspel_schaalfactor_fx);

  typedef std::map<Sleutel64, int> FunctieIndexMap;

  int waarde_aantal, factor_aantal;
  FunctieIndexMap mapWaarde;
  FunctieIndexMap mapSchaalFactor;
public:
  Eindspelen();
  void init();

  int probe_waarde(Sleutel64 key);
  int probe_schaalfactor(Sleutel64 key, Kleur& sterkeZijde);

  eindspel_waarde_fx waarde_functies[16];
  eindspel_schaalfactor_fx factor_functies[32];
};

template <Kleur sterkeZijde> Waarde endgameValue_KXK(const Stelling& pos);
template <Kleur sterkeZijde> SchaalFactor endgameScaleFactor_KBPsK(const Stelling& pos);
template <Kleur sterkeZijde> SchaalFactor endgameScaleFactor_KQKRPs(const Stelling& pos);
template <Kleur sterkeZijde> SchaalFactor endgameScaleFactor_KPsK(const Stelling& pos);
template <Kleur sterkeZijde> SchaalFactor endgameScaleFactor_KPKP(const Stelling& pos);


#endif // #ifndef ENDGAME_H_INCLUDED
