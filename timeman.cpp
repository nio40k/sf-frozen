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
#include <cfloat>
#include <cmath>

#include "search.h"
#include "timeman.h"
#include "uci.h"

Tijdsbeheer Tijdscontrole; // Our global time management object

namespace {

  enum TimeType { OptimumTime, MaxTime };

  const int MoveHorizon   = 50;   // Plan time management at most this many moves ahead
  const double MaxRatio   = 7.09; // When in trouble, we can step over reserved time with this ratio
  const double StealRatio = 0.35; // However we must not steal time from remaining moves over this ratio


  // move_importance() is a skew-logistic function based on naive statistical
  // analysis of "how many games are still undecided after n half-moves". Game
  // is considered "undecided" as long as neither side has >275cp advantage.
  // Data was extracted from the CCRL game database with some simple filtering criteria.

  double move_importance(int ply) {

    const double XScale = 7.64;
    const double XShift = 58.4;
    const double Skew   = 0.183;

    return pow((1 + exp((ply - XShift) / XScale)), -Skew) + DBL_MIN; // Ensure non-zero
  }

  template<TimeType T>
  int64_t remaining(int64_t myTime, int movesToGo, int ply, int slowMover)
  {
    const double TMaxRatio   = (T == OptimumTime ? 1 : MaxRatio);
    const double TStealRatio = (T == OptimumTime ? 0 : StealRatio);

    double moveImportance = (move_importance(ply) * slowMover) / 100;
    double otherMovesImportance = 0;

    for (int i = 1; i < movesToGo; ++i)
        otherMovesImportance += move_importance(ply + 2 * i);

    double ratio1 = (TMaxRatio * moveImportance) / (TMaxRatio * moveImportance + otherMovesImportance);
    double ratio2 = (moveImportance + TStealRatio * otherMovesImportance) / (moveImportance + otherMovesImportance);

    return int64_t(myTime * std::min(ratio1, ratio2)); // Intel C++ asks an explicit cast
  }

} // namespace


/// init() is called at the beginning of the search and calculates the allowed
/// thinking time out of the time control and current game ply. We support four
/// different kinds of time controls, passed in 'limits':
///
///  inc == 0 && movestogo == 0 means: x basetime  [sudden death!]
///  inc == 0 && movestogo != 0 means: x moves in y minutes
///  inc >  0 && movestogo == 0 means: x basetime + z increment
///  inc >  0 && movestogo != 0 means: x moves in y minutes + z increment

void Tijdsbeheer::init(Search::TijdLimiet& limits, Kleur us, int ply, PartijFase phase)
{
#if 1
  int minThinkingTime = 20;  // Options["Minimum Thinking Time"];
  int moveOverhead    = 30;  // Options["Move Overhead"];
  int slowMover       = 89;  // Options["Slow Mover"];

  startTijd = limits.startTijd;
  optimaleTijd = maximumTijd = std::max(limits.time[us], minThinkingTime);

  const int MaxMTG = limits.movestogo ? std::min(limits.movestogo, MoveHorizon) : MoveHorizon;

  // We calculate optimum time usage for different hypothetical "moves to go"-values
  // and choose the minimum of calculated search time values. Usually the greatest
  // hypMTG gives the minimum values.
  for (int hypMTG = 1; hypMTG <= MaxMTG; ++hypMTG)
  {
      // Calculate thinking time for hypothetical "moves to go"-value
	  int64_t hypMyTime =  limits.time[us]
                     + limits.inc[us] * (hypMTG - 1)
                     - moveOverhead * (2 + std::min(hypMTG, 40));

      hypMyTime = std::max(hypMyTime, (int64_t)0);

	  int64_t t1 = minThinkingTime + remaining<OptimumTime>(hypMyTime, hypMTG, ply, slowMover);
	  int64_t t2 = minThinkingTime + remaining<MaxTime    >(hypMyTime, hypMTG, ply, slowMover);

      optimaleTijd = std::min(t1, optimaleTijd);
      maximumTijd = std::min(t2, maximumTijd);
  }

  if (Options["Ponder"])
	  optimaleTijd += optimaleTijd / 4;
#else
	startTijd = limits.startTijd;

	int tijd_voor_speler = limits.time[us];
	int tijd_increment = limits.inc[us];
	const int minThinkingTime = 20;
	const int moveOverhead = 30;

	tijd_voor_speler = std::max(tijd_voor_speler - 250, 9 * tijd_voor_speler / 10);
	if (tijd_voor_speler < 0)
		tijd_voor_speler = 0;

	//int n_max = limits.movestogo ? std::min(limits.movestogo, 60) : 60;
	//int totale_tijd = tijd_voor_speler + (n_max - 1) * tijd_increment;
	//optimaleTijd = (280 + n_max * std::min(ply, 80) / 20) * totale_tijd / n_max / 256;

	int n_max = limits.movestogo ? std::min(limits.movestogo, 60) : 60;
	int totale_tijd = std::max(tijd_voor_speler + (n_max - 1) * (tijd_increment - moveOverhead), 0);
	optimaleTijd = minThinkingTime + (TUNE_1 + n_max * std::min(ply, 80) * TUNE_2 / 256 / 20) * totale_tijd / n_max / 256;

	maximumTijd = std::min(6 * optimaleTijd, (int64_t)totale_tijd * (n_max + 2) / (3 * n_max));

	if (Options["Ponder"])
		optimaleTijd += optimaleTijd / 4;

	if (maximumTijd > tijd_voor_speler)
		maximumTijd = tijd_voor_speler;
	if (optimaleTijd > maximumTijd)
		optimaleTijd = maximumTijd;

#endif
}

int64_t Tijdsbeheer::verstreken() const
{
	return int64_t(now() - startTijd);
}

void Tijdsbeheer::aanpassing_na_ponderhit()
{
	int64_t nieuweMaxTijd = maximumTijd + verstreken();
	optimaleTijd = optimaleTijd * nieuweMaxTijd / maximumTijd;
}
