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

#include <iostream>
#include <sstream>
#include <string>

#include "evaluate.h"
#include "movegen.h"
#include "position.h"
#include "search.h"
#include "thread.h"
#include "timeman.h"
#include "uci.h"

using namespace std;

extern void benchmark(const Stelling& pos, istream& is);

namespace {

  // FEN string of the initial position, normal chess
  const char* StartFEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";

  // position() is called when engine receives the "position" UCI command.
  // The function sets up the position described in the given FEN string ("fen")
  // or the starting position ("startpos") and then makes the moves given in the
  // following move list ("moves").

  void position(Stelling& pos, istringstream& is) {

    Zet zet;
    string token, fen;

    is >> token;

    if (token == "startpos")
    {
        fen = StartFEN;
        is >> token; // Consume "moves" token if any
    }
    else if (token == "fen")
        while (is >> token && token != "moves")
            fen += token + " ";
    else
        return;

    pos.set(fen, Options["UCI_Chess960"], Threads.main());
	string validation = pos.valideer_stelling();
	if (validation != "")
	{
		sync_cout << "Invalid position: " << validation << endl << "Reverting to starting position" << sync_endl;
		pos.set(StartFEN, Options["UCI_Chess960"], Threads.main());
	}

    // Parse move list (if any)
    while (is >> token && (zet = UCI::zet_van_string(pos, token)) != GEEN_ZET)
    {
		pos.speel_zet(zet);
		pos.verhoog_partij_ply();
    }
  }


  // setoption() is called when engine receives the "setoption" UCI command. The
  // function updates the UCI option ("name") to the given value ("value").

  void setoption(istringstream& is) {

    string token, name, value;

    is >> token; // Consume "name" token

    // Read option name (can contain spaces)
    while (is >> token && token != "value")
        name += string(" ", name.empty() ? 0 : 1) + token;

    // Read option value (can contain spaces)
    while (is >> token)
        value += string(" ", value.empty() ? 0 : 1) + token;

    if (Options.count(name))
        Options[name] = value;
    else
        sync_cout << "info string Unknown UCI option \"" << name << "\"" << sync_endl;
  }

  void setoption2(istringstream& is) {

	  string name, value;

	  is >> name;
	  is >> value;

	  if (Options.count(name))
		  Options[name] = value;
	  else
		  sync_cout << "info string Unknown UCI option \"" << name << "\"" << sync_endl;
  }


  // go() is called when engine receives the "go" UCI command. The function sets
  // the thinking time and other parameters from the input string, then starts
  // the search.

  void go(const Stelling& pos, istringstream& is) {

    Search::TijdLimiet tijdLimiet;
    string woord;

    tijdLimiet.startTijd = now(); // As early as possible!

    while (is >> woord)
        if (woord == "searchmoves")
            while (is >> woord)
                tijdLimiet.zoekZetten.add(UCI::zet_van_string(pos, woord));

        else if (woord == "wtime")     is >> tijdLimiet.time[WIT];
        else if (woord == "btime")     is >> tijdLimiet.time[ZWART];
        else if (woord == "winc")      is >> tijdLimiet.inc[WIT];
        else if (woord == "binc")      is >> tijdLimiet.inc[ZWART];
        else if (woord == "movestogo") is >> tijdLimiet.movestogo;
        else if (woord == "depth")     is >> tijdLimiet.depth;
        else if (woord == "nodes")     is >> tijdLimiet.nodes;
        else if (woord == "movetime")  is >> tijdLimiet.movetime;
        else if (woord == "mate")      is >> tijdLimiet.mate;
        else if (woord == "infinite")  tijdLimiet.infinite = 1;
        else if (woord == "ponder")    tijdLimiet.ponder = 1;

	// go = go infinite
	if (!tijdLimiet.time[pos.aan_zet()] && !tijdLimiet.movestogo && !tijdLimiet.depth && !tijdLimiet.nodes 
		&& !tijdLimiet.movetime && !tijdLimiet.mate)
		tijdLimiet.infinite = 1;

	// for Arena we accept pondering even if the UCI PONDER is not activated
	if (tijdLimiet.ponder && !Options["Ponder"])
		Options["Ponder"] = string("true");

    Threads.begin_zoek_opdracht(pos, tijdLimiet);
  }

} // namespace

std::string trim(const std::string& str,
	const std::string& whitespace = " \t")
{
	const auto strBegin = str.find_first_not_of(whitespace);
	if (strBegin == std::string::npos)
		return ""; // no content

	const auto strEnd = str.find_last_not_of(whitespace);
	const auto strRange = strEnd - strBegin + 1;

	return str.substr(strBegin, strRange);
}

/// UCI::loop() waits for a command from stdin, parses it and calls the appropriate
/// function. Also intercepts EOF from stdin to ensure gracefully exiting if the
/// GUI dies unexpectedly. When called with some command line arguments, e.g. to
/// run 'bench', once the command is executed the function returns immediately.
/// In addition to the UCI ones, also some additional debug commands are supported.

void UCI::loop(int argc, char* argv[]) {

  Stelling pos;
  string token, cmd;

  pos.set(StartFEN, false, Threads.main());
  Search::clear();

#ifdef TRACE_TM
  open_tracefile();
#endif

  for (int i = 1; i < argc; ++i)
      cmd += std::string(argv[i]) + " ";

  do {
      if (argc == 1 && !getline(cin, cmd)) // Block here waiting for input or EOF
          cmd = "quit";
	  cmd = trim(cmd);
	  if (cmd == "")
		  continue;

      istringstream is(cmd);

      token.clear(); // getline() could return empty or blank line
      is >> skipws >> token;

      // The GUI sends 'ponderhit' to tell us to ponder on the same move the
      // opponent has played. In case Signals.stopOnPonderhit is set we are
      // waiting for 'ponderhit' to stop the search (for instance because we
      // already ran out of time), otherwise we should continue searching but
      // switching from pondering to normal search.
      if (    token == "quit"
          ||  token == "stop"
          || (token == "ponderhit" && Search::Signals.stopOnPonderhit))
      {
          Search::Signals.stop = true;
          Threads.main()->start_zoeken(true); // Could be sleeping
		  Threads.main()->wait_for_search_finished();
      }
	  else if (token == "ponderhit")
	  {
		  Search::Limits.ponder = 0; // Switch to normal search
		  Search::pas_tijd_aan_na_ponderhit();
	  }

      else if (token == "uci")
          sync_cout << "id name " << engine_info(true)
                    << Options
                    << "uciok"  << sync_endl;

      else if (token == "ucinewgame")
      {
		  Search::Signals.stop = true;
		  Threads.main()->start_zoeken(true); // Could be sleeping
		  Threads.main()->wait_for_search_finished();
		  Search::clear();
      }
	  else if (token == "isready")
		  sync_cout << "readyok" << sync_endl;
	  else if (Search::Running)
		  sync_cout << "info string Search running, command ignored" << sync_endl;
	  else
	  {
		  if (token == "go")         go(pos, is);
		  else if (token == "position")   position(pos, is);
		  else if (token == "setoption")  setoption(is);
		  else if (token == "set")  setoption2(is);

		  // Additional custom non-UCI commands, useful for debugging
		  //else if (token == "flip")       pos.flip();
		  else if (token == "bench")      benchmark(pos, is);
		  else if (token == "b")          benchmark(pos, is);
		  //else if (token == "d")          sync_cout << pos << sync_endl;
		  else if (token == "e")          sync_cout << Eval::trace(pos) << sync_endl;
		  else
			  sync_cout << "info string Unknown UCI command \"" << cmd << "\"" << sync_endl;
	  }

  } while (token != "quit" && argc == 1); // Passed args have one-shot behaviour

  Threads.main()->wait_for_search_finished();

#ifdef TRACE_TM
  close_tracefile();
#endif
}


/// UCI::value() converts a Value to a string suitable for use with the UCI
/// protocol specification:
///
/// cp <x>    The score from the engine's point of view in centipawns.
/// mate <y>  Mate in y moves, not plies. If the engine is getting mated
///           use negative values for y.

string UCI::waarde(Waarde v) {

  stringstream ss;

  if (abs(v) < LANGSTE_MAT_WAARDE)
  {
	  int vv;
	  if (abs(v) >= maak_waarde(500))
		  vv = 16 * v;
	  else if (v <= maak_waarde(-250))
		  vv = 14 * v - maak_waarde(1000);
	  else if (v <= maak_waarde(-125))
		  vv = 16 * v - maak_waarde(500);
	  else if (v <= maak_waarde(125))
		  vv = 20 * v;
	  else if (v <= maak_waarde(250))
		  vv = 16 * v + maak_waarde(500);
	  else
		  vv = 14 * v + maak_waarde(1000);

	  ss << "cp " << vv * 100 / (16 * WAARDE_PION);
  }
  else
	  ss << "mate " << (v > 0 ? MAT_WAARDE - v + 1 : -MAT_WAARDE - v) / 2;

  return ss.str();
}


/// UCI::square() converts a Veld to a string in algebraic notation (g1, a7, etc.)

std::string UCI::veld(Veld s) {
  return std::string{ char('a' + lijn(s)), char('1' + rij(s)) };
}


/// UCI::move() converts a Zet to a string in coordinate notation (g1f3, a7a8q).
/// The only special case is castling, where we print in the e1g1 notation in
/// normal chess mode, and in e1h1 notation in chess960 mode.

string UCI::zet(Zet zet, const Stelling& pos) {

  char sZet[6];

  Veld van = van_veld(zet);
  Veld naar = naar_veld(zet);

  if (zet == GEEN_ZET || zet == NULL_ZET)
      return "0000";

  if (zet_type(zet) == ROKADE && pos.is_chess960())
      naar = pos.rokade_toren_veld(naar);

  //string sZet = UCI::veld(from) + UCI::veld(to);
  sZet[0] = 'a' + lijn(van);
  sZet[1] = '1' + rij(van);
  sZet[2] = 'a' + lijn(naar);
  sZet[3] = '1' + rij(naar);
  if (zet_type(zet) != PROMOTIE)
	  return string(sZet, 4);

  sZet[4] = "   nbrq"[promotie_stuk(zet)];
  return string(sZet, 5);
}


/// UCI::to_move() converts a string representing a move in coordinate notation
/// (g1f3, a7a8q) to the corresponding legal Zet, if any.

Zet UCI::zet_van_string(const Stelling& pos, string& str) {

  if (str.length() == 5) // Junior could send promotion piece in uppercase
      str[4] = char(tolower(str[4]));

  for (const auto& zet : ZettenLijst<GEN_LEGALE_ZETTEN>(pos))
      if (str == UCI::zet(zet, pos))
          return zet;

  return GEEN_ZET;
}
