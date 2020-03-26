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

#ifndef PAWNS_H_INCLUDED
#define PAWNS_H_INCLUDED

#include "misc.h"
#include "position.h"
#include "types.h"

namespace Pionnen {

/// Pawns::Entry contains various information about a pawn structure. A lookup
/// to the pawn hash table (performed by calling the probe function) returns a
/// pointer to an Entry object.

struct Tuple {

  Score pionnen_score() const { return score; }
  Bitboard pion_aanval(Kleur c) const { return pionAanval[c]; }
  Bitboard vrijpionnen(Kleur c) const { return vrijPionnen[c]; }
  Bitboard pion_aanval_bereik(Kleur c) const { return pionAanvalBereik[c]; }
  int pion_bereik(Kleur c) const { return pionBereik[c]; }
  int pion_asymmetrie() const { return asymmetrie; }

  int half_open_lijn(Kleur c, Lijn f) const {
    return halfOpenLijnen[c] & (1 << f);
  }

  int semiopen_side(Kleur c, Lijn f, bool leftSide) const {
    return halfOpenLijnen[c] & (leftSide ? (1 << f) - 1 : ~((1 << (f + 1)) - 1));
  }

  int pionnen_op_kleur(Kleur c, Veld s) const {
    return pionnenOpKleur[c][!!(DonkereVelden & s)];
  }

  int pionnen_niet_op_kleur(Kleur c, Veld s) const {
	  return pionnenOpKleur[c][!(DonkereVelden & s)];
  }

  template<Kleur IK>
  Score koning_veiligheid(const Stelling& pos) {
    if (koningVeld[IK] != pos.veld<KONING>(IK) || rokadeMogelijkheden[IK] != pos.rokade_mogelijkheden(IK))
		koningVeiligheid[IK] = bereken_koning_veiligheid<IK>(pos);
	return koningVeiligheid[IK];
  }

  template<Kleur IK>
  Score bereken_koning_veiligheid(const Stelling& pos);

  Sleutel64 key;
  Bitboard vrijPionnen[KLEUR_N];
  Bitboard pionAanval[KLEUR_N];
  Bitboard pionAanvalBereik[KLEUR_N];
  Score score;
  Score koningVeiligheid[KLEUR_N];
  uint8_t koningVeld[KLEUR_N];
  uint8_t rokadeMogelijkheden[KLEUR_N];
  uint8_t halfOpenLijnen[KLEUR_N];
  uint8_t pionBereik[KLEUR_N];
  int asymmetrie;
  int pionnenOpKleur[KLEUR_N][KLEUR_N]; // [color][light/dark squares]
  int gemiddeldeLijn, nPionnen;
  bool conversie_moeilijk;
  int safety[KLEUR_N];
  int lijn_breedte;
  char padding[8]; // Align to 128 bytes
};
static_assert(offsetof(struct Tuple, halfOpenLijnen) == 72, "offset wrong");
static_assert(sizeof(Tuple) == 128, "Pawn Entry size incorrect");

typedef HashTable<Tuple, PION_HASH_GROOTTE> Tabel;

void init();
Tuple* probe(const Stelling& pos);

} // namespace Pawns

#endif // #ifndef PAWNS_H_INCLUDED
