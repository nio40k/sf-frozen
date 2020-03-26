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

#ifndef MATERIAL_H_INCLUDED
#define MATERIAL_H_INCLUDED

#include "endgame.h"
#include "misc.h"
#include "position.h"
#include "types.h"

namespace Materiaal {

/// Material::Entry contains various information about a material configuration.
/// It contains a material imbalance evaluation, a function pointer to a special
/// endgame evaluation function (which in most cases is NULL, meaning that the
/// standard evaluation function will be used), and scale factors.
///
/// The scale factors are used to scale the evaluation score up or down. For
/// instance, in KRB vs KR endgames, the score is scaled down by a factor of 4,
/// which will result in scores of absolute value less than one pawn.

struct Tuple {

  PartijFase partij_fase() const { return PartijFase(gamePhase); }

  bool heeft_waarde_functie() const { return waardeFunctieIndex >= 0; }
  Waarde waarde_uit_functie(const Stelling& pos) const;

  SchaalFactor schaalfactor_uit_functie(const Stelling& pos, Kleur c) const;

  Sleutel64 key;
  int waardeFunctieIndex;
  int schaalFunctieIndex[KLEUR_N]; // Could be one for each side (e.g. KPKP, KBPsKs)
  EvalWaarde waarde;
  SchaalFactor conversie;
  uint8_t factor[KLEUR_N], gamePhase;
  bool conversie_is_geschat;
  //char padding[8]; // Align to 32 bytes
};
static_assert(sizeof(Tuple) == 32, "Material Entry size incorrect");

typedef HashTable<Tuple, MATERIAAL_HASH_GROOTTE> Tabel;

Tuple* probe(const Stelling& pos);

} // namespace Material

#endif // #ifndef MATERIAL_H_INCLUDED
