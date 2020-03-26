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

#ifndef TT_H_INCLUDED
#define TT_H_INCLUDED

#include "misc.h"
#include "types.h"
#include <algorithm>

using std::string;

enum WaardeLimiet {
	GEEN_LIMIET,
	BOVENGRENS,
	ONDERGRENS,
	EXACTE_WAARDE = BOVENGRENS | ONDERGRENS
};


struct TTTuple {

  Zet zet() const { return (Zet) zet16; }
  Waarde waarde() const { return (Waarde)waarde16; }
  Waarde eval() const { return (Waarde)eval16; }
  Diepte diepte() const { return Diepte(diepte8 * int(PLY) + PLY - 1); }
  WaardeLimiet limiet() const { return (WaardeLimiet)(vlaggen8 & 0x03); }
  bool bit3() const { return vlaggen8 & 0x04; }

  void bewaar(Sleutel64 k, Waarde v, uint8_t vlaggen, Diepte d, Zet zet, Waarde ev, uint8_t gen) {

    int dd = d / PLY;
    uint16_t k16 = k >> 48;  // Use the high 16 bits as key inside the cluster
	//k16 = std::max(k16, (uint16_t)1);

    // Preserve any existing move for the same position
    if (zet || k16 != sleutel16)
        zet16 = (uint16_t)zet;

    // Don't overwrite more valuable entries
    if (  k16 != sleutel16
        || dd > diepte8 - 4
     /* || g != (genLimiet8 & 0xFC) // Matching non-zero keys are already refreshed by probe() */
        || (vlaggen & 0x03) == EXACTE_WAARDE)
    {
        sleutel16  = k16;
        waarde16   = (int16_t)v;
        eval16     = (int16_t)ev;
        vlaggen8   = (uint8_t)(gen + vlaggen);
        diepte8    = (int8_t)dd;
    }
  }

private:
  friend class TranspositieTabel;

  // 10 bytes
  uint16_t sleutel16;
  uint16_t zet16;
  int16_t  waarde16;
  int16_t  eval16;
  uint8_t  vlaggen8;
  int8_t   diepte8;
};


/// A TranspositieTabel consists of a power of 2 number of clusters and each
/// cluster consists of ClusterGrootte number of TTTuple. Each non-empty entry
/// contains information of exactly one position. The size of a cluster should
/// divide the size of a cache line size, to ensure that clusters never cross
/// cache lines. This ensures best cache performance, as the cacheline is
/// prefetched, as soon as possible.

class TranspositieTabel {

  static const int CacheLineSize = 64;
  static const int ClusterGrootte = 3;

  struct Cluster {
    TTTuple entry[ClusterGrootte];
    char padding[2]; // Align to a divisor of the cache line size
  };

  static_assert(CacheLineSize % sizeof(Cluster) == 0, "Cluster size incorrect");

public:
 ~TranspositieTabel() { _aligned_free(tabel); }
  void nieuwe_generatie() { generatie8 += 8; } // Lower 2 bits are used by Bound
  uint8_t generatie() const { return generatie8; }
  TTTuple* zoek(const Sleutel64 sleutel, bool& gevonden) const;
  int bereken_hashfull() const;
  void verander_grootte(size_t mbSize);
  void wis();

  // The lowest order bits of the key are used to get the index of the cluster
  inline TTTuple* tuple(const Sleutel64 key) const {
    //return &tabel[(size_t)key & (clusterAantal - 1)].entry[0];
    return (TTTuple*)((char *)tabel + (key & clusterMask));
  }
  inline void prefetch_tuple(const Sleutel64 sleutel) const {
	  prefetch(tuple(sleutel));
  }
  bool save_to_file(string uci_hash_file);
  bool load_from_file(string uci_hash_file);

private:
  size_t clusterAantal;
  size_t clusterMask;
  Cluster* tabel;
  uint8_t generatie8;
};

extern TranspositieTabel TT;

#endif // #ifndef TT_H_INCLUDED
