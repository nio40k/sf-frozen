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

#include <cstring>   // For std::memset
#include <iostream>

#include "bitboard.h"
#include "tt.h"

TranspositieTabel TT; // Our global transposition table


/// TranspositieTabel::resize() sets the size of the transposition table,
/// measured in megabytes. Transposition table consists of a power of 2 number
/// of clusters and each cluster consists of ClusterGrootte number of TTTuple.

void TranspositieTabel::verander_grootte(size_t mbSize) {

  size_t nieuwClusterAantal = size_t(1) << msb((mbSize * 1024 * 1024) / sizeof(Cluster));

  if (nieuwClusterAantal == clusterAantal)
      return;

  _aligned_free(tabel);
  clusterAantal = 0;

  tabel = (Cluster*)_aligned_malloc(nieuwClusterAantal * sizeof(Cluster), 64);

  if (!tabel)
  {
      std::cerr << "ERROR: Unable to allocate the requested hash size " << mbSize << " MB" << std::endl;
	  // probeer opnieuw met 1 MB
	  nieuwClusterAantal = 32768;
	  tabel = (Cluster*)_aligned_malloc(nieuwClusterAantal * sizeof(Cluster), 64);
	  if (!tabel)
		  exit(EXIT_FAILURE);
  }
  sync_cout << "info string " << nieuwClusterAantal * sizeof(Cluster) / (1024 * 1024) << " MB Hash" << sync_endl;

  clusterAantal = nieuwClusterAantal;
  clusterMask = (clusterAantal - 1) * sizeof(Cluster);
}


/// TranspositieTabel::clear() overwrites the entire transposition table
/// with zeros. It is called whenever the table is resized, or when the
/// user asks the program to clear the table (from the UCI interface).

void TranspositieTabel::wis() {

  std::memset(tabel, 0, clusterAantal * sizeof(Cluster));
}

#if defined(_WIN32)
#define NOMINMAX // Disable macros min() and max()
#include <shlobj.h>
#undef NOMINMAX
string GetUserFolder()
{
	char szPath[MAX_PATH];
	SHGetFolderPath(NULL, CSIDL_PERSONAL, NULL, 0, szPath);
	return string(szPath);
}
#else
string GetUserFolder()
{
	return "/user/dat/";
}
#endif
static char hash_file_ID[17]  = "HOUDINI_HASH_500";
static char learn_file_ID[17] = "HOUDINI_LEARN500";

bool TranspositieTabel::save_to_file(string uci_hash_file) {

	string file_name;
	if (uci_hash_file == "" || uci_hash_file == "<empty>")
		file_name = GetUserFolder() + "\\hash.dat";
	else
		file_name = uci_hash_file;

	FILE *hashfile = fopen(uci_hash_file.c_str(), "wb");
	if (!hashfile)
		return false;

	sync_cout << "info string Saving Hash to File " << file_name << "..." << sync_endl;

	bool result = fwrite(hash_file_ID, 1, 16, hashfile) == 16;

	if (result)
	{
		uint64_t save_waarden[4];
		save_waarden[0] = clusterAantal;
		save_waarden[1] = generatie8;
		save_waarden[2] = 0;
		save_waarden[3] = 0;
		result = fwrite(&save_waarden[0], sizeof(save_waarden[0]), 4, hashfile) == 4;
	}

	uint64_t hash_grootte = clusterAantal;
	Cluster *pp = tabel;
	while (result && hash_grootte)
	{
		uint64_t blok_grootte = std::min(hash_grootte, uint64_t(32 * 1024 * 1024));
		result = fwrite(pp, sizeof(Cluster), blok_grootte, hashfile) == blok_grootte;
		hash_grootte -= blok_grootte;
		pp += blok_grootte;
	}

	fclose(hashfile);
	return result;
}

bool TranspositieTabel::load_from_file(string uci_hash_file) {

	char ID[17];
	uint64_t hash_grootte, blok_grootte;
	uint64_t save_waarden[4];

	string file_name;
	if (uci_hash_file == "" || uci_hash_file == "<empty>")
		file_name = GetUserFolder() + "\\hash.dat";
	else
		file_name = uci_hash_file;

	FILE *hashfile = fopen(uci_hash_file.c_str(), "rb");
	if (!hashfile)
		return false;

	sync_cout << "info string Loading Hash from File " << file_name << "..." << sync_endl;

	bool Result = fread(ID, 1, 16, hashfile) == 16;
	if (Result)
		Result = memcmp(ID, hash_file_ID, 16) == 0;

	if (Result)
		Result = fread(&save_waarden[0], sizeof(save_waarden[0]), 4, hashfile) == 4;

	if (Result)
		hash_grootte = save_waarden[0];

	if (Result && hash_grootte != clusterAantal)
	{
		size_t mb = (hash_grootte * sizeof(Cluster)) >> 20;
		verander_grootte(mb);
		Result = hash_grootte == clusterAantal;
	}

	if (Result)
		generatie8 = (uint8_t)save_waarden[1];

	Cluster *pp = tabel;
	while (Result && hash_grootte)
	{
		blok_grootte = std::min(hash_grootte, uint64_t(32 * 1024 * 1024));
		Result = fread(pp, sizeof(Cluster), blok_grootte, hashfile) == blok_grootte;
		hash_grootte -= blok_grootte;
		pp += blok_grootte;
	}

	fclose(hashfile);
	return Result;
}


/// TranspositieTabel::probe() looks up the current position in the transposition
/// table. It returns true and a pointer to the TTTuple if the position is found.
/// Otherwise, it returns false and a pointer to an empty or least valuable TTTuple
/// to be replaced later. The replace value of an entry is calculated as its depth
/// minus 8 times its relative age. TTTuple t1 is considered more valuable than
/// TTTuple t2 if its replace value is greater than that of t2.

TTTuple* TranspositieTabel::zoek(const Sleutel64 key, bool& gevonden) const {

  TTTuple* const ttt = tuple(key);
  uint16_t sleutel16 = key >> 48;  // Use the high 16 bits as key inside the cluster
  //sleutel16 = std::max(sleutel16, (uint16_t)1);

  for (int i = 0; i < ClusterGrootte; ++i)
	  if (!ttt[i].sleutel16)
	  {
		  gevonden = false;
		  return &ttt[i];
	  }
      else if (ttt[i].sleutel16 == sleutel16)
      {
          if ((ttt[i].vlaggen8 & 0xf8) != generatie8)
               ttt[i].vlaggen8 = uint8_t(generatie8 + (ttt[i].vlaggen8 & 0x07)); // Refresh

		  gevonden = true;
          return &ttt[i];
      }

  TTTuple* vervanging = ttt;
  for (int i = 1; i < ClusterGrootte; ++i)
      if (   vervanging->diepte8 - ((generatie8 - (vervanging->vlaggen8 & 0xf8)) & 0xf8)
           >      ttt[i].diepte8 - ((generatie8 - (ttt[i].vlaggen8 & 0xf8)) & 0xf8) )
          vervanging = &ttt[i];

  gevonden = false;
  return vervanging;
}


/// Returns an approximation of the hashtable occupation during a search. The
/// hash is x permill full, as per UCI protocol.

int TranspositieTabel::bereken_hashfull() const
{
  const int i_max = 999 / ClusterGrootte + 1;
  int cnt = 0;
  for (int i = 0; i < i_max; i++)
  {
      const TTTuple* ttt = &tabel[i].entry[0];
      for (int j = 0; j < ClusterGrootte; j++)
          if ((ttt[j].vlaggen8 & 0xf8) == generatie8)
              cnt++;
  }
  return cnt * 1000 / (i_max * ClusterGrootte);
}
