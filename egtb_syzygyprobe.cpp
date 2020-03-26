/*
8/8/8/5k2/4b3/3K4/2B5/N1B5 w - - 0 1
8/3r4/1P6/5k2/3Kb3/1R1B4/8/8 w - - 0 1
8/1R6/6p1/r6k/3P4/2KP4/8/8 w - - 0 1
*/

#include <iostream>
#include <algorithm>

#include "position.h"
#include "search.h"
#include "syzygy\tbprobe.h"


static bool SyzygyInUse = false;
Waarde Syzygy_probe_wdl(Stelling& pos, Waarde alfa, Waarde beta);
Waarde Syzygy_probe_dtz(Stelling& pos, Waarde alfa, Waarde beta);


void Syzygy_init(const std::string& path)
{
	bool PathSpecified = (path != "") && (path != "<empty>");
	if (PathSpecified)
	{
		syzygy_path_init(path);
		Tablebases::MaxPieces_wdl = Tablebases::MaxPieces_dtz = TBmax_men;
		if (Tablebases::MaxPieces_wdl)
		{
			SyzygyInUse = true;
			Tablebases::EGTB_probe_wdl = &Syzygy_probe_wdl;
			Tablebases::EGTB_probe_dtz = &Syzygy_probe_dtz;
			sync_cout << "info string Syzygy " << Tablebases::MaxPieces_wdl << " men EGTB available - " << TBnum_piece + TBnum_pawn  << " tablebases found" << sync_endl;
			return;
		}
	}
	if (PathSpecified || SyzygyInUse)
	{
		SyzygyInUse = false;
		Tablebases::MaxPieces_wdl = Tablebases::MaxPieces_dtz = 0;
		Tablebases::EGTB_probe_wdl = nullptr;
		Tablebases::EGTB_probe_dtz = nullptr;
		sync_cout << "info string Syzygy EGTB not available" << sync_endl;
	}
}

Waarde Syzygy_probe_wdl(Stelling& pos, Waarde alfa, Waarde beta)
{
	int success;
	int v;

	v = syzygy_probe_wdl(pos, &success);

	if (success == 0)
		return GEEN_WAARDE;

	pos.verhoog_tb_hits();

	int drawScore = Tablebases::UseRule50 ? 1 : 0;
	if (v > drawScore)
		return Waarde(LANGSTE_MAT_WAARDE - 1);
	else if (v < -drawScore)
		return Waarde(-LANGSTE_MAT_WAARDE + 1);
	else
		return REMISE_WAARDE;
}

Waarde Syzygy_probe_dtz(Stelling& pos, Waarde alfa, Waarde beta)
{
	int success;
	int v;

	v = syzygy_probe_dtz(pos, &success);

	if (success == 0)     // DTZ file niet beschikbaar, we returnen de wdl probe
		return Syzygy_probe_wdl(pos, alfa, beta);

	pos.verhoog_tb_hits();

	int move50 = pos.vijftig_zetten_teller();

	if (v > 0 && (!Tablebases::UseRule50 || move50 + v <= 99))  // gewonnen
		return Waarde(LANGSTE_MAT_WAARDE - (move50 + v));
	else if (v < 0 && (!Tablebases::UseRule50 || move50 - v <= 99))  // verloren
		return Waarde(-LANGSTE_MAT_WAARDE + (move50 - v));
	else // remise, of meer dan 50 zetten nodig om te winnen of te verliezen
		return REMISE_WAARDE;
}
