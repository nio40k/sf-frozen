#include <iostream>
#include <algorithm>

#include "position.h"
#include "search.h"
#include "egtb_nalimovprobe.h"

typedef uint64_t INDEX;
typedef uint8_t veld;
typedef uint8_t color;
typedef uint8_t piece;

#define EP_NO_SQUARE 127

#define x_colorWhite 0
#define x_colorBlack 1

#define L_pageL 65536
#define L_tbbe_ssL   ((L_pageL - 4) / 2)
#define L_bev_broken (L_tbbe_ssL + 1)     // illegal or busted
#define L_bev_mi1    L_tbbe_ssL           // mate in 1 zet

typedef INDEX (*PfnCalcIndex) (veld*, veld*, veld, int Inverse);

extern int IInitializeTb(char *);
extern int FTbSetCacheSize(void *, unsigned long);
extern int IDescFindFromCounters (int*);
extern int FRegisteredFun (int, color);
extern PfnCalcIndex PfnIndCalcFun (int, color);
extern int L_TbtProbeTable (int, color, INDEX);

// ********************************************************************

int NalimovCacheSize = 32;

static bool NalimovInUse = false;
void *NalimovCache;
Waarde Nalimov_probe(Stelling& pos, Waarde alfa, Waarde beta);

void Nalimov_init(const std::string& path)
{
	bool PathSpecified = (path != "") && (path != "<empty>");
	if (PathSpecified)
	{
		char cPath[1024];
		sprintf(cPath, "%.1023s", path.c_str());
		Tablebases::MaxPieces_dtm = IInitializeTb(cPath);
		if (Tablebases::MaxPieces_dtm)
		{
			NalimovInUse = true;
			Nalimov_setCache(NalimovCacheSize, false);
			Tablebases::EGTB_probe_dtm = &Nalimov_probe;
			sync_cout << "info string Nalimov " << Tablebases::MaxPieces_dtm << " men EGTB available - " << NalimovCacheSize << " MB cache" << sync_endl;
			return;
		}
	}
	if (PathSpecified || NalimovCache)
	{
		NalimovInUse = false;
		if (NalimovCache)
		{
			_aligned_free(NalimovCache);
			NalimovCache = NULL;
		}
		Tablebases::MaxPieces_dtm = 0;
		Tablebases::EGTB_probe_dtm = nullptr;
		sync_cout << "info string Nalimov EGTB not available" << sync_endl;
	}
}

void Nalimov_setCache(unsigned long size, bool verbose)
{
	if (! NalimovInUse)
		NalimovCacheSize = size;
	else if (!NalimovCache || NalimovCacheSize != size)
	{
		if (NalimovCache)
		{
			_aligned_free(NalimovCache);
			NalimovCache = nullptr;
		}
		NalimovCacheSize = size;
		NalimovCache = _aligned_malloc(NalimovCacheSize * 1024 * 1024, 64);
		FTbSetCacheSize(NalimovCache, NalimovCacheSize * 1024 * 1024);
		if (verbose)
			sync_cout << "info string Nalimov " << Tablebases::MaxPieces_dtm << " men EGTB available - " << NalimovCacheSize << " MB cache" << sync_endl;
	}
}

//#define LOG_EGTB_PROBE
#ifdef LOG_EGTB_PROBE
#include <time.h>

static LIJN *tracefile;

void close_egtbtracefile()
{
	if (tracefile)
	{
		fclose(tracefile);
		tracefile = 0;
	}
}

void open_egtbtracefile()
{
	close_egtbtracefile();
	tracefile = fopen("C:\\Data\\Robert\\Schaak\\egtb_trace.log", "a");
	fprintf(tracefile, "\n");
}

void write_egtbtracefile(const char *msg)
{
	time_t rawtime;
	struct tm *timeinfo;
	char STR[64];
	if (! tracefile)
		open_egtbtracefile();
	time (&rawtime);
	timeinfo = localtime ( &rawtime );
	strftime(STR, 64, "%Y/%m/%d %X", timeinfo);
	if (tracefile)
		fprintf(tracefile, "[%s] %s\n", STR, msg);
}

#endif


void register_piece(int rgiCounters[], int offset, veld rgSquares[], int piece, Bitboard bb)
{
	if (bb)
	{
		rgSquares[3 * piece] = pop_lsb(&bb);
		if (bb)
		{
			rgSquares[3 * piece + 1] = pop_lsb(&bb);
			if (bb)
			{
				rgSquares[3 * piece + 2] = pop_lsb(&bb);
				rgiCounters[offset] = 3;
			}
			else
				rgiCounters[offset] = 2;
		}
		else
			rgiCounters[offset] = 1;
	}
	else
		rgiCounters[offset] = 0;
}

Waarde Nalimov_probe(Stelling& pos, Waarde alfa, Waarde beta)
{
	int rgiCounters[10], iTb;
	bool Inverted;
	color side;
	veld sqW[16], sqB[16], *psqW, *psqB;
	veld sqEP;
	INDEX pos_index;

	if (pos.stukken() == pos.stukken(KONING))
		return REMISE_WAARDE;

	register_piece(rgiCounters, 0, sqW, 0, pos.stukken(WIT, PION));
	register_piece(rgiCounters, 1, sqW, 1, pos.stukken(WIT, PAARD));
	register_piece(rgiCounters, 2, sqW, 2, pos.stukken(WIT, LOPER));
	register_piece(rgiCounters, 3, sqW, 3, pos.stukken(WIT, TOREN));
	register_piece(rgiCounters, 4, sqW, 4, pos.stukken(WIT, DAME));
	register_piece(rgiCounters, 5, sqB, 0, pos.stukken(ZWART, PION));
	register_piece(rgiCounters, 6, sqB, 1, pos.stukken(ZWART, PAARD));
	register_piece(rgiCounters, 7, sqB, 2, pos.stukken(ZWART, LOPER));
	register_piece(rgiCounters, 8, sqB, 3, pos.stukken(ZWART, TOREN));
	register_piece(rgiCounters, 9, sqB, 4, pos.stukken(ZWART, DAME));

	iTb = IDescFindFromCounters (rgiCounters);
	if (iTb == 0)
		return GEEN_WAARDE;

	sqW[15] = pos.veld<KONING>(WIT);
	sqB[15] = pos.veld<KONING>(ZWART);

	if (iTb > 0)
	{
		Inverted = false;
		side = pos.aan_zet() == WIT ? x_colorWhite : x_colorBlack;
		psqW = sqW;
		psqB = sqB;
	}
	else
	{
		iTb = -iTb;
		Inverted = true;
		side = pos.aan_zet() == WIT ? x_colorBlack : x_colorWhite;
		psqW = sqB;
		psqB = sqW;
	}
	if (!FRegisteredFun(iTb, side))
		return GEEN_WAARDE;

	sqEP = pos.enpassant_veld();
	if (sqEP == SQ_NONE)
		sqEP = EP_NO_SQUARE;

	pos_index = PfnIndCalcFun(iTb, side) (psqW, psqB, sqEP, Inverted);
	int probe_result = L_TbtProbeTable (iTb, side, pos_index);
	if (probe_result == L_bev_broken)
		return GEEN_WAARDE;

	pos.verhoog_tb_hits();

	Waarde result;
	if (probe_result > 0)
		result = Waarde(MAT_WAARDE - 1 + 2 * (probe_result - L_bev_mi1) - pos.info()->ply);
	else if (probe_result < 0)
		result = Waarde(-MAT_WAARDE + 2 * (L_bev_mi1 + probe_result) + pos.info()->ply);
	else
		result = REMISE_WAARDE;

	#ifdef LOG_EGTB_PROBE
		char s[128];
		const char piece_table[14] =
		{
			' ', 'K', 'P', 'N', 'B', 'R', 'Q',
			' ', 'k', 'p', 'n', 'b', 'r', 'q',
		};
		int count = 0;
		uint64 bb = bb_bezet;
		while (bb)
		{
			int sq = LSB(bb);
			clearLSB(bb);
			s[count++] = piece_table[VELD[sq]];
		}
		sprintf(s + count, ": %d", result);

		write_egtbtracefile(s);
	#endif

	return result;
}

