#pragma once

#ifndef K_SEARCH_H_INCLUDED
#define K_SEARCH_H_INCLUDED

#include "types.h"
#include "movepick.h"
#include "movegen.h"

//#define TRACE_LOG

#define S(x) (x * 256 + 50) / 100
const int TypicalPosValue[PIECE_TYPE_NB] = { S(40), S(40), S(40), S(46), S(56), S(71), 0 };
const int MaxPosValue[PIECE_TYPE_NB] = { S(120), S(90), S(121), S(129), S(108), S(121), 0 };
#undef S
const bool dword_A5A620 = true;
const bool dword_A5A628 = true;
const int Stukwaarde_Speciaal[PIECE_TYPE_NB] = { 0, 1, 4, 4, 6, 12, 999, 0 };
const int SEE_stukwaarde[PIECE_TYPE_NB] = { 0, 2, 7, 7, 11, 21, 999, 0 };

extern int MaxStukWaardenPerFase[97][PIECE_TYPE_NB];

enum TPickerActie
{
	ACTIE_NONE = 0,
	ACTIE_TT_ZET = 1,
	ACTIE_SLAGZETTEN = 2,
	ACTIE_KILLER1 = 3,
	ACTIE_KILLER2 = 4,
	ACTIE_COUNTER1 = 5,
	ACTIE_COUNTER2 = 6,
	ACTIE_NORMALEZETTEN = 7,
	ACTIE_UIT_SCHAAK = 8,
	ACTIE_SCHAAKZETTEN = 9,
	ACTIE_BIJNA_PROMOTIEZETTEN = 10,
	ACTIE_SLECHTESLAGEN = 11,
	ACTIE_EINDE = 12,
	ACTIE_SCHAAKZETTEN_GEEN_SEE = 13,
	ACTIE_SLAGZETTEN_GEEN_SEE = 14,
	ACTIE_BIJNA_PROMOTIEZETTEN_GEEN_SEE = 15,
};

enum TPickerStrategie
{
	STRAT_ALLE_ZETTEN = 0,
	STRAT_GA_UIT_SCHAAK = 4,
	STRAT_ENKEL_SLAGZETTEN = 7,
	STRAT_SLAG_SCHAAK_VRIJPION = 8,
	STRAT_SLAG_EN_SCHAAK = 10
};


struct KMovePicker {
	Position *pos;
	HistoryStats *history;
	CounterMoveStats *counterMovesHistory;
	TPickerStrategie strategie;
	int etappe;
	TPickerActie actie;
	ExtMove *zet_max;
	ExtMove *zet_index;
	Move ttMove;
	Move killer1, killer2;
	Move counter1, counter2;
	ExtMove zetlijst[220];
	ExtMove *slechte_zetten;
	ExtMove speciale_zetten[6];
	ExtMove *speciaal_index;
	int diepte;
	bool test_SEE;

	Move GeefQSearchZet();
};

void initMaxStuk();
bool SEE_Verliezende_Ruil(const Position &pos, Move zet);
bool SEE_Echt_Winnend(const Position &pos, Square van, Square naar);

#endif