
#include "k_search.h"

int MaxStukWaardenPerFase[97][PIECE_TYPE_NB];

const TPickerActie MovePickerStages[11][10] = {
/*0*/	{ ACTIE_TT_ZET, ACTIE_SLAGZETTEN, ACTIE_KILLER1, ACTIE_KILLER2, ACTIE_COUNTER1, ACTIE_COUNTER2, ACTIE_NORMALEZETTEN, ACTIE_SLECHTESLAGEN, ACTIE_EINDE },
		{ ACTIE_TT_ZET, ACTIE_SLAGZETTEN, ACTIE_SCHAAKZETTEN, ACTIE_SLECHTESLAGEN, ACTIE_EINDE },
		{ ACTIE_TT_ZET, ACTIE_SLAGZETTEN, ACTIE_SCHAAKZETTEN, ACTIE_EINDE },
		{ ACTIE_TT_ZET, ACTIE_SLAGZETTEN, ACTIE_EINDE },
/*4*/	{ ACTIE_TT_ZET, ACTIE_UIT_SCHAAK, ACTIE_SLECHTESLAGEN, ACTIE_EINDE },
		{ ACTIE_TT_ZET, ACTIE_SLAGZETTEN, ACTIE_SCHAAKZETTEN, ACTIE_BIJNA_PROMOTIEZETTEN, ACTIE_EINDE },
		{ ACTIE_TT_ZET, ACTIE_SLAGZETTEN, ACTIE_SCHAAKZETTEN, ACTIE_BIJNA_PROMOTIEZETTEN, ACTIE_SLECHTESLAGEN, ACTIE_EINDE },
/*7*/	{ ACTIE_TT_ZET, ACTIE_SLAGZETTEN_GEEN_SEE, ACTIE_EINDE },
/*8*/	{ ACTIE_TT_ZET, ACTIE_SLAGZETTEN_GEEN_SEE, ACTIE_SCHAAKZETTEN_GEEN_SEE, ACTIE_BIJNA_PROMOTIEZETTEN_GEEN_SEE, ACTIE_EINDE },
		{ ACTIE_TT_ZET, ACTIE_SLAGZETTEN_GEEN_SEE, ACTIE_SCHAAKZETTEN_GEEN_SEE, ACTIE_BIJNA_PROMOTIEZETTEN_GEEN_SEE, ACTIE_EINDE },
/*10*/	{ ACTIE_TT_ZET, ACTIE_SLAGZETTEN_GEEN_SEE, ACTIE_SCHAAKZETTEN_GEEN_SEE, ACTIE_EINDE }
};

void initMaxStuk()
{
	for (int fase = 0; fase <= 96; fase++)
	{
		MaxStukWaardenPerFase[fase][NO_PIECE] = 0;
		MaxStukWaardenPerFase[fase][PAWN] = (93312 - 375 * fase) / 375; // 960; omzetting naar 1/256 pion
		MaxStukWaardenPerFase[fase][KNIGHT] = (335040 - 220 * fase) / 375; // 960; omzetting naar 1/256 pion
		MaxStukWaardenPerFase[fase][BISHOP] = (352800 - 46 * fase) / 375; // 960; omzetting naar 1/256 pion
		MaxStukWaardenPerFase[fase][ROOK] = (556320 - 1067 * fase) / 375; // 960; omzetting naar 1/256 pion
		MaxStukWaardenPerFase[fase][QUEEN] = (1040448 - 878 * fase) / 375; // 960; omzetting naar 1/256 pion
	}
}

Move KMovePicker::GeefQSearchZet()
{
	while (true)
	{
		while (zet_index == zet_max)
		{
			actie = MovePickerStages[strategie][etappe];
			etappe++;
			switch (actie)
			{
			default:
				continue;
			case ACTIE_EINDE:
				return MOVE_NONE;
			case ACTIE_TT_ZET:
				slechte_zetten = zetlijst + 220;
				speciaal_index = speciale_zetten;
				if (!ttMove || !pos->pseudo_legal(ttMove))
					continue;
				return ttMove;
			case ACTIE_SLAGZETTEN_GEEN_SEE:
				zet_index = zetlijst;
				zet_max = generate<CAPTURES>(*pos, zetlijst);
				for (ExtMove *m = zetlijst; m < zet_max; ++m)
					m->value = PieceValue[MG][pos->piece_on(to_sq(*m))]
					- 200 * relative_rank(pos->side_to_move(), to_sq(*m));
				break;
			case ACTIE_SCHAAKZETTEN_GEEN_SEE:
				zet_index = zetlijst;
				zet_max = generate<QUIET_CHECKS>(*pos, zetlijst);
				// schaakzetten van paard krijgen prioriteit
				for (ExtMove *m = zetlijst; m < zet_max; ++m)
				{
					if (type_of(pos->piece_on(from_sq(*m))) == KNIGHT)
						m->value = Value(99);
					else
						m->value = VALUE_ZERO;
				}
				break;
			case ACTIE_UIT_SCHAAK:
				zet_index = zetlijst;
				zet_max = generate<EVASIONS>(*pos, zetlijst);
				for (ExtMove *m = zetlijst; m < zet_max; ++m)
				{
					if (type_of(*m) == ENPASSANT)
					{
						m->value = HistoryStats::Max + 30000;
					}
					else if (type_of(*m) == PROMOTION)
					{
						m->value = HistoryStats::Max + 31000;
					}
					else
					{
						Square naar = to_sq(*m);
						Square van = from_sq(*m);
						if (pos->piece_on(naar) != NO_PIECE)
							m->value = HistoryStats::Max + 8 * type_of(pos->piece_on(naar)) - 2 * type_of(pos->piece_on(van));
						else if (m->move == killer1)
							m->value = HistoryStats::Max + 29000;
						else if (m->move == killer2)
							m->value = HistoryStats::Max + 28000;
						else
							m->value = Value(((*history)[pos->piece_on(van)][naar] / 2
								+ (*counterMovesHistory)[pos->piece_on(van)][naar] / 2) / 2);
					}
				}
				test_SEE = true;
				break;
			case ACTIE_SLECHTESLAGEN:
				zet_index = slechte_zetten;
				zet_max = &zetlijst[220];
				test_SEE = false;
				break;
			case ACTIE_BIJNA_PROMOTIEZETTEN_GEEN_SEE:
				zet_index = zetlijst;
				zet_max = zetlijst;
				Bitboard b;
				if (pos->side_to_move() == BLACK)
					b = (~pos->pieces() << 8) & ~pos->pieces() & (pos->pieces(BLACK, PAWN) >> 8) & 0xFFFF00;
				else
					b = (~pos->pieces() >> 8) & ~pos->pieces() & (pos->pieces(WHITE, PAWN) << 8) & 0xFFFF0000000000;
				while (b)
				{
					Square veld = pop_lsb(&b);
					zet_max->move = make_move(veld + (pos->side_to_move() == WHITE ? DELTA_S : DELTA_N), veld);
					zet_max->value = VALUE_ZERO;
					zet_max++;
				}
				break;
			}
		}

		// vind zet met beste score
		int beste_waarde = zet_index->value;
		ExtMove* beste_zet_index = zet_index;
		for (ExtMove* n = zet_index + 1; n < zet_max; ++n)
		{
			if (n->value > beste_waarde)
			{
				beste_waarde = n->value;
				beste_zet_index = n;
			}
		};
		if (zet_index != beste_zet_index)
		{
			ExtMove tmp = *beste_zet_index;
			*beste_zet_index = *zet_index;
			*zet_index = tmp;
		}

		Move zet = zet_index->move;
		zet_index++;
		if (zet == ttMove)
			continue;

		if (!test_SEE
			|| type_of(zet) == ENPASSANT
			|| !SEE_Verliezende_Ruil(*pos, zet))
			return zet;

		slechte_zetten--;
		*slechte_zetten = *(zet_index - 1);
	}
}


	bool SEE_Echt_Winnend(const Position &pos, Square van, Square naar)
	{
		// SEE (kleur, van, naar)
		// met speciale stukwaarden (waardoor er enkel echt winnende uitwisselingen overblijven): 1 4 4 6 12
		// met waarden 1 3 3 5 9 blijven er enkel echt verliezende uitwisselingen over
		// waarden 2 7 7 11 21 zijn neutraal

		const Color Us = pos.side_to_move();
		const Color Them = (Us == WHITE ? BLACK : WHITE);

		int score = Stukwaarde_Speciaal[type_of(pos.piece_on(naar))];
		if (!score)
			return false;
		score -= Stukwaarde_Speciaal[type_of(pos.piece_on(van))];
		if (score > 0)
			return true;

		Bitboard mask = ~SquareBB[van] & (pos.attacks_from<KNIGHT>(naar) | PseudoAttacks[QUEEN][naar]) & ~SquareBB[naar];
		Bitboard aanvallers = mask & pos.pieces(Them);
		if (!aanvallers)
			return true;
		Bitboard verdedigers = mask & pos.pieces(Us);
		while (true)
		{
			Bitboard laagste_aanvallers, attackB, attackR;
			laagste_aanvallers = aanvallers & pos.pieces(PAWN) & pos.attacks_from<PAWN>(naar, Us);
			if (laagste_aanvallers)
				goto LABEL_128;
			laagste_aanvallers = aanvallers & pos.pieces(KNIGHT) & pos.attacks_from<KNIGHT>(naar);
			if (laagste_aanvallers)
				goto LABEL_128;
			attackB = attacks_bb<BISHOP>(naar, aanvallers | verdedigers);
			laagste_aanvallers = aanvallers & pos.pieces(BISHOP) & attackB;
			if (laagste_aanvallers)
				goto LABEL_128;
			attackR = attacks_bb<ROOK>(naar, aanvallers | verdedigers);
			laagste_aanvallers = aanvallers & pos.pieces(ROOK) & attackR;
			if (laagste_aanvallers)
				goto LABEL_128;
			laagste_aanvallers = aanvallers & pos.pieces(QUEEN) & (attackB | attackR);
			if (laagste_aanvallers)
				goto LABEL_128;
			laagste_aanvallers = aanvallers & pos.pieces(KING) & pos.attacks_from<KING>(naar);
			if (!laagste_aanvallers)
				return true;
		LABEL_128:
			if (score < -100)
				return false;
			Square aanval_van = lsb(laagste_aanvallers);
			score += Stukwaarde_Speciaal[type_of(pos.piece_on(aanval_van))];
			if (score <= 0)
				return false;
			if (!verdedigers)
				return false;
			aanvallers &= ~SquareBB[aanval_van];

			laagste_aanvallers = verdedigers & pos.pieces(PAWN) & pos.attacks_from<PAWN>(naar, Them);
			if (laagste_aanvallers)
				goto LABEL_206;
			laagste_aanvallers = verdedigers & pos.pieces(KNIGHT) & pos.attacks_from<KNIGHT>(naar);
			if (laagste_aanvallers)
				goto LABEL_206;
			attackB = attacks_bb<BISHOP>(naar, aanvallers | verdedigers);
			laagste_aanvallers = verdedigers & pos.pieces(BISHOP) & attackB;
			if (laagste_aanvallers)
				goto LABEL_206;
			attackR = attacks_bb<ROOK>(naar, aanvallers | verdedigers);
			laagste_aanvallers = verdedigers & pos.pieces(ROOK) & attackR;
			if (laagste_aanvallers)
				goto LABEL_206;
			laagste_aanvallers = verdedigers & pos.pieces(QUEEN) & (attackB | attackR);
			if (laagste_aanvallers)
				goto LABEL_206;
			laagste_aanvallers = verdedigers & pos.pieces(KING) & pos.attacks_from<KING>(naar);
			if (!laagste_aanvallers)
				return false;
		LABEL_206:
			if (score > 100)
				return true;
			aanval_van = lsb(laagste_aanvallers);
			score -= Stukwaarde_Speciaal[type_of(pos.piece_on(aanval_van))];
			if (score > 0)
				return true;
			if (!aanvallers)
				return true;
			verdedigers &= ~SquareBB[aanval_van];
		}
	}

	bool SEE_Verliezende_Ruil(const Position &pos, Move zet)
	{
		const Color Us = pos.side_to_move();
		const Color Them = (Us == WHITE ? BLACK : WHITE);

		Square van = from_sq(zet);
		Square naar = to_sq(zet);
		if (type_of(zet) == ENPASSANT)
			return false;
		if (type_of(pos.piece_on(van)) == KING)
			return false;

		int score = SEE_stukwaarde[type_of(pos.piece_on(naar))];
		score -= SEE_stukwaarde[type_of(pos.piece_on(van))];
		if (score >= 0)
			return false;

		Bitboard mask = ~SquareBB[van] & (pos.attacks_from<KNIGHT>(naar) | PseudoAttacks[QUEEN][naar]) & ~SquareBB[naar];
		Bitboard aanvallers = mask & pos.pieces(Them);
		if (!aanvallers)
			return false;
		Bitboard verdedigers = mask & pos.pieces(Us);
		while (true)
		{
			Bitboard laagste_aanvallers, attackB, attackR;
			laagste_aanvallers = aanvallers & pos.pieces(PAWN) & pos.attacks_from<PAWN>(naar, Us);
			if (laagste_aanvallers)
				goto LABEL_128;
			laagste_aanvallers = aanvallers & pos.pieces(KNIGHT) & pos.attacks_from<KNIGHT>(naar);
			if (laagste_aanvallers)
				goto LABEL_128;
			attackB = attacks_bb<BISHOP>(naar, aanvallers | verdedigers);
			laagste_aanvallers = aanvallers & pos.pieces(BISHOP) & attackB;
			if (laagste_aanvallers)
				goto LABEL_128;
			attackR = attacks_bb<ROOK>(naar, aanvallers | verdedigers);
			laagste_aanvallers = aanvallers & pos.pieces(ROOK) & attackR;
			if (laagste_aanvallers)
				goto LABEL_128;
			laagste_aanvallers = aanvallers & pos.pieces(QUEEN) & (attackB | attackR);
			if (laagste_aanvallers)
				goto LABEL_128;
			laagste_aanvallers = aanvallers & pos.pieces(KING) & pos.attacks_from<KING>(naar);
			if (!laagste_aanvallers)
				return false;
		LABEL_128:
			if (score < -100)
				return true;
			Square aanval_van = lsb(laagste_aanvallers);
			score += SEE_stukwaarde[type_of(pos.piece_on(aanval_van))];
			if (score < 0)
				return true;
			if (!verdedigers)
				return true;
			aanvallers &= ~SquareBB[aanval_van];

			laagste_aanvallers = verdedigers & pos.pieces(PAWN) & pos.attacks_from<PAWN>(naar, Them);
			if (laagste_aanvallers)
				goto LABEL_206;
			laagste_aanvallers = verdedigers & pos.pieces(KNIGHT) & pos.attacks_from<KNIGHT>(naar);
			if (laagste_aanvallers)
				goto LABEL_206;
			attackB = attacks_bb<BISHOP>(naar, aanvallers | verdedigers);
			laagste_aanvallers = verdedigers & pos.pieces(BISHOP) & attackB;
			if (laagste_aanvallers)
				goto LABEL_206;
			attackR = attacks_bb<ROOK>(naar, aanvallers | verdedigers);
			laagste_aanvallers = verdedigers & pos.pieces(ROOK) & attackR;
			if (laagste_aanvallers)
				goto LABEL_206;
			laagste_aanvallers = verdedigers & pos.pieces(QUEEN) & (attackB | attackR);
			if (laagste_aanvallers)
				goto LABEL_206;
			laagste_aanvallers = verdedigers & pos.pieces(KING) & pos.attacks_from<KING>(naar);
			if (!laagste_aanvallers)
				return true;
		LABEL_206:
			if (score > 100)
				return false;
			aanval_van = lsb(laagste_aanvallers);
			score -= SEE_stukwaarde[type_of(pos.piece_on(aanval_van))];
			if (score >= 0)
				return false;
			if (!aanvallers)
				return false;
			verdedigers &= ~SquareBB[aanval_van];
		}
	}

