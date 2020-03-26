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

#include <cassert>

#include "movegen.h"
#include "position.h"

namespace {

  template<RokadeMogelijkheid rokade, bool AlleenSchaakZetten, bool Chess960>
  ZetEx* schrijf_rokade(const Stelling& pos, ZetEx* zetten) {

	const Kleur IK = rokade <= WIT_LANG ? WIT : ZWART;
    const Kleur JIJ = (IK == WIT ? ZWART : WIT);
    const bool korte_rokade = (rokade == WIT_KORT || rokade == ZWART_KORT);

    if (pos.rokade_verhinderd(rokade) || !pos.rokade_mogelijk(rokade))
        return zetten;

	const Veld vanK = Chess960 ? pos.veld<KONING>(IK) : relatief_veld(IK, SQ_E1);
	const Veld naarK = relatief_veld(IK, korte_rokade ? SQ_G1 : SQ_C1);
	const Veld richting = naarK > vanK ? LINKS : RECHTS;

    assert(!pos.schaak_gevers());

	if (Chess960) {
		for (Veld veld = naarK; veld != vanK; veld += richting)
			if (pos.aanvallers_naar(veld) & pos.stukken(JIJ))
				return zetten;

		Veld vanT = pos.rokade_toren_veld(naarK);
		if (aanval_bb<TOREN>(naarK, pos.stukken() ^ vanT) & pos.stukken(JIJ, TOREN, DAME))
			return zetten;
	}
	else {
		if (pos.aanvallers_naar(naarK) & pos.stukken(JIJ))
			return zetten;
		if (pos.aanvallers_naar(naarK + richting) & pos.stukken(JIJ))
			return zetten;
	}

    Zet zet = maak_zet<ROKADE>(vanK, naarK);

    if (AlleenSchaakZetten && !pos.geeft_schaak(zet))
        return zetten;

    *zetten++ = zet;
    return zetten;
  }


  template<Kleur IK, ZetGeneratie Type, Veld Delta>
  ZetEx* schrijf_promoties(const Stelling& pos, ZetEx* zetten, Veld naar) {

    const Kleur JIJ = (IK == WIT ? ZWART : WIT);

    if (Type == GEN_SLAG_OF_PROMOTIE || Type == GEN_GA_UIT_SCHAAK || Type == GEN_ALLE_ZETTEN)
        *zetten++ = maak_zet<PROMOTIE>(naar - Delta, naar, DAME);

    if (Type == GEN_RUSTIGE_ZETTEN || Type == GEN_GA_UIT_SCHAAK || Type == GEN_ALLE_ZETTEN)
    {
        *zetten++ = maak_zet<PROMOTIE>(naar - Delta, naar, TOREN);
        *zetten++ = maak_zet<PROMOTIE>(naar - Delta, naar, LOPER);
        *zetten++ = maak_zet<PROMOTIE>(naar - Delta, naar, PAARD);
    }

    // Knight promotion is the only promotion that can give a direct check
    // that's not already included in the queen promotion.
    if (Type == GEN_RUSTIGE_SCHAAKS && (KorteAanval[W_PAARD][naar] & pos.veld<KONING>(JIJ)))
        *zetten++ = maak_zet<PROMOTIE>(naar - Delta, naar, PAARD);

    return zetten;
  }


  template<Kleur IK, ZetGeneratie Type>
  ZetEx* zetten_voor_pion(const Stelling& pos, ZetEx* zetten, Bitboard target) {

    // Compute our parametrized parameters at compile time, named according to
    // the point of view of white side.
    const Kleur JIJ = (IK == WIT ? ZWART : WIT);
    const Bitboard TRank8BB = (IK == WIT ? Rank8BB  : Rank1BB);
    const Bitboard TRank7BB = (IK == WIT ? Rank7BB  : Rank2BB);
    const Bitboard TRank3BB = (IK == WIT ? Rank3BB  : Rank6BB);
    const Veld Up    = (IK == WIT ? BOVEN  : ONDER);
    const Veld Right = (IK == WIT ? RECHTSBOVEN : LINKSONDER);
    const Veld Left  = (IK == WIT ? LINKSBOVEN : RECHTSONDER);

    Bitboard emptySquares;

    Bitboard pawnsOn7    = pos.stukken(IK, PION) &  TRank7BB;
    Bitboard pawnsNotOn7 = pos.stukken(IK, PION) & ~TRank7BB;

    Bitboard enemies = (Type == GEN_GA_UIT_SCHAAK ? pos.stukken(JIJ) & target:
                        Type == GEN_SLAG_OF_PROMOTIE ? target : pos.stukken(JIJ));

    // Single and double pawn pushes, no promotions
    if (Type != GEN_SLAG_OF_PROMOTIE)
    {
        emptySquares = (Type == GEN_RUSTIGE_ZETTEN || Type == GEN_RUSTIGE_SCHAAKS ? target : ~pos.stukken());

        Bitboard b1 = shift_bb<Up>(pawnsNotOn7)   & emptySquares;
        Bitboard b2 = shift_bb<Up>(b1 & TRank3BB) & emptySquares;

        if (Type == GEN_GA_UIT_SCHAAK) // Consider only blocking squares
        {
            b1 &= target;
            b2 &= target;
        }

        if (Type == GEN_RUSTIGE_SCHAAKS)
        {
            b1 &= pos.aanval_van<PION>(pos.veld<KONING>(JIJ), JIJ);
            b2 &= pos.aanval_van<PION>(pos.veld<KONING>(JIJ), JIJ);

            // Add pawn pushes which give discovered check. This is possible only
            // if the pawn is not on the same file as the enemy king, because we
            // don't generate captures. Note that a possible discovery check
            // promotion has been already generated amongst the captures.
			Bitboard dcCandidates = pos.info()->xRay[~pos.aan_zet()];
            if (pawnsNotOn7 & dcCandidates)
            {
                Bitboard dc1 = shift_bb<Up>(pawnsNotOn7 & dcCandidates) & emptySquares & ~bb_lijn(pos.veld<KONING>(JIJ));
                Bitboard dc2 = shift_bb<Up>(dc1 & TRank3BB) & emptySquares;

                b1 |= dc1;
                b2 |= dc2;
            }
        }

        while (b1)
        {
            Veld to = pop_lsb(&b1);
            *zetten++ = maak_zet(to - Up, to);
        }

        while (b2)
        {
            Veld to = pop_lsb(&b2);
            *zetten++ = maak_zet(to - Up - Up, to);
        }
    }

    // Promotions and underpromotions
    if (pawnsOn7 && (Type != GEN_GA_UIT_SCHAAK || (target & TRank8BB)))
    {
        if (Type == GEN_SLAG_OF_PROMOTIE)
            emptySquares = ~pos.stukken();

        if (Type == GEN_GA_UIT_SCHAAK)
            emptySquares &= target;

        Bitboard b1 = shift_bb<Right>(pawnsOn7) & enemies;
        Bitboard b2 = shift_bb<Left >(pawnsOn7) & enemies;
        Bitboard b3 = shift_bb<Up   >(pawnsOn7) & emptySquares;

		while (b1)
			zetten = schrijf_promoties<IK, Type, Right>(pos, zetten, pop_lsb(&b1));

        while (b2)
            zetten = schrijf_promoties<IK, Type, Left >(pos, zetten, pop_lsb(&b2));

        while (b3)
            zetten = schrijf_promoties<IK, Type, Up   >(pos, zetten, pop_lsb(&b3));
    }

    // Standard and en-passant captures
    if (Type == GEN_SLAG_OF_PROMOTIE || Type == GEN_GA_UIT_SCHAAK || Type == GEN_ALLE_ZETTEN)
    {
        Bitboard b1 = shift_bb<Right>(pawnsNotOn7) & enemies;
        Bitboard b2 = shift_bb<Left >(pawnsNotOn7) & enemies;

        while (b1)
        {
            Veld to = pop_lsb(&b1);
            *zetten++ = maak_zet(to - Right, to);
        }

        while (b2)
        {
            Veld to = pop_lsb(&b2);
            *zetten++ = maak_zet(to - Left, to);
        }

        if (pos.enpassant_veld() != SQ_NONE)
        {
            assert(rij(pos.enpassant_veld()) == relatieve_rij(IK, RIJ_6));

            // An en passant capture can be an evasion only if the checking piece
            // is the double pushed pawn and so is in the target. Otherwise this
            // is a discovery check and we are forced to do otherwise.
            if (Type == GEN_GA_UIT_SCHAAK && !(target & (pos.enpassant_veld() - Up)))
                return zetten;

            b1 = pawnsNotOn7 & pos.aanval_van<PION>(pos.enpassant_veld(), JIJ);

            assert(b1);

            while (b1)
                *zetten++ = maak_zet<ENPASSANT>(pop_lsb(&b1), pos.enpassant_veld());
        }
    }

    return zetten;
  }


  template<Kleur IK, StukType Pt, bool AlleenSchaakZetten>
  ZetEx* zetten_voor_stuk(const Stelling& pos, ZetEx* zetten, Bitboard target) {

    assert(Pt != KONING && Pt != PION);

    const Veld* pl = pos.veld_lijst<Pt>(IK);

    for (Veld van = *pl; van != SQ_NONE; van = *++pl)
    {
        if (AlleenSchaakZetten)
        {
            if (    (Pt == LOPER || Pt == TOREN || Pt == DAME)
                && !(LegeAanval[Pt][van] & target & pos.info()->schaakVelden[Pt]))
                continue;

			// aftrekschaaks worden in genereerZetten<GEN_RUSTIGE_SCHAAKS> geschreven
            if (pos.info()->xRay[~pos.aan_zet()] & van)
                continue;
        }

        Bitboard b = pos.aanval_van<Pt>(van) & target;

        if (AlleenSchaakZetten)
            b &= pos.info()->schaakVelden[Pt];

        while (b)
            *zetten++ = maak_zet(van, pop_lsb(&b));
    }

    return zetten;
  }


  template<Kleur IK, ZetGeneratie Type>
  ZetEx* zetten_voor_alle_stukken(const Stelling& pos, ZetEx* zetten, Bitboard target) {

    const bool AlleenSchaakZetten = Type == GEN_RUSTIGE_SCHAAKS;

    zetten = zetten_voor_pion<IK, Type>(pos, zetten, target);
    zetten = zetten_voor_stuk<IK, PAARD, AlleenSchaakZetten>(pos, zetten, target);
    zetten = zetten_voor_stuk<IK, LOPER, AlleenSchaakZetten>(pos, zetten, target);
    zetten = zetten_voor_stuk<IK, TOREN, AlleenSchaakZetten>(pos, zetten, target);
    zetten = zetten_voor_stuk<IK, DAME, AlleenSchaakZetten>(pos, zetten, target);

    if (Type != GEN_RUSTIGE_SCHAAKS && Type != GEN_GA_UIT_SCHAAK)
    {
        Veld veldK = pos.veld<KONING>(IK);
        Bitboard b = pos.aanval_van<KONING>(veldK) & target;
        while (b)
            *zetten++ = maak_zet(veldK, pop_lsb(&b));
    }

    if (Type != GEN_SLAG_OF_PROMOTIE && Type != GEN_GA_UIT_SCHAAK && pos.rokade_mogelijkheden(IK))
    {
        if (pos.is_chess960())
        {
            zetten = schrijf_rokade<IK == WIT ? WIT_KORT : ZWART_KORT, AlleenSchaakZetten, true>(pos, zetten);
            zetten = schrijf_rokade<IK == WIT ? WIT_LANG : ZWART_LANG, AlleenSchaakZetten, true>(pos, zetten);
        }
        else
        {
            zetten = schrijf_rokade<IK == WIT ? WIT_KORT : ZWART_KORT, AlleenSchaakZetten, false>(pos, zetten);
            zetten = schrijf_rokade<IK == WIT ? WIT_LANG : ZWART_LANG, AlleenSchaakZetten, false>(pos, zetten);
        }
    }

    return zetten;
  }

} // namespace


/// generate<SLAGZETTEN> generates all pseudo-legal captures and queen
/// promotions. Returns a pointer to the end of the move list.
///
/// generate<RUSTIGE_ZETTEN> generates all pseudo-legal non-captures and
/// underpromotions. Returns a pointer to the end of the move list.
///
/// generate<ALLE_ZETTEN> generates all pseudo-legal captures and
/// non-captures. Returns a pointer to the end of the move list.

template<ZetGeneratie Type>
ZetEx* genereerZetten(const Stelling& pos, ZetEx* zetten) {

  assert(Type == GEN_SLAG_OF_PROMOTIE || Type == GEN_RUSTIGE_ZETTEN || Type == GEN_ALLE_ZETTEN);
  assert(!pos.schaak_gevers());

  Kleur ik = pos.aan_zet();

  Bitboard target =  Type == GEN_SLAG_OF_PROMOTIE ? pos.stukken(~ik)
                   : Type == GEN_RUSTIGE_ZETTEN ? ~pos.stukken()
                   : Type == GEN_ALLE_ZETTEN ? ~pos.stukken(ik) : 0;

  return ik == WIT ? zetten_voor_alle_stukken<WIT, Type>(pos, zetten, target)
                   : zetten_voor_alle_stukken<ZWART, Type>(pos, zetten, target);
}

// Explicit template instantiations
template ZetEx* genereerZetten<GEN_SLAG_OF_PROMOTIE>(const Stelling&, ZetEx*);
template ZetEx* genereerZetten<GEN_RUSTIGE_ZETTEN>(const Stelling&, ZetEx*);
template ZetEx* genereerZetten<GEN_ALLE_ZETTEN>(const Stelling&, ZetEx*);


/// generate<RUSTIGE_SCHAAKS> generates all pseudo-legal non-captures and knight
/// underpromotions that give check. Returns a pointer to the end of the move list.
template<>
ZetEx* genereerZetten<GEN_RUSTIGE_SCHAAKS>(const Stelling& pos, ZetEx* zetten) {

  assert(!pos.schaak_gevers());

  Kleur ik = pos.aan_zet();
  Bitboard aftrekSchaak = pos.aftrek_schaak_mogelijk();

  while (aftrekSchaak)
  {
     Veld van = pop_lsb(&aftrekSchaak);
     StukType pt = stuk_type(pos.stuk_op_veld(van));

     if (pt == PION)
         continue; // Will be generated together with direct checks

     Bitboard b = pos.aanval_van(Stuk(pt), van) & ~pos.stukken();

     if (pt == KONING)
         b &= ~LegeAanval[DAME][pos.veld<KONING>(~ik)];

     while (b)
         *zetten++ = maak_zet(van, pop_lsb(&b));
  }

  return ik == WIT ? zetten_voor_alle_stukken<WIT, GEN_RUSTIGE_SCHAAKS>(pos, zetten, ~pos.stukken())
                   : zetten_voor_alle_stukken<ZWART, GEN_RUSTIGE_SCHAAKS>(pos, zetten, ~pos.stukken());
}


/// generate<GA_UIT_SCHAAK> generates all pseudo-legal check evasions when the side
/// to move is in check. Returns a pointer to the end of the move list.
template<>
ZetEx* genereerZetten<GEN_GA_UIT_SCHAAK>(const Stelling& pos, ZetEx* zetten) {

  assert(pos.schaak_gevers());

  Kleur ik = pos.aan_zet();
  Veld veldK = pos.veld<KONING>(ik);
  Bitboard aangevallenVelden = 0;
  Bitboard verreAanvallers = pos.schaak_gevers() & ~pos.stukken(PAARD, PION);

  // Find all the squares attacked by slider schaak_gevers. We will remove them from
  // the king evasions in order to skip known illegal moves, which avoids any
  // useless legality checks later on.
  while (verreAanvallers)
  {
      Veld schaakveld = pop_lsb(&verreAanvallers);
      aangevallenVelden |= bbVerbinding[schaakveld][veldK] ^ schaakveld;
  }

  // Generate evasions for king, capture and non capture moves
  Bitboard b = pos.aanval_van<KONING>(veldK) & ~pos.stukken(ik) & ~aangevallenVelden;
  while (b)
      *zetten++ = maak_zet(veldK, pop_lsb(&b));

  if (meer_dan_een(pos.schaak_gevers()))
      return zetten; // Double check, only a king move can save the day

  // Generate blocking evasions or captures of the checking piece
  Veld schaakVeld = lsb(pos.schaak_gevers());
  Bitboard target = bb_tussen(schaakVeld, veldK) | schaakVeld;

  return ik == WIT ? zetten_voor_alle_stukken<WIT, GEN_GA_UIT_SCHAAK>(pos, zetten, target)
                   : zetten_voor_alle_stukken<ZWART, GEN_GA_UIT_SCHAAK>(pos, zetten, target);
}


/// generate<LEGALE_ZETTEN> generates all the legal moves in the given position

template<>
ZetEx* genereerZetten<GEN_LEGALE_ZETTEN>(const Stelling& pos, ZetEx* zetten) {

  Bitboard pinned = pos.gepende_stukken();
  Veld veldK = pos.veld<KONING>(pos.aan_zet());
  ZetEx* pZet = zetten;

  zetten = pos.schaak_gevers() ? genereerZetten<GEN_GA_UIT_SCHAAK>(pos, zetten)
                            : genereerZetten<GEN_ALLE_ZETTEN>(pos, zetten);
  while (pZet != zetten)
      if (   (pinned || van_veld(*pZet) == veldK || zet_type(*pZet) == ENPASSANT)
          && !pos.legale_zet(*pZet))
          *pZet = (--zetten)->zet;
      else
          ++pZet;

  return zetten;
}


ZetEx* genereerSlagzettenOpVeld(const Stelling& pos, ZetEx* zetten, Veld sq) {

	Bitboard target = bbVeld[sq];

	return pos.aan_zet() == WIT
		? zetten_voor_alle_stukken<WIT, GEN_SLAG_OF_PROMOTIE>(pos, zetten, target)
		: zetten_voor_alle_stukken<ZWART, GEN_SLAG_OF_PROMOTIE>(pos, zetten, target);
}
