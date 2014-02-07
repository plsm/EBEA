/**
 * 

 * @author Pedro Mariano
 * @version 1.0 2013/12/13
 */
:- module gl.'pgp+pa'.strategy.

:- interface.

/**
 * The strategy of a Public Good Provision game with punishing and
 * abstaining.  It must tell if a player abstains from participating or
 * not, if it participates, it must indicate if it provides for the
 * good.  Moreover it must indicate if it punishes.
  
 * <p>A deterministic strategy is represented by constructor {@code
 * deti(maybe(maybe(bool)))}.  Values represent the following behaviours:

 * <ul>
  
 * <li> {@code deti(no)} A loner that abstains from play the PGP.  He gets
 * the payoff {@code lonerPayoff}.</li>

 * <li> {@code deti(yes(no))}  A player of the PGP that defects. </li>

 * <li> {@code deti(yes(yes(no)))} A player of the PGP that cooperates but
 * does not punishes defectors. </li>

 * <li> {@code deti(yes(yes(yes)))} A player of the PGP that cooperates and
 * punishes defectors. </li>

 * </ul>
 */

:- type strategy --->
	pure(ldcp).

/**
 * Enumeration that represents the four types of strategies used by
 * <i>Hauert et al 2008</i>.  These are deterministic strategies.
 */

:- type ldcp --->
	loner ;
	defector ;
	cooperator ;
	punisher.

:- instance printable(strategy).

:- instance parseable(strategy).

:- func init(ldcp) = strategy.

:- func default = strategy.

/**
 * Returns value {@code 1}.  The strategy of the Public Good Provision with
 * punishment and abstaining game only contains one parameter: the strategy type.

 * <p> This function is part of type class {@code strategy(strategy)}
 * and {@code chromosome(strategy,strategy)}.
 */

:- func numberParameters(strategy) = int.

/**
 * mutateGene(Index, !Random, Strategy, Result)
 
 * Mutate the single gene of a Public Good Provision chromosome.  Currently
 * we sum an uniform random variable.

 * <p> This predicate is part of type class {@code chromosome(strategy,strategy)}.
 *
 */

:- pred mutateGene(parameters, int, distribution, distribution, R, R, strategy, strategy)
	<= ePRNG(R).
:- mode mutateGene(in, in, in, out, in, out, in, out) is det.

:- func dialog = list(dialogItem(strategy)).

/**
 * <p> This predicate is part of type classes {@code strategy(strategy)}
 * and {@code chromosome(strategy,strategy)}.
 */

:- pred print(io.output_stream, strategy, io, io).
:- mode print(in, in, di, uo) is det.

:- pred stringToLDCP(string, ldcp).
:- mode stringToLDCP(in, out) is semidet.
:- mode stringToLDCP(out, out) is multi.
:- mode stringToLDCP(out, in) is det.

:- implementation.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

:- instance printable(strategy) where
	[
		pred(print/4) is gl.'pgp+pa'.strategy.print
	].

:- instance parseable(strategy) where
[
	pred(parse/3) is gl.'pgp+pa'.strategy.parse
].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

init(LDCP) = pure(LDCP).

default = pure(cooperator).

dialog =
	[di(label("loner"), newValue(pure(loner))),
	 di(label("defector"), newValue(pure(defector))),
	 di(label("cooperator"), newValue(pure(cooperator))),
	 di(label("punisher"),   newValue(pure(punisher)))
	].

numberParameters(_) = 1.

mutateGene(noParameters, Index, !Distribution, !Random, Strategy, Result) :-
	(if
		Index = 0
	then
		Strategy = pure(Type),
		rng.nextInt(0, 2, NewTypeIndex, !Random),
		(if
			mutateLDCP(Type, NewTypeIndex, NewType)
		then
			Result = pure(NewType)
		else
			throw("gl.pgp+pa.mutateGene/5 problem in predicate mutateLDCP/3")
		)
	else
		throw("gl.'pgp+pa'.mutateGene/5: Invalid gene index")
	).


print(Stream, Strategy, !IO) :-
	Strategy = pure(Type),
	io.print(Stream, Type, !IO).

stringToLDCP("loner", loner).
stringToLDCP("defector", defector).
stringToLDCP("cooperator", cooperator).
stringToLDCP("punisher", punisher).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- pred parse(strategy, list(int), list(int)).
:- mode parse(in, out, in) is det.
:- mode parse(out, in, out) is semidet.

parse(pure(LDCP)) -->
	[0],
	parseLDCP(LDCP)
	.

:- pred parseLDCP(ldcp, list(int), list(int)).
:- mode parseLDCP(in, out, in) is det.
:- mode parseLDCP(out, in, out) is semidet.

parseLDCP(loner) -->      [0].
parseLDCP(defector) -->   [1].
parseLDCP(cooperator) --> [2].
parseLDCP(punisher) -->   [3].

:- pred mutateLDCP(ldcp, int, ldcp).
:- mode mutateLDCP(in, in, out) is semidet.
:- mode mutateLDCP(in, out, in) is semidet.
:- mode mutateLDCP(in, out, out) is multi.

mutateLDCP(loner, 0, defector).
mutateLDCP(loner, 1, cooperator).
mutateLDCP(loner, 2, punisher).
mutateLDCP(defector, 0, loner).
mutateLDCP(defector, 1, cooperator).
mutateLDCP(defector, 2, punisher).
mutateLDCP(cooperator, 0, loner).
mutateLDCP(cooperator, 1, defector).
mutateLDCP(cooperator, 2, punisher).
mutateLDCP(punisher, 0, loner).
mutateLDCP(punisher, 1, defector).
mutateLDCP(punisher, 2, cooperator).

:- end_module gl.'pgp+pa'.strategy.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
