/**
 * An implementation of the Battle of Sexes game.

  
 * @author Pedro Mariano
 * @version 1.0 2014/01/13
 */
:- module gl.battlesexes.

:- interface.

:- include_module game, strategy, parameters.
:- import_module gl.battlesexes.game, gl.battlesexes.strategy, gl.battlesexes.parameters.
:- import_module unit.
:- import_module fraction.

% :- type ac --->
% 	ac(qtyMale                     :: int,
% 		sumProbabilityMaleGoTennis   :: fraction,
% 		qtyFemale                   :: int,
% 		sumProbabilityFemaleGoOpera :: fraction
% 	  ).
:- type ac --->
	ac(qtyMale                     :: int,
		sumProbabilityMaleGoTennis   :: float,
		qtyFemale                   :: int,
		sumProbabilityFemaleGoOpera :: float
	  ).

:- instance abstractGame(game).
:- instance asymmetricGame(game, strategy).

:- instance chromosome(strategy, unit, parameters).

%:- instance factory(factory, game, strategy, parameters).
	
:- instance foldable(strategy, ac).

:- instance printable(strategy).

:- instance printable(ac).

:- implementation.

:- import_module fraction, probability, rng, rng.distribution.
:- import_module array, bool, char, exception, float, int, list, map, maybe, string.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types


:- instance abstractGame(game) where
[
	func(lowestPayoff/1)  is gl.battlesexes.game.lowestPayoff,
	func(highestPayoff/1) is gl.battlesexes.game.highestPayoff,
	func(paretoPayoff/1)  is gl.battlesexes.game.paretoPayoff,
	func(numberPlayers/1) is gl.battlesexes.game.numberPlayers
].

:- instance asymmetricGame(game, strategy) where
[
	func(numberRoles/1)    is gl.battlesexes.numberRoles,
	pred(playAsymmetric/5) is gl.battlesexes.play
].

:- instance chromosome(strategy, unit, parameters) where
[
	func(numberGenes/1) is gl.battlesexes.strategy.numberParameters,
	pred(mutateGene/8)  is gl.battlesexes.strategy.mutateGene,
	func(born/2)        is gl.battlesexes.born
].

% :- instance factory(factory, game, strategy, parameters) where
% [
% 	pred(value/5) is gl.battlesexes.factory.value
% ].

:- instance foldable(strategy, ac) where
[
	func(fold/2) is gl.battlesexes.fold,
	func(initAC/0) is gl.battlesexes.fold
].

:- instance printable(strategy) where
[
	pred(print/4) is gl.battlesexes.strategy.print
].

:- instance printable(ac) where
[
	pred(print/4) is gl.battlesexes.printAc
].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- pred printAc(io.output_stream, ac, io, io).
:- mode printAc(in, in, di, uo) is det.

printAc(Stream, AC, !IO) :-
	io.print(Stream, AC^qtyMale, !IO),
	io.print(Stream, ' ', !IO),
%	fraction.print(Stream, AC^sumProbabilityMaleGoTennis // AC^qtyMale, !IO),
	io.print(Stream, AC^sumProbabilityMaleGoTennis / float(AC^qtyMale), !IO),
	io.print(Stream, ' ', !IO),
	io.print(Stream, AC^qtyFemale, !IO),
	io.print(Stream, ' ', !IO),
%	fraction.print(Stream, AC^sumProbabilityFemaleGoOpera // AC^qtyFemale, !IO)
	io.print(Stream, AC^sumProbabilityFemaleGoOpera / float(AC^qtyFemale), !IO)
	.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% type class {@code game(game)} predicates and functions

:- func numberRoles(game) = int.

numberRoles(_) = 2.

/**
 * play(Game, Profile, !Random, Payoffs)
  
 * Compute the game using the given player profile and return the
 * strategies' payoff.

 * @param Random The pseudo-random number generator used to calculate
 * players' cake division and division acceptance.
 */

:- pred play(game, array(strategy), R, R, maybe(array(float)))
	<= ePRNG(R).
:- mode play(in, in, in, out, out) is det.

play(Game, Profile, !Random, MPayoffs) :-
%	trace[io(!IO)] (io.format("%s => %s\n", [s(string(Game)), s(string(Profile))], !IO)),
	Strategy1 = array.lookup(Profile, 0),
	Strategy2 = array.lookup(Profile, 1),
	(
		Strategy1 = female(_),
		Strategy2 = female(_),
		MPayoffs = no
		;
		Strategy1 = female(_),
		Strategy2 = male(_),
		gameFemaleMale(Game, Strategy1^probabilityOpera, Strategy2^probabilityTennis, !Random, Payoffs),
		MPayoffs = yes(Payoffs)
		;
		Strategy1 = female(_),
		Strategy2 = person(yes, _),
		gameFemaleMale(Game, Strategy1^probabilityOpera, Strategy2^probabilityFavourite, !Random, Payoffs),
		MPayoffs = yes(Payoffs)
		;
		Strategy1 = female(_),
		Strategy2 = person(no, _),
		MPayoffs = no
		;
		Strategy1 = male(_),
		Strategy2 = female(_),
		gameMaleFemale(Game, Strategy1^probabilityTennis, Strategy2^probabilityOpera, !Random, Payoffs),
		MPayoffs = yes(Payoffs)
		;
		Strategy1 = male(_),
		Strategy2 = male(_),
		MPayoffs = no
		;
		Strategy1 = male(_),
		Strategy2 = person(yes, _),
		MPayoffs = no
		;
		Strategy1 = male(_),
		Strategy2 = person(no, _),
		gameMaleFemale(Game, Strategy1^probabilityTennis, Strategy2^probabilityFavourite, !Random, Payoffs),
		MPayoffs = yes(Payoffs)
		;
		Strategy1 = person(yes, _),
		Strategy2 = female(_),
		gameMaleFemale(Game, Strategy1^probabilityFavourite, Strategy2^probabilityOpera, !Random, Payoffs),
		MPayoffs = yes(Payoffs)
		;
		Strategy1 = person(yes, _),
		Strategy2 = male(_),
		MPayoffs = no
		;
		Strategy1 = person(yes, _),
		Strategy2 = person(yes, _),
		MPayoffs = no
		;
		Strategy1 = person(yes, _),
		Strategy2 = person(no, _),
		gameMaleFemale(Game, Strategy1^probabilityFavourite, Strategy2^probabilityFavourite, !Random, Payoffs),
		MPayoffs = yes(Payoffs)
		;
		Strategy1 = person(no, _),
		Strategy2 = female(_),
		MPayoffs = no
		;
		Strategy1 = person(no, _),
		Strategy2 = male(_),
		gameFemaleMale(Game, Strategy1^probabilityFavourite, Strategy2^probabilityTennis, !Random, Payoffs),
		MPayoffs = yes(Payoffs)
		;
		Strategy1 = person(no, _),
		Strategy2 = person(yes, _),
		gameFemaleMale(Game, Strategy1^probabilityFavourite, Strategy2^probabilityFavourite, !Random, Payoffs),
		MPayoffs = yes(Payoffs)
		;
		Strategy1 = person(no, _),
		Strategy2 = person(no, _),
		MPayoffs = no
	).

:- pred gameFemaleMale(game, probability, probability, R, R, array(float)) <= ePRNG(R).
:- mode gameFemaleMale(in, in, in, in, out, out) is det.

gameFemaleMale(Game, ProbabilityFemale, ProbabilityMale, !Random, Payoffs) :-
	probability.flipCoin(ProbabilityFemale, FemaleOpera, !Random),
	probability.flipCoin(ProbabilityMale, MaleTennis, !Random),
	(if
		FemaleOpera = yes,
		MaleTennis = no
	then
		Payoffs = array.from_list([float(Game^payoffSamePlaceLike), float(Game^payoffSamePlaceDislike)])
	else if
		FemaleOpera = no,
		MaleTennis = yes
	then
		Payoffs = array.from_list([float(Game^payoffSamePlaceDislike), float(Game^payoffSamePlaceLike)])
	else
		Payoffs = array.from_list([float(Game^payoffDiffPlace), float(Game^payoffDiffPlace)])
	).

:- pred gameMaleFemale(game, probability, probability, R, R, array(float)) <= ePRNG(R).
:- mode gameMaleFemale(in, in, in, in, out, out) is det.

gameMaleFemale(Game, ProbabilityMale, ProbabilityFemale, !Random, Payoffs) :-
	probability.flipCoin(ProbabilityFemale, FemaleOpera, !Random),
	probability.flipCoin(ProbabilityMale, MaleTennis, !Random),
	(if
		FemaleOpera = yes,
		MaleTennis = no
	then
		Payoffs = array.from_list([float(Game^payoffSamePlaceDislike), float(Game^payoffSamePlaceLike)])
	else if
		FemaleOpera = no,
		MaleTennis = yes
	then
		Payoffs = array.from_list([float(Game^payoffSamePlaceLike), float(Game^payoffSamePlaceDislike)])
	else
		Payoffs = array.from_list([float(Game^payoffDiffPlace), float(Game^payoffDiffPlace)])
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% type class {@code strategy(strategy)} predicates and functions

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% type class {@code chromosome(strategy,unit,parameters,ac)} predicates and functions

/**
 * The result of developing an Battlesexes chromosome is nothing.  This is
 * represented by an {@code unit} value.

 * <p> This function is part of type class {@code chromosome(strategy)}.
 *
 */
:- func born(parameters, strategy) = unit.

born(_, _) = unit.

/**
 * fold(Strategy, AC) = Result
  
 * Adds the strategy provide probability to the accumulator.  Updates the
 * number of strategies reduced so far.
 */

:- func fold(strategy, ac) = ac.

fold(Strategy, AC) = Result :-
	Strategy = female(P),
	AC1 = 'sumProbabilityFemaleGoOpera :='(AC, AC^sumProbabilityFemaleGoOpera + float(P)),
%	AC1 = 'sumProbabilityFemaleGoOpera :='(AC, AC^sumProbabilityFemaleGoOpera + probability.fraction(P)),
	Result = 'qtyFemale :='(AC1, AC^qtyFemale + 1)
	;
	Strategy = male(P),
	AC1 = 'sumProbabilityMaleGoTennis :='(AC, AC^sumProbabilityMaleGoTennis + float(P)),
%	AC1 = 'sumProbabilityMaleGoTennis :='(AC, AC^sumProbabilityMaleGoTennis + probability.fraction(P)),
	Result = 'qtyMale :='(AC1, AC^qtyMale + 1)
	;
	Strategy = person(yes, P),
	AC1 = 'sumProbabilityMaleGoTennis :='(AC, AC^sumProbabilityMaleGoTennis + float(P)),
	Result = 'qtyMale :='(AC1, AC^qtyMale + 1)
	;
	Strategy = person(no, P),
	AC1 = 'sumProbabilityFemaleGoOpera :='(AC, AC^sumProbabilityFemaleGoOpera + float(P)),
	Result = 'qtyFemale :='(AC1, AC^qtyFemale + 1)
	.

/**
 * fold = Result
  
 * Returns the initial value of the accumulator used to reduce a collection
 * of PGP strategies.
 */

:- func fold = ac.

fold = Result :-
	Result^qtyMale = 0,
	Result^sumProbabilityMaleGoTennis = 0.0,
%	Result^sumProbabilityMaleGoTennis = fraction.zero,
	Result^qtyFemale = 0,
	Result^sumProbabilityFemaleGoOpera = 0.0
%	Result^sumProbabilityFemaleGoOpera = fraction.zero
	.



:- end_module gl.battlesexes.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
