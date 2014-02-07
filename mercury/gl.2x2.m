/**
 * 2-player game with two actions.  Provides probabilistic and
 * deterministic strategies.  Mutation of one type of strategy does not
 * create the other type.

 * @author Pedro Mariano
 * @version 1.0 2012/07/13
 * @version 2.0 2013/12/30
 */
:- module gl.'2x2'.

:- interface.

:- include_module factory, game, parameters, strategy.
:- import_module  gl.'2x2'.factory, gl.'2x2'.game, gl.'2x2'.parameters, gl.'2x2'.strategy.

:- import_module unit.

:- import_module game, chromosome, gfactory, foldable, printable.

/**
 * The accumulator used to reduce a collection of 2x2 strategies.
 */
:- type ac.

:- pred value(factory, string, game, list({int, strategy}), parameters).
:- mode value(in, out, out, out, out) is nondet.


:- instance game(game, strategy).

:- instance chromosome(strategy, unit, parameters).

:- instance factory(factory, game, strategy, parameters).

:- instance foldable(strategy, ac).

:- instance printable(ac).

/**
 * Return a default factory that creates strategies and games for a batch run.
 */
:- func defaultFactory = factory.

:- implementation.

:- import_module rng, rng.distribution.
:- import_module array, bool, exception, float, int, list, maybe, string.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types


:- type ac --->
	ac(
		qty::int,
		prob::float
	).

:- instance game(game, strategy) where
  [
	func(lowestPayoff/1)  is gl.'2x2'.game.lowestPayoff,
	func(highestPayoff/1) is gl.'2x2'.game.highestPayoff,
	func(paretoPayoff/1)  is gl.'2x2'.game.paretoPayoff,
	func(numberPlayers/1) is gl.'2x2'.game.numberPlayers,
	pred(play/5)          is gl.'2x2'.play
].

:- instance chromosome(strategy, unit, parameters)  where
[
	func(numberGenes/1) is gl.'2x2'.strategy.numberParameters,
	pred(mutateGene/8)  is gl.'2x2'.mutateGene,
	func(born/2)        is gl.'2x2'.born
].

:- instance factory(factory, game, strategy, parameters) where
[
	pred(value/5) is gl.'2x2'.value
].

:- instance foldable(strategy, ac) where
[
	func(fold/2) is gl.'2x2'.fold,
	func(initAC/0) is gl.'2x2'.fold
].

:- instance printable(ac)
	where
[
	pred(print/4) is gl.'2x2'.writeAc
].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

defaultFactory = factory([0.5, 1.5], [-0.5, 0.5], [10], [0.5], [yes, no], [0.5]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

value(Factory, Key, Game, StrategyQuantities, Parameters) :-
	list.member(Temptation, Factory^vTemptation),
	list.member(Sucker,     Factory^vSucker),
	initGame(Temptation, Sucker) = Game,
	list.member(Qty, Factory^vQuantityStrategy),
	(
		Key = string.format("%f %f %f", [f(Temptation), f(Sucker), f(FirstProbability)]),
		list.member(FirstProbability, Factory^vFirstProbability),
		Strategy^firstProbability = FirstProbability,
		StrategyQuantities = [{Qty, Strategy}]
		;
		Key = string.format("%f %f %s", [f(Temptation), f(Sucker), s(string(FirstAction))]),
		list.member(FirstAction, Factory^vFirstAction),
		Strategy^firstAction = FirstAction,
		StrategyQuantities = [{Qty, Strategy}]
		;
		Key = string.format("%f %f %f", [f(Temptation), f(Sucker), f(FractionFirstYes)]),
		list.member(FractionFirstYes, Factory^vFractionFirstYes),
		QtyFirstYes = float.round_to_int(float(Qty) * FractionFirstYes),
		QtyFirstNo = Qty - QtyFirstYes,
		StrategyQuantities = [{QtyFirstYes, dete(yes)}, {QtyFirstNo, dete(no)}]
	),
	Parameters = parameters(-1.0)
	.


% :- func lowestPayoff(game) = float.

% lowestPayoff(Game) = float.min(0.0, Game^sucker).

% :- func highestPayoff(game) = float.

% highestPayoff(Game) = float.max(1.0, Game^temptation).

% :- func paretoPayoff(game) = float.

% paretoPayoff(Game) = float.max(1.0, (Game^temptation + Game^sucker) / 2.0).

% :- func numberPlayers(game) = int.

% numberPlayers(_) = 2.

:- pred play(game, array(strategy), R, R, array(float))
	<= ePRNG(R).
:- mode play(in, in, in, out, out) is det.

play(Game, Profile, !Random, Payoff) :-
	action(array.lookup(Profile, 0), FirstAction0, !Random),
	action(array.lookup(Profile, 1), FirstAction1, !Random),
	(
		FirstAction0 = yes,
		FirstAction1 = yes,
		Payoff0 = 1.0,
		Payoff1 = 1.0
		;
		FirstAction0 = yes,
		FirstAction1 = no,
		Payoff0 = Game^sucker,
		Payoff1 = Game^temptation
		;
		FirstAction0 = no,
		FirstAction1 = yes,
		Payoff0 = Game^temptation,
		Payoff1 = Game^sucker
		;
		FirstAction0 = no,
		FirstAction1 = no,
		Payoff0 = 0.0,
		Payoff1 = 0.0
	),
	Payoff = array.array([Payoff0, Payoff1]).


:- func strategyToString(strategy) = string.

strategyToString(prob(FirstProbability)) = string.format("%f", [f(FirstProbability)]).

strategyToString(dete(FirstAction)) = string(FirstAction).


:- func born(parameters, strategy) = unit.

born(_, _) = unit.


/**
 * mutateGene(Index, !Random, Strategy, Result)
 
 * Mutate the single gene of a 2x2 chromosome.

 * <p> This predicate is part of type class {@code chromosome(strategy,unit,parameters,ac)}.
 *
 */

:- pred mutateGene(parameters, int, distribution, distribution, R, R, strategy, strategy)
	<= ePRNG(R).
:- mode mutateGene(in, in, in, out, in, out, in, out) is det.

mutateGene(Parameters, Index, !Distribution, !Random, Strategy, Result) :-
	(if
		Index = 0
	then
		Strategy = prob(FirstProbability),
		distribution.unitGaussian(Perturb0, !Distribution, !Random),
		Perturb = Perturb0 * Parameters^stdev,
		Result^firstProbability = float.max(0.0, float.min(FirstProbability + Perturb, 1.0))
		;
		Strategy = dete(FirstAction),
		Result^firstAction = bool.not(FirstAction)
	else
		throw("gl.2x2.mutateGene/5: Invalid gene index")
	).

/**
 * fold(Strategy, AC) = Result
  
 * Adds the strategy provide probability to the accumulator.  Updates the
 * number of strategies reduced so far.
 */

:- func fold(strategy, ac) = ac.

fold(Strategy, AC) = Result :-
	Result^qty = AC^qty + 1,
	(
		Strategy = prob(FirstProbability),
		Result^prob = AC^prob + FirstProbability
		;
		Strategy = dete(yes),
		Result^prob = AC^prob + 1.0
		;
		Strategy = dete(no),
		Result^prob = AC^prob
	).

/**
 * fold = Result
  
 * Returns the initial value of the accumulator used to reduce a collection
 * of PGP strategies.
 */

:- func fold = ac.

fold = Result :-
	Result^qty = 0,
	Result^prob = 0.0.


:- pred writeAc(io.output_stream, ac, io, io).
:- mode writeAc(in, in, di, uo) is det.

writeAc(Stream, Ac, !IO) :-
	(if
		Ac^qty = 0
	then
		io.print(Stream, "1/0", !IO)
	else
		io.print(Stream, Ac^prob / float(Ac^qty), !IO)
	).


:- end_module gl.'2x2'.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
