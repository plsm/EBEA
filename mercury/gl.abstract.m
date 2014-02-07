/**
 * Defines the abstract game that is used by the partner selection
 * demonstrator.  In this game players are characterised by the probability
 * to act good.  The game payoff is one if all players behave correctly or
 * zero otherwise.

 * @author Pedro Mariano
 * @version 1.0 2012/05/26
 */
:- module gl.abstract.

:- interface.

:- import_module game, strategy, chromosome, rng, scanable, printable.
:- import_module array, io, maybe, unit.

%:- type abstractGame.

%:- type abstractStrategy.
:- type abstractGame --->
	game(players::int).

:- type abstractStrategy == float.



:- instance game(abstractGame, abstractStrategy).

:- instance strategy(abstractStrategy).

:- instance chromosome(abstractStrategy, unit, unit, unit).

:- instance scanable(abstractStrategy).

:- instance printable(abstractStrategy).



:- func lowestPayoff(abstractGame) = float.

:- func highestPayoff(abstractGame) = float.

:- func paretoPayoff(abstractGame) = float.

:- func numberPlayers(abstractGame) = int.
	
:- pred play(abstractGame, array(abstractStrategy), R, R, array(float)) <= ePRNG(R).
:- mode play(in, in, in, out, out) is det.

:- pred readGame(io.input_stream, maybe(abstractGame), io, io).
:- mode readGame(in, out, di, uo) is det.

:- pred readStrategy(io.input_stream, maybe(abstractStrategy), io, io).
:- mode readStrategy(in, out, di, uo) is det.

:- pred printStrategy(io.output_stream, abstractStrategy, io, io).
:- mode printStrategy(in, in, di, uo) is det.

:- pred readStrategyParameters(io.input_stream, maybe(unit), io, io).
:- mode readStrategyParameters(in, out, di, uo) is det.


:- implementation.

:- import_module rng.distribution.
:- import_module bool, exception, float, int, list, math, string.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

% :- instance game(abstractGame, abstractStrategy) where
% [
% 	func(lowestPayoff/1) is partnerselection.abstract_game.lowestPayoff,
% 	func(highestPayoff/1) is partnerselection.abstract_game.highestPayoff,
% 	func(paretoPayoff/1) is partnerselection.abstract_game.paretoPayoff,
% 	func(numberPlayers/1) is partnerselection.abstract_game.numberPlayers
% ].

:- instance game(abstractGame, abstractStrategy) where
[
	func(lowestPayoff/1) is gl.abstract.lowestPayoff,
	func(highestPayoff/1) is gl.abstract.highestPayoff,
	func(paretoPayoff/1) is gl.abstract.paretoPayoff,
	func(numberPlayers/1) is gl.abstract.numberPlayers,
%	pred(read/4) is gl.abstract.readGame,
	pred(play/5) is gl.abstract.play
].

:- instance strategy(abstractStrategy) where
[
	func(numberParameters/1) is gl.abstract.numberParameters,
	func(toString/1) is gl.abstract.strategyToString
].

:- instance chromosome(abstractStrategy, unit, unit) where
[
	func(numberGenes/1) is gl.abstract.numberParameters,
	pred(mutateGene/8) is gl.abstract.mutateParameter,
	func(born/1) is gl.abstract.born
].


:- instance scanable(abstractStrategy) where
	[
		pred(scan/4) is gl.abstract.readStrategy
	].

:- instance printable(abstractStrategy) where
	[
		pred(print/4) is gl.abstract.printStrategy
	].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

readStrategyParameters(_, yes(unit), !IO).


lowestPayoff(_Game) = 0.0.

highestPayoff(_Game) = 1.0.

paretoPayoff(_AbstractGame) = 0.5.

numberPlayers(AbstractGame) = AbstractGame^players.


readStrategy(Stream, MStrategy, !IO) :-
	io.read_word(Stream, ROkProbability, !IO),
	(if
		ROkProbability = ok(LCOkProbability),
		string.from_char_list(LCOkProbability, SOkProbability),
		string.to_float(SOkProbability, OkProbability)
	then
		MStrategy = yes(OkProbability)
	else
		MStrategy = no
	).

printStrategy(Stream, Strategy, !IO) :-
	io.print(Stream, Strategy, !IO).

readGame(Stream, MGame, !IO) :-
	io.read_line_as_string(Stream, RLine, !IO),
	(if
		RLine = ok(Line),
		string.words(Line) = [SNumberPlayers],
		string.to_int(SNumberPlayers, NumberPlayers)
	then
		MGame = yes(game(NumberPlayers))
	else
		MGame = no
	).


play(_AbstractGame, Players, !Random, Payoffs) :-
	FoldProb =
	(func(P, AC) = R :-
		R = AC * P
	),
	array.foldl(FoldProb, Players, 1.0) = OkProbability,
	flipCoin(OkProbability, Result, !Random),
	%MapPayoff =
	GeneratePayoff =
	(func(_P) = R :-
		Result = yes,
		R = 1.0
		;
		Result = no,
		R = 0.0
	),
	array.generate(array.size(Players), GeneratePayoff) = Payoffs.
	%array.map(MapPayoff, Players) = Payoffs.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

/**
 * born(Probability) = Result.
  
 * The phenotype of this strategy is equal to its chromosome.

 * <p> This function is part of the chromosome/2 instance.
 */

:- func born(abstractStrategy) = unit.

born(_Probability) = unit.

:- func gameToString(abstractGame) = string.

gameToString(Game) = string.format("%d", [i(Game^players)]).

:- func numberParameters(abstractStrategy) = int.

numberParameters(_) = 1.

:- pred mutateParameter(unit, int, distribution, distribution, R, R, abstractStrategy, abstractStrategy) <= ePRNG(R).
:- mode mutateParameter(in, in, in, out, in, out, in, out) is det.

mutateParameter(_Parameters, Index, !Distribution, !Random, Strategy, Result) :-
	(if
		Index = 0
	then
		nextFloat(Perturb0, !Random),
		Perturb = 2.0 * Perturb0 - 1.0,
		Result = float.max(0.0, float.min(Strategy + Perturb, 1.0))
	else
		throw("mutateParameter/5: Invalid parameter index")
	).

:- func strategyToString(abstractStrategy) = string.

strategyToString(Strategy) = string.format("%f", [f(Strategy)]).

/**
 * Since this an abstract game, the fold method does nothing.
 */
:- func fold(abstractStrategy, unit) = unit.

fold(_, Unit) = Unit.

/**
 * Since this an abstract game, the fold method does nothing.
 */
:- func fold = unit.

fold = unit.

:- end_module gl.abstract.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
