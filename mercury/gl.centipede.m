/**
 * This module provides an implementation of Centipede Game.  This is a
 * multi-stage game where in each stage one player has the chance to stop
 * or continue the game.  If the game continues, players exchange roles in
 * the next stage.

 * @author Pedro Mariano
 * @version 1.0 2013/03/30
 * @version 2.0 2013/12/30
 */
:- module gl.centipede.

:- interface.

:- include_module factory, game, strategy, parameters.
:- import_module gl.centipede.factory, gl.centipede.game, gl.centipede.parameters, gl.centipede.strategy. 

:- import_module bool, unit.


/**
 * Accumulator used to reduce a collection of strategies.
 */
:- type ac.


:- instance abstractGame(game).
:- instance asymmetricGame(game, strategy).

:- instance chromosome(strategy, unit, parameters).

:- instance foldable(strategy, ac).


:- instance printable(ac).


% :- pred scanFactory(io.input_stream, scanable.result(factory), io.state, io.state).
% :- mode scanFactory(in, out, di, uo) is det.

:- implementation.

:- import_module rng.distribution, rng.
:- import_module array, bool, exception, float, int, list, maybe, table_statistics, string.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types


:- type ac --->
	ac(
		qtyFirstStrategies  :: int,
		sumFirstTime        :: int,
		% qtyMixedStrategies :: int,
		% sumMixedTime       :: int,
		qtySecondStrategies :: int,
		sumSecondTime       :: int
	).


:- instance abstractGame(game)
	where
[
	func(lowestPayoff/1)  is gl.centipede.game.lowestPayoff,
	func(highestPayoff/1) is gl.centipede.game.highestPayoff,
	func(paretoPayoff/1)  is gl.centipede.game.paretoPayoff,
	func(numberPlayers/1) is gl.centipede.game.numberPlayers
].

:- instance asymmetricGame(game, strategy) where
[
	func(numberRoles/1)    is gl.centipede.numberRoles,
	pred(playAsymmetric/5) is gl.centipede.play
].

:- instance chromosome(strategy, unit, parameters) where
[
	func(numberGenes/1) is gl.centipede.strategy.numberParameters,
	pred(mutateGene/8)  is gl.centipede.mutateGene,
	func(born/2)        is gl.centipede.born
].

:- instance foldable(strategy, ac) where
[
	func(fold/2) is gl.centipede.fold,
	func(initAC/0) is gl.centipede.fold
].


:- instance printable(ac) where
[
	pred(print/4) is gl.centipede.printAc
].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

% :- type stype --->
% 	first ;
% 	second.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions



/*
scanFactory(Stream, IMFactory, !IO) :-
	util.scanValues(Stream, "numberStages",    string.to_int,     IMNumberStages, !IO),
	util.scanValues(Stream, "potShare",        string.to_float,   IMPotShare, !IO),
	util.scanValues(Stream, "potIncrease",     stringPotIncrease, IMPotIncrease, !IO),
	scanable.scanInt(Stream, yes, "numberCopies",    bounded(1, yes), unbound, IMNumberCopies,    !IO),
	scanable.scanInt(Stream, yes, "firstStart",      bounded(0, yes), unbound, IMFirstStart,      !IO),
	scanable.scanInt(Stream, yes, "firstIncrement",  bounded(1, yes), unbound, IMFirstIncrement,  !IO),
	scanable.scanInt(Stream, yes, "secondStart",     bounded(0, yes), unbound, IMSecondStart,     !IO),
	scanable.scanInt(Stream, yes, "secondIncrement", bounded(1, yes), unbound, IMSecondIncrement, !IO),
	util.scanValues(Stream, "timeStdDev",      string.to_float,   IMTimeStdDev, !IO),
	(if
		IMNumberStages    = ok(ok(VNumberStages)),
		IMPotShare        = ok(ok(VPotShare)),
		IMPotIncrease     = ok(ok(Factory^vPotIncrease)),
		IMNumberCopies    = ok(ok(Factory^vNumberCopies)),
		IMFirstStart      = ok(ok(Factory^vFirstStart)),
		IMFirstIncrement  = ok(ok(Factory^vFirstIncrement)),
		IMSecondStart     = ok(ok(Factory^vSecondStart)),
		IMSecondIncrement = ok(ok(Factory^vSecondIncrement)),
		IMTimeStdDev      = ok(ok(VTimeStdDev))
	then
		Factory^vNumberStages = VNumberStages,
		Factory^vPotShare = VPotShare,
		Factory^vTimeStdDev = VTimeStdDev,
		Factory = factory(VNumberStages, VPotShare, VPotIncrease, VNumberCopies, VFirstStart, VFirstIncrement
		IMFactory = ok(ok(Factory))
	else
		IMFactory = ok(error("" + IMNumberStages + IMPotShare + IMPotIncrease + IMFirstStart + IMSecondStart + IMSecondIncrement + IMTimeStdDev))
	).
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- func rangeInt(int, int, int) = list(int).
																	  
rangeInt(Min, Step, Max) = Result :-
	(if
		Min >= Max
	then
		Result = [Max]
	else
		Result = [Min | rangeInt(Min + Step, Step, Max)]
	).



																	  
:- pred stringPotIncrease(string, potIncrease).
:- mode stringPotIncrease(in, out) is semidet.

stringPotIncrease(String, PotIncrease) :-
	string.first_char(String, Code, SNumber),
	string.to_float(SNumber, Number),
	(
		Code = 'g',
		PotIncrease = geometric(Number)
		;
		Code = 'a',
		PotIncrease = arithmetic(Number)
	).




:- pred parseStrategyType(string, strategyType).
:- mode parseStrategyType(in, out) is semidet.
:- mode parseStrategyType(out, in) is det.

parseStrategyType("first", first).
%parseStrategyType("mixed", mixed).
parseStrategyType("second", second).


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
	array.lookup(Profile, 0) = StrategyA,
	array.lookup(Profile, 1) = StrategyB,
	(if
		strategiesTime(StrategyA, StrategyB, Time, AMovesFirst),
		PotSize = gl.centipede.game.potSize(Game, Time)
	then
		AMovesFirst = yes,
		(if
			Time /\ 1 = 1
		then
			Payoffs = array([PotSize * Game^potShare,         PotSize * (1.0 - Game^potShare)])
		else
			Payoffs = array([PotSize * (1.0 - Game^potShare), PotSize * Game^potShare])
		),
		MPayoffs = yes(Payoffs)
		;
		AMovesFirst = no,
		(if
			Time /\ 1 = 0
		then
			Payoffs = array([PotSize * Game^potShare,         PotSize * (1.0 - Game^potShare)])
		else
			Payoffs = array([PotSize * (1.0 - Game^potShare), PotSize * Game^potShare])
		),
		MPayoffs = yes(Payoffs)
	else
		MPayoffs = no
	).
/*	
	(
		StrategyA = first(TimeA),
		(
			(
				StrategyB = second(TimeB)
				;
				StrategyB = mixed(TimeB)
			),
			Time = int.min(TimeA, TimeB),
			PotSize = potSize(Game, Time),
			(if
				Time /\ 1 = 1
			then
				Payoffs = array([PotSize * Game^potShare,         PotSize * (1.0 - Game^potShare)])
			else
				Payoffs = array([PotSize * (1.0 - Game^potShare), PotSize * Game^potShare])
			)
			;
			StrategyB = first(_),
			Payoffs = array([0.0, 0.0])
		)
		;
		(
			StrategyA = second(TimeA)
			;
			StrategyA = mixed(TimeA)
		),
		StrategyB = first(TimeB),
		Time = int.min(TimeA, TimeB),
		PotSize = potSize(Game, Time),
		(if
			Time /\ 1 = 0
		then
			Payoffs = array([PotSize * Game^potShare,         PotSize * (1.0 - Game^potShare)])
		else
			Payoffs = array([PotSize * (1.0 - Game^potShare), PotSize * Game^potShare])
		)
		;
		StrategyA = second(_),
		StrategyB = second(_),
		Payoffs = array([0.0, 0.0])
		;
		StrategyA = mixed(TimeA),
		StrategyB = mixed(TimeB),
		Time = int.min(TimeA, TimeB),
		PotSize = potSize(Game, Time),
		(if
			Time /\ 1 = 1
		then
			Payoffs = array([PotSize * Game^potShare,         PotSize * (1.0 - Game^potShare)])
		else
			Payoffs = array([PotSize * (1.0 - Game^potShare), PotSize * Game^potShare])
		)
	).
*/

:- pred strategiesTime(strategy, strategy, int, bool).
:- mode strategiesTime(in, in, out, out) is semidet.

%strategiesTime(first(TimeA),  mixed(TimeB),  int.min(TimeA, TimeB), yes).
strategiesTime(first(TimeA),  second(TimeB), int.min(TimeA, TimeB), yes).
%strategiesTime(mixed(TimeA),  first(TimeB),  int.min(TimeA, TimeB), no).
%strategiesTime(mixed(TimeA),  mixed(TimeB),  int.min(TimeA, TimeB), yes).
%strategiesTime(mixed(TimeA),  second(TimeB), int.min(TimeA, TimeB), yes).
strategiesTime(second(TimeA), first(TimeB),  int.min(TimeA, TimeB), no).
%strategiesTime(second(TimeA), mixed(TimeB),  int.min(TimeA, TimeB), no).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% type class {@code strategy(strategy)} predicates and functions

:- func strategyToString(strategy) = string.

strategyToString(Strategy) = Result :-
	Strategy = first(Time),
	Result = string.format("f %d", [i(Time)])
	% ;
	% Strategy = mixed(Time),
	% Result = string.format("m %d", [i(Time)])
	;
	Strategy = second(Time),
	Result = string.format("s %d", [i(Time)])
	.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% type class {@code chromosome(strategy,unit,parameters,ac)} predicates and functions

/**
 * The result of developing a Centipede chromosome is nothing.  This is
 * represented by an {@code unit} value.

 * <p> This function is part of type class {@code chromosome(strategy)}.
 *
 */
:- func born(parameters, strategy) = unit.

born(_Parameters, _Strategy) = unit.

/**
 * mutateGene(Index, !Random, Strategy, Result)
 
 * Mutate the single gene of a Centipede chromosome.  The unique parameter
 * is perturbed by a Gaussian distribution.

 * <p> This predicate is part of type class {@code chromosome(strategy)}.
 *
 */

:- pred mutateGene(parameters, int, distribution, distribution, R, R, strategy, strategy)
	<= ePRNG(R).
:- mode mutateGene(in, in, in, out, in, out, in, out) is det.

mutateGene(Parameters, Index, !Distribution, !Random, Strategy, Result) :-
	Strategy = first(Time),
	(if
		Index = 0
	then
		distribution.unitGaussian(Perturb0, !Distribution, !Random),
		NewTime =
			int.max(
				1,
				int.min(float.round_to_int(float(Time) + Perturb0 * Parameters^timeStdDev) \/ 1,
				Parameters^numberStagesCopy)),
		Result = first(NewTime)
	else
		throw("gl.centipede.mutateGene/5: Invalid gene index")
	)
	;
	% Strategy = mixed(Time),
	% (if
	% 	Index = 0
	% then
	% 	distribution.unitGaussian(Perturb0, !Distribution, !Random),
	% 	NewTime = 
	% 		int.max(
	% 			1,
	% 			int.min(float.round_to_int(float(Time) + Perturb0 * Parameters^timeStdDev) /\ \ 1,
	% 			Parameters^numberStagesCopy)),
	% 	Result = mixed(NewTime)
	% else
	% 	throw("gl.centipede.mutateGene/5: Invalid gene index")
	% )
	% ;
	Strategy = second(Time),
	(if
		Index = 0
	then
		distribution.unitGaussian(Perturb0, !Distribution, !Random),
		NewTime = 
			int.max(
				2,
				int.min(float.round_to_int(float(Time) + Perturb0 * Parameters^timeStdDev) /\ \ 1,
				Parameters^numberStagesCopy)),
		Result = second(NewTime)
	else
		throw("gl.centipede.mutateGene/5: Invalid gene index")
	)
	.

/**
 * fold(Strategy, AC) = Result
  
 * Adds the strategy provide probability to the accumulator.  Updates the
 * number of strategies reduced so far.
 */

:- func fold(strategy, ac) = ac.

fold(Strategy, AC) = Result :-
	Strategy = first(Time),
	AC1 = 'qtyFirstStrategies :='(AC, AC^qtyFirstStrategies + 1),
	Result = 'sumFirstTime :='(AC1, AC^sumFirstTime + Time)
	% ;
	% Strategy = mixed(Time),
	% AC1 = 'qtyMixedStrategies :='(AC, AC^qtyMixedStrategies + 1),
	% Result = 'sumMixedTime :='(AC1, AC^sumMixedTime + Time)
	;
	Strategy = second(Time),
	AC1 = 'qtySecondStrategies :='(AC, AC^qtySecondStrategies + 1),
	Result = 'sumSecondTime :='(AC1, AC^sumSecondTime + Time)
	.

/**
 * fold = Result
  
 * Returns the initial value of the accumulator used to reduce a collection
 * of Centipede strategies.
 */

:- func fold = ac.

fold = Result :-
	Result^qtyFirstStrategies = 0,
	Result^sumFirstTime = 0,
	% Result^qtyMixedStrategies = 0,
	% Result^sumMixedTime = 0,
	Result^qtySecondStrategies = 0,
	Result^sumSecondTime = 0
	.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% type class {@code writeable(ac)} predicate

:- pred printAc(io.output_stream, ac, io, io).
:- mode printAc(in, in, di, uo) is det.

printAc(Stream, AC, !IO) :-
	io.print(Stream, AC^qtyFirstStrategies, !IO),
	io.print(Stream, ' ', !IO),
	(if
		AC^qtyFirstStrategies = 0
	then
		io.print(Stream, "1/0", !IO)
	else
		io.print(Stream, float(AC^sumFirstTime) / float(AC^qtyFirstStrategies), !IO)
	),
	io.print(Stream, ' ', !IO),
	io.print(Stream, AC^qtySecondStrategies, !IO),
	io.print(Stream, ' ', !IO),
	(if
		AC^qtySecondStrategies = 0
	then
		io.print(Stream, "1/0", !IO)
	else
		io.print(Stream, float(AC^sumSecondTime) / float(AC^qtySecondStrategies), !IO)
	)
	% io.print(Stream, ' ', !IO),
	% io.print(Stream, AC^qtyMixedStrategies, !IO),
	% io.print(Stream, ' ', !IO),
	% (if
	% 	AC^qtyMixedStrategies = 0
	% then
	% 	io.print(Stream, "1/0", !IO)
	% else
	% 	io.print(Stream, float(AC^sumMixedTime) / float(AC^qtyMixedStrategies), !IO)
	% )
	.

:- end_module gl.centipede.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
