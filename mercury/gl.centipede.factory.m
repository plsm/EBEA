/**
 * 

 * @author Pedro Mariano
 * @version 1.0 2013/12/11
 */
:- module gl.centipede.factory.

:- interface.

:- type factory.

:- import_module gl.centipede.game.

:- type factory --->
	factory(
		vNumberStages    :: list(int),
		vPotShare        :: list(float),
		vPotIncrease     :: list(potIncrease),
		vNumberCopies    :: int,
		vFirstStart      :: int,
		vFirstIncrement  :: int,
		vSecondStart     :: int,
		vSecondIncrement :: int,
		vTimeStdDev      :: list(float)
	).


/**
 * This predicate checks if a factory has a set of valid parameters.  If
 * there are some invalid values, the predicate fails.
 */

:- pred check(factory).
:- mode check(in) is semidet.

:- func default = factory.

:- func vNumberStages(    factory) = list(int).
%:- mode vNumberStages(in) = out is det.
%:- mode vNumberStages(out(bound(factory(ground, free, free, free, free, free, free, free, free)))) = in is det.
:- func vPotShare(        factory) = list(float).
:- func vPotIncrease( factory) = list(potIncrease).

%:- mode vPotShare(in) = out is det.
%:- mode vPotShare(out) = in is det.

:- func 'vNumberStages :='(    factory, list(int)) = factory.
:- func 'vPotShare :='(        factory, list(float)) = factory.
:- func 'vPotIncrease :='(     factory, list(potIncrease)) = factory.
:- func 'vNumberCopies :='(    factory, int) = factory.
:- func 'vFirstStart :='(      factory, int) = factory.
:- func 'vFirstIncrement :='(  factory, int) = factory.
:- func 'vSecondStart :='(     factory, int) = factory.
:- func 'vSecondIncrement :='( factory, int) = factory.
:- func vTimeStdDev(      factory) = list(float).
:- func 'vTimeStdDev :='(      factory, list(float)) = factory.

:- instance factory(factory, game, strategy, parameters).

:- implementation.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

:- instance factory(factory, game, strategy, parameters) where
[
	pred(value/5) is gl.centipede.factory.value
].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

default = factory(
	[4,6],                             % number of stages
	[0.8, 0.9],                        % pot share
	[arithmetic(0.5), geometric(1.5)], % pot increase
	10,                                % number of copies
	1, 1,                              % first strategy parameters
	1, 1,                              % second strategy parameters
	[1.0]                              % time to stop standard deviation
).

check(Factory) :-
	(all [NumberStages]
		list.member(NumberStages, Factory^vNumberStages)
		=>
		NumberStages > 0
	),
	(all [PotShare]
		list.member(PotShare, Factory^vPotShare)
		=>
		(
			PotShare > 0.5,
			PotShare =< 1.0
		)
	),
	(all [Ratio]
		list.member(geometric(Ratio), Factory^vPotIncrease)
		=>
		Ratio > 1.0
	),
	(all [Increase]
		list.member(arithmetic(Increase), Factory^vPotIncrease)
		=>
		Increase > 0.0
	),
	Factory^vNumberCopies > 0,
	Factory^vFirstStart > 0,
	Factory^vFirstIncrement > 0,
	Factory^vSecondStart > 0,
	Factory^vSecondIncrement > 0,
	(all [TimeStdDev]
		list.member(TimeStdDev, Factory^vTimeStdDev)
		=>
		TimeStdDev >= 0.0
	).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

/**
 * Given a factory this predicate succeeds with the games, strategies and
 * parameters from the factory.
 */

:- pred value(factory, string, game, list({int, strategy}), parameters).
:- mode value(in, out, out, out, out) is nondet.

value(Factory, Key, Game, [{QtyFirst, first(StrategyFirst)}, {QtySecond, second(StrategySecond)}], Parameter) :-
	Key = string.format("%d %f %s %d %d %d %d",
		[i(NumberStages), f(PotShare), s(string(PotIncrease)),
		 i(QtyFirst), i(StrategyFirst),
		 i(QtySecond), i(StrategySecond)]),
	%
	list.member(NumberStages, Factory^vNumberStages),
	list.member(PotShare,     Factory^vPotShare),
	list.member(PotIncrease,  Factory^vPotIncrease),
	gl.centipede.game.init(NumberStages, PotShare, PotIncrease, Game),
	%
	QtyFirst = Factory^vNumberCopies,
	QtySecond = Factory^vNumberCopies,
	list.member(StrategyFirst,  rangeInt(Factory^vFirstStart, Factory^vFirstIncrement, NumberStages)),
	list.member(StrategySecond, rangeInt(Factory^vSecondStart, Factory^vSecondIncrement, NumberStages)),
	%
	list.member(TimeStdDev, Factory^vTimeStdDev),
	Parameter = parameters(TimeStdDev, Game^numberStages)
	.

:- end_module gl.centipede.factory.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
