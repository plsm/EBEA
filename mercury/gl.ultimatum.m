/**
 * An implementation of the Ultimatum game.

 * <p> <b>TO DO</b>: How can I guarantee that strategy parameters are
 * valid?  The cake division is always equal or less than the cake size?
 * Predicate {@code readStrategy} does not have a parameter with type
 * {@code game}.
  
 * @author Pedro Mariano
 * @version 1.0 2012/12/10
 * @version 2.0 2013/12/30
 */
:- module gl.ultimatum.

:- interface.

:- include_module game, strategy, factory, parameters.
:- import_module gl.ultimatum.game, gl.ultimatum.strategy, gl.ultimatum.factory, gl.ultimatum.parameters.
:- import_module unit.

:- type ac.

:- instance abstractGame(game).
:- instance asymmetricGame(game, strategy).

:- instance chromosome(strategy, unit, parameters).

:- instance factory(factory, game, strategy, parameters).
	
:- instance foldable(strategy, ac).

:- instance printable(strategy).

:- instance printable(ac).

:- implementation.

:- import_module rng, rng.distribution.
:- import_module array, bool, char, exception, float, int, list, map, maybe, string.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

:- type ac --->
	ac(cakeDivisionSum::int,
		qtyDictator::int,
		cakeAcceptanceThresholdSum::int,
		qtySerfSimple::int).

:- instance abstractGame(game) where
[
	func(lowestPayoff/1)  is gl.ultimatum.game.lowestPayoff,
	func(highestPayoff/1) is gl.ultimatum.game.highestPayoff,
	func(paretoPayoff/1)  is gl.ultimatum.game.paretoPayoff,
	func(numberPlayers/1) is gl.ultimatum.game.numberPlayers
].

:- instance asymmetricGame(game, strategy) where
[
	func(numberRoles/1)    is gl.ultimatum.numberRoles,
	pred(playAsymmetric/5) is gl.ultimatum.play
].

:- instance chromosome(strategy, unit, parameters) where
[
	func(numberGenes/1) is gl.ultimatum.strategy.numberParameters,
	pred(mutateGene/8)  is gl.ultimatum.strategy.mutateGene,
	func(born/2)        is gl.ultimatum.born
].

:- instance factory(factory, game, strategy, parameters) where
[
	pred(value/5) is gl.ultimatum.factory.value
].

:- instance foldable(strategy, ac) where
[
	func(fold/2) is gl.ultimatum.fold,
	func(initAC/0) is gl.ultimatum.fold
].

:- instance printable(strategy) where
[
	pred(print/4) is gl.ultimatum.strategy.print
].

:- instance printable(ac) where
[
	pred(print/4) is gl.ultimatum.printAc
].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

:- type stype --->
	dictator ;
	serfSimple ;
	serfComplete.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- pred printAc(io.output_stream, ac, io, io).
:- mode printAc(in, in, di, uo) is det.

printAc(Stream, AC, !IO) :-
	io.print(Stream, AC^qtyDictator, !IO),
	io.print(Stream, ' ', !IO),
	(if
		AC^qtyDictator = 0
	then
		io.print(Stream, "1/0", !IO)
	else
		io.print(Stream, float(AC^cakeDivisionSum) / float(AC^qtyDictator), !IO)
	),
	io.print(Stream, ' ', !IO),
	io.print(Stream, AC^qtySerfSimple, !IO),
	io.print(Stream, ' ', !IO),
	(if
		AC^qtySerfSimple = 0
	then
		io.print(Stream, "1/0", !IO)
	else
		io.print(Stream, float(AC^cakeAcceptanceThresholdSum) / float(AC^qtySerfSimple), !IO)
	)
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
	array.foldl2(cakeSizeDictator, Profile, no, MCakeSizeDictator, !Random),
	(
		MCakeSizeDictator = yes(CakeSizeDictator),
		array.foldl2(cakeAcceptance(Game^cakeSize - CakeSizeDictator), Profile, no, MCakeAcceptance, !Random),
		(
			MCakeAcceptance = yes(CakeAcceptance),
			(
				CakeAcceptance = yes,
				Payoffs = array.generate(2, payoff(Profile, float(CakeSizeDictator), float(Game^cakeSize - CakeSizeDictator)))
				;
				CakeAcceptance = no,
				Payoffs = array.init(2, 0.0)
			),
			MPayoffs = yes(Payoffs)
			;
			% no serfs in the profile
			MCakeAcceptance = no,
			MPayoffs = no
		)
		;
		% no dictators in the profile
		MCakeSizeDictator = no,
		MPayoffs = no
	).

:- pred cakeSizeDictator(strategy, maybe(int), maybe(int), R, R)
	<= ePRNG(R).
:- mode cakeSizeDictator(in, in, out, in, out) is det.

cakeSizeDictator(Strategy, AC, MCakeSizeDictator, !Random) :-
	AC = no,
	(
		Strategy = d(dictator(CakeDivision)),
		MCakeSizeDictator = yes(CakeDivision)
		;
		Strategy = s(_),
		MCakeSizeDictator = no
	)
	;
	AC = yes(_),
	(
		% there can only be one dictator in the profile
		Strategy = d(_),
		MCakeSizeDictator = no
		;
		Strategy = s(_),
		MCakeSizeDictator = AC
	)
	.

:- pred cakeAcceptance(int, strategy, maybe(bool), maybe(bool), R, R)
	<= ePRNG(R).
:- mode cakeAcceptance(in, in, in, out, in, out) is det.

cakeAcceptance(CakeSizeSerf, Strategy, AC, MCakeAcceptance, !Random) :-
	AC = no,
	(
		Strategy = d(_),
		MCakeAcceptance = no
		;
		Strategy = s(Serf),
		MCakeAcceptance = yes(accepts(CakeSizeSerf, Serf))
		% Serf = simple(_),
		% MCakeAcceptance = yes((if CakeSizeSerf >= Serf^cakeAcceptanceThreshold then yes else no))
		% ;
		% Strategy = s(Serf),
		% Serf = complete(CakeAcceptance),
		% MCakeAcceptance = yes(map.lookup(CakeAcceptance, CakeSizeSerf))
	)
	;
	AC = yes(_),
	(
		Strategy = d(_),
		MCakeAcceptance = AC
		;
		% there can only be one serf in the profile
		Strategy = s(_),
		MCakeAcceptance = no
	)
	.

:- func accepts(int, serf) = bool.

accepts(CakeSizeSerf, Serf) = Result :-
	Serf = simple(_),
	Result =
		(if
			CakeSizeSerf >= Serf^cakeAcceptanceThreshold
		then
			yes
		else
			no
		)
	;
	Serf = complete(CakeAcceptance),
	Result = map.lookup(CakeAcceptance, CakeSizeSerf)
	.

:- func payoff(array(strategy), float, float, int) = float.

payoff(Profile, CakeSizeDictator, CakeSizeSerf, Index) = Result :-
	Strategy = array.lookup(Profile, Index),
	(
		Strategy = d(_),
		Result = CakeSizeDictator
		;
		Strategy = s(_),
		Result = CakeSizeSerf
	)
	.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% type class {@code strategy(strategy)} predicates and functions

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% type class {@code chromosome(strategy,unit,parameters,ac)} predicates and functions

/**
 * The result of developing an Ultimatum chromosome is nothing.  This is
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
	Strategy = d(dictator(CakeDivision)),
	AC1 = 'cakeDivisionSum :='(AC, AC^cakeDivisionSum + CakeDivision),
	Result = 'qtyDictator :='(AC1, AC^qtyDictator + 1)
	;
	Strategy = s(Serf),
	Serf = simple(CakeAcceptanceThreshold),
	AC1 = 'cakeAcceptanceThresholdSum :='(AC, AC^cakeAcceptanceThresholdSum + CakeAcceptanceThreshold),
	Result = 'qtySerfSimple :='(AC1, AC^qtySerfSimple + 1)
	;
	Strategy = s(Serf),
	Serf = complete(_CakeAcceptance),
	throw("gl.ultimatum.fold/2: not implemented")
	% AC1 = 'cakeAcceptanceSum :='(AC, map.foldl(Update, CakeAcceptance, AC^cakeAcceptanceSum)),
	% Result = 'qtySerf :='(AC1, AC^qtySerf + 1),
	% % higher-order term
	% Update =
	% (func(K, V, Map) = R :-
	% 	(if
	% 		map.search(Map, {K, V}, Q)
	% 	then
	% 		R = map.det_update(Map, {K, V}, Q + 1)
	% 	else
	% 		R = map.det_insert(Map, {K, V}, 1)
	% 	)
	% )
	.

/**
 * fold = Result
  
 * Returns the initial value of the accumulator used to reduce a collection
 * of PGP strategies.
 */

:- func fold = ac.

fold = Result :-
	Result^cakeDivisionSum = 0,
	Result^qtyDictator = 0,
	Result^cakeAcceptanceThresholdSum = 0,
	Result^qtySerfSimple = 0.



:- end_module gl.ultimatum.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
