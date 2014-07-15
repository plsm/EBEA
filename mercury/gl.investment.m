/**
 * In the Public Investment game players have the option to invest a fixed
 * amount of money into a pot.  The pot is then multiplied by a factor and
 * the result is divided among all participants.  The investment decision
 * is a binary action, in that the player either invests all his money or
 * keeps it with him.  The player's initial money is fixed and set to one.
 * The game parameters are the number of players and the multiplying
 * factor.

 * @author Pedro Mariano
 * @version 1.0 2013/05/29
 * @version 2.0 2013/12/30
 */
:- module gl.investment.

:- interface.

:- include_module game, strategy, parameter.
:- import_module gl.investment.game, gl.investment.strategy, gl.investment.parameter.
:- import_module probability.
:- import_module bool, io, unit.

:- type ac --->
	ac(
		qtyProb    :: int,
		sumProb    :: float,
		qtyDeteYes :: int,
		qtyDeteNo  :: int
	).

% :- type factory.

:- instance abstractGame(game).
:- instance symmetricGame(game, strategy).
:- instance asymmetricGame(game, strategy).

:- instance chromosome(strategy, unit, parameter).

% :- instance factory(factory, game, strategy, parameter).

:- instance foldable(strategy, ac).

:- instance printable(ac).

% :- func defaultFactory = factory.

:- implementation.

:- import_module rng, rng.distribution.

:- import_module array, exception, float, int, list, maybe, string.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types


% :- type factory --->
% 	factory(
% 		vPlayers           :: list(int),
% 		vFactor            :: list(float),
% 		vInvestProbability :: list(float),
% 		vProbabilityQty    :: list(int),
% 		vInvestAction      :: list(bool),
% 		vActionQty         :: list(int),
% 		vStddev            :: list(float)
% 	).

:- instance abstractGame(game)
	where
[
	func(lowestPayoff/1)   is gl.investment.game.lowestPayoff,
	func(highestPayoff/1)  is gl.investment.game.highestPayoff,
	func(paretoPayoff/1)   is gl.investment.game.paretoPayoff,
	func(numberPlayers/1)  is gl.investment.game.numberPlayers
].

:- instance symmetricGame(game, strategy)
	where
[
	pred(playSymmetric/5)  is gl.investment.play
].

:- instance asymmetricGame(game, strategy) where
[
	func(numberRoles/1)    is game.singleRole,
	pred(playAsymmetric/5) is game.playSymmetricBridge
].

:- instance chromosome(strategy, unit, parameter)
	where
[
	func(numberGenes/1)  is gl.investment.strategy.numberGenes,
	pred(mutateGene/8)   is gl.investment.strategy.mutateGene,
	func(born/2)         is gl.investment.born
].

% :- instance factory(factory, game, strategy, parameter) where
% [
% 	pred(value/5) is valueFactory
% ].

:- instance foldable(strategy, ac)
	where
[
	func(fold/2)    is gl.investment.fold,
	func(initAC/0)  is gl.investment.initAC
].

:- instance printable(ac)
	where
[
	pred(print/4) is printAC
].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

:- type stype --->
	prob ;
	dete.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

% defaultFactory =
% 	factory(
% 	[3],
% 	[2.0],
% 	[0.5],
% 	[10],
% 	[yes],
% 	[10],
% 	[0.1]
% 	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

% :- pred valueFactory(factory, string, game, list({int, strategy}), parameter).
% :- mode valueFactory(in, out, out, out, out) is nondet.

% valueFactory(Factory, Key, Game, StrategyQuantities, Parameters) :-
% 	list.member(Players,  Factory^vPlayers),
% 	list.member(Factor,   Factory^vFactor),
% 	initGame(Players, Factor, Game),
% 	(
% 		Key = string.format("%d %f %f %d %f",
% 			[i(Players), f(Factor),
% 			 f(InvestProbability), i(ProbabilityQty),
% 			 f(Stddev)]),
% 		list.member(InvestProbability,  Factory^vInvestProbability),
% 		list.member(ProbabilityQty,     Factory^vProbabilityQty),
% 		initStochasticStrategy(InvestProbability, StochasticStrategy),
% 		StrategyQuantities = [{ProbabilityQty, StochasticStrategy}]
% 		;
% 		Key = string.format("%d %f %s %d %f",
% 			[i(Players), f(Factor),
% 			 s(string(InvestAction)), i(ActionQty),
% 			 f(Stddev)]),
% 		list.member(InvestAction, Factory^vInvestAction),
% 		list.member(ActionQty,    Factory^vActionQty),
% 		initDeterministicStrategy(InvestAction) = DeterministicStrategy,
% 		StrategyQuantities = [{ActionQty, DeterministicStrategy}]
% 	),
% 	list.member(Stddev,               Factory^vStddev),
% 	Parameters = parameters(Stddev).

/**
 * play(Game, Profile, !Random, Payoff)

 * Play the given game between the given strategy profile.  The
 * pseudo-random number generator is used by stochastic strategies.  Unify
 * parameter {@code Payoff} with the payoffs obtained by the strategies.
  
 */
:- pred play(game, array(strategy), R, R, array(float))
	<= ePRNG(R).
:- mode play(in, in, in, out, out) is det.

play(Game, Profile, !Random, Payoff) :-
	array.map_foldl(playerAction, Profile, Actions, !Random),
	NumberContributors = array.foldl(numberContributors, Actions, 0.0),
	array.map(playerPayoff(NumberContributors * Game^factor / float(Game^players)), Actions) = Payoff.

:- pred playerAction(strategy, action, R, R)
	<= ePRNG(R).
:- mode playerAction(in, out, in, out) is det.

playerAction(Strategy, Action, !Random) :-
	Strategy = prob(Probability),
	probability.flipCoin(Probability, Outcome, !Random),
	(
		Outcome = yes,
		Action = contribute
		;
		Outcome = no,
		Action = exploit
	)
	;
	Strategy = dete(Outcome),
	(
		Outcome = yes,
		Action = contribute
		;
		Outcome = no,
		Action = exploit
	)
	.

:- type action --->
	contribute ;
	exploit.

:- func numberContributors(action, float) = float.

numberContributors(contribute, AC) = AC + 1.0.
numberContributors(exploit, AC) = AC.

:- func playerPayoff(float, action) = float.

playerPayoff(PotShare, Action) = Result :-
	Action = contribute,
	Result = PotShare - 1.0
	;
	Action = exploit,
	Result = PotShare
	.



%

/**
 * Return the phenotype of the given strategy/chromosome.
 */
:- func born(parameter, strategy) = unit.

born(_, _) = unit.

%

/**
 * Reduce the given strategy with the accumulator and return the new value
 * of the accumulator
  
 * <p> This function is part of type class {@code foldable(strategy,ac)}.
  
 */
:- func fold(strategy, ac) = ac.

fold(Strategy, AC) = Result :-
	Strategy = prob(InvestProbability),
	X = 'sumProb :='(AC, AC^sumProb + float(InvestProbability)),
	Result = 'qtyProb :='(X, AC^qtyProb + 1)
	;
	Strategy = dete(yes),
	Result = 'qtyDeteYes :='(AC, AC^qtyDeteYes + 1)
	;
	Strategy = dete(no),
	Result = 'qtyDeteNo :='(AC, AC^qtyDeteNo + 1)
	.

/**
 * init_AC = Result

 * Return the initial value of the accumulator used to reduce a collection
 * of strategies.
  
 * <p> This function is part of type class {@code foldable(strategy,ac)}.
  
 */
:- func initAC = ac.

initAC = Result :-
	Result^qtyProb = 0,
	Result^sumProb = 0.0,
	Result^qtyDeteYes = 0,
	Result^qtyDeteNo = 0.

%


/**
 * printAC(Stream, AC, !IO)

 * Print an accumulator to the given text stream.
  
 */
:- pred printAC(io.output_stream, ac, io.state, io.state).
:- mode printAC(in, in, di, uo) is det.

printAC(Stream, AC, !IO) :-
	io.print(Stream, AC^qtyProb, !IO),
	io.print(Stream, " ", !IO),
	io.print(Stream, AC^sumProb, !IO),
	io.print(Stream, " ", !IO),
	io.print(Stream, AC^qtyDeteYes, !IO),
	io.print(Stream, " ", !IO),
	io.print(Stream, AC^qtyDeteNo, !IO).

:- end_module gl.investment.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
