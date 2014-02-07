/**
 * Template module that is used to implement some game to be used in Energy
 * Based Evolutionary Algorithm.

 * @author Pedro Mariano
 * @version 1.0 2013/05/29
 */
:- module gl.template.

:- interface.

:- import_module chromosome, game, scanable, printable, foldable.
:- import_module io, unit.

/**
 * The game.
 */
:- type game.

/**
 * The strategy to be used in a game.
 */
:- type strategy.

/**
 * Parameters that control the mutation operator.
 */
:- type parameters.

/**
 * The accumulator used to reduce a collection of strategies.
 */
:- type ac.

:- instance game(game, strategy).

:- instance chromosome(strategy, unit, parameters).

:- instance foldable(strategy, ac).

:- instance scanable(game).

:- instance scanable(strategy).

:- instance scanable(parameters).

:- instance printable(strategy).

:- instance printable(ac).

/**
 * scanGame(Stream, IMGame, !IO)

 * Scan a game from the given text stream.

  
 */
:- pred scanGame(io.input_stream, scanable.result(game), io.state, io.state).
:- mode scanGame(in, out, di, uo) is det.

:- implementation.

:- import_module rng, rng.distribution.
:- import_module array, bool, exception.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

:- type game --->
	game(
		players :: int
	).

:- type strategy --->
	prob(
		probability :: float
	) ;
	dete(
		action :: bool
	).

:- type parameters --->
	parameters(
		stdDev :: float
	).

:- type ac --->
	ac(
		qty  :: int,
		prob :: float
	).

:- instance game(game, strategy)
	where
[
	func(lowestPayoff/1)   is gl.template.lowestPayoff,
	func(highestPayoff/1)  is gl.template.highestPayoff,
	func(paretoPayoff/1)   is gl.template.paretoPayoff,
	func(numberPlayers/1)  is gl.template.numberPlayers,
	pred(play/5)           is gl.template.play
].

:- instance chromosome(strategy, unit, parameters)
	where
[
	func(numberGenes/1)  is gl.template.numberGenes,
	pred(mutateGene/8)   is gl.template.mutateGene,
	func(born/2)         is gl.template.born
].

:- instance foldable(strategy, ac)
	where
[
	func(fold/2)    is gl.template.fold,
	func(initAC/0)  is gl.template.initAC
].

:- instance scanable(game)
	where
[
	pred(scan/4) is scanGame
].

:- instance scanable(strategy)
	where
[
	pred(scan/4) is scanStrategy
].

:- instance scanable(parameters)
	where
[
	pred(scan/4) is scanParameters
].

:- instance printable(strategy)
	where
[
	pred(print/4) is printStrategy
].

:- instance printable(ac)
	where
[
	pred(print/4) is printAC
].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

scanGame(Stream, IMGame, !IO) :-
	throw("not implemented").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

/**
 * numberPlayers(Game) = Result.
  
 * Return the number of players in the given game.
 
 * <p> This function is part of type class {@code game(game,strategy)}.
  
 */
:- func numberPlayers(game) = int.

numberPlayers(Game) = Result :-
	throw("not implemented").

/**
 * lowestPayoff(Game) = Result
  
 * Return the lowest payoff that can be obtained in the given game.
  
 * <p> This function is part of type class {@code game(game,strategy)}.
  
 */
:- func lowestPayoff(game) = float.

lowestPayoff(Game) = Result :-
	throw("not implemented").

/**
 * highestPayoff(Game) = Result

 * Return the highest payoff that can be obtained in the given game.
  
 * <p> This function is part of type class {@code game(game,strategy)}.
  
 */
:- func highestPayoff(game) = float.

highestPayoff(Game) = Result :-
	throw("not implemented").

/**
 * paretoPayoff(Game) = Result

 * Return the Pareto payoff that can be obtained in the given game.
  
 * <p> This function is part of type class {@code game(game,strategy)}.
  
 */
:- func paretoPayoff(game) = float.

paretoPayoff(Game) = Result :-
	throw("not implemented").

/**
 * play(Game, Profile, !Random, Payoff)

 * Play the given game between the given strategy profile.  The
 * pseudo-random number generator is used by stochastic strategies.  Unify
 * parameter {@code Payoff} with the payoffs obtained by the strategies.
  
 * <p> This predicate is part of type class {@code game(game,strategy)}.
 */

:- pred play(game, profile(strategy), R, R, payoffVector)
	<= ePRNG(R).
:- mode play(in, in, in, out, out).

play(Game, Profile, !Random, Payoff) :-
	throw("not implemented").

%

/**
 * numberGenes(Strategy) = Result
  
 * Return the number of genes used by the given strategy.  Genes are the
 * unit subject to mutation which is performed by predicate {@code
 * mutateGene}.

 * <p> This function is part of type class {@code
 * chromosome(strategy,unit,parameters)}.

 * @see mutateGene/8
 */
:- func numberGenes(strategy) = int.

numberGenes(Strategy) = Result :-
	throw("not implemented").

/**
 * mutateGene(Parameters, Index, !Distribution, !Random, Strategy, Result)
 
 * Mutate one gene of the given strategy.  The parameters, probabilistic
 * distributions and pseudo-random number generators can be used by the
 * mutation operator.

 * <p> This predicate is part of type class {@code
 * chromosome(strategy,unit,parameters)}.
 */

:- pred mutateGene(parameters, int, distribution, distribution, R, R, strategy, strategy)
	<= ePRNG(R).
:- mode mutateGene(in, in, in, out, in, out, in, out) is det.

mutateGene(Parameters, Index, !Distribution, !Random, Strategy, Result) :-
	throw("not implemented").

/**
 * Return the phenotype of the given strategy/chromosome.

 * <p> This function is part of type class {@code
 * chromosome(strategy,unit,parameters)}.

 */
:- func born(parameters, strategy) = unit.

born(_, _) = unit.

%

/**
 * Reduce the given strategy with the accumulator and return the new value
 * of the accumulator
  
 * <p> This function is part of type class {@code foldable(strategy,ac)}.
  
 */
:- func fold(strategy, ac) = ac.

fold(Strategy, AC) = Result :-
	throw("not implemented").

/**
 * init_AC = Result

 * Return the initial value of the accumulator used to reduce a collection
 * of strategies.
  
 * <p> This function is part of type class {@code foldable(strategy,ac)}.
  
 */
:- func initAC = ac.

initAC = Result :-
	throw("not implemented").

%

/**
 * scanStrategy(Stream, IMStrategy, !IO)

 * Scan a strategy from the given text stream.
  
 */
:- pred scanStrategy(io.input_stream, scanable.result(strategy), io.state, io.state).
:- mode scanStrategy(in, out, di, uo) is det.

scanStrategy(Stream, IMStrategy, !IO) :-
	throw("not implemented").

/**
 * scanParameters(Stream, IMParameters, !IO)

 * Scan a parameters from the given text stream.
  
 */
:- pred scanParameters(io.input_stream, scanable.result(parameters), io.state, io.state).
:- mode scanParameters(in, out, di, uo) is det.

scanParameters(Stream, IMParameters, !IO) :-
	throw("not implemented").

/**
 * printStrategy(Stream, Strategy, !IO)

 * Print a strategy to the given text stream.
  
 */
:- pred printStrategy(io.output_stream, strategy, io.state, io.state).
:- mode printStrategy(in, in, di, uo) is det.

printStrategy(Stream, Strategy, !IO) :-
	throw("not implemented").

/**
 * printAC(Stream, AC, !IO)

 * Print an accumulator to the given text stream.
  
 */
:- pred printAC(io.output_stream, ac, io.state, io.state).
:- mode printAC(in, in, di, uo) is det.

printAC(Stream, AC, !IO) :-
	throw("not implemented").


:- end_module gl.template.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
