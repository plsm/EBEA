/**
 * 

 * @author Pedro Mariano
 * @version 1.0 2013/12/13
 */
:- module gl.ultimatum.game.

:- interface.

:- type game --->
	game(cakeSize::int).

:- instance parseable(game).

/**
 * Return a default Ultimatum {@code game} that can be used to run games.
 */

:- func default = game.

:- func init(int) = game.

:- pred init(int, game).
:- mode init(in, out) is semidet.

:- pred errors(game, string).
:- mode errors(in, out) is nondet.

/**
 * The lowest payoff obtained in the Ultimatum game is zero. It can be
 * obtained when the dictator gives zero to himself or to the serf, or the
 * serf does not accept a division.

 * <p> Function of the type class {@code game(game,strategy)}.
 */

:- func lowestPayoff(game) = float.

/**
 * highestPayoff(Game) = Result

 * The highest payoff obtained in the Ultimatum game is the cake size.  It
 * can be obtained when the dictator gives the entire cake to himself or to
 * the serf and the serf accepts this division.

 * <p> Function of the type class {@code game(game, strategy)}.
 */
:- func highestPayoff(game) = float.

/**
 * paretoPayoff(Game) = Result

 * The Pareto payoff obtained in the Ultimatum game is half the cake size.
 * It is obtained when the dictator divides the cake in half and the serf
 * accepts this division.

 * <p> Function of the type class {@code game(game,strategy)}.
 */
:- func paretoPayoff(game) = float.

/**
 * numberPlayers(Game) = Result
 
 * Return the value of attribute {@code players} of {@code Game}.

 * <p> Function of the type class {@code game(game,strategy)}.
 */
:- func numberPlayers(game) = int.

:- func dialog = list(dialogItem(game)).

:- func cakeSize(game) = int.
:- func 'cakeSize :='(game, int) = game.


:- implementation.

:- import_module float, int.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

:- instance parseable(game) where
[
	pred(parse/3) is gl.ultimatum.game.parse
].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

default = init(10).

init(CakeSize) =
	(if
		init(CakeSize, Game)
	then
		Game
	else
		throw("gl.ultimatum.game.init/1: invalid parameter")
	).

init(CakeSize, Game) :-
	Game = game(CakeSize),
	CakeSize >= 2,
	CakeSize =< 255.

errors(game(CakeSize), "cake size must be greater or equal to two") :-
	CakeSize >= 2.

errors(game(CakeSize), "cake size must be less or equal to 255") :-
	CakeSize =< 255.

:- pragma memo(lowestPayoff/1).

lowestPayoff(_Game) = 0.0.

:- pragma memo(highestPayoff/1).

highestPayoff(Game) = float(Game^cakeSize).

:- pragma memo(paretoPayoff/1).

paretoPayoff(Game) = float(Game^cakeSize / 2).

numberPlayers(_Game) = 2.


dialog =
	[di(label("cake size"),  updateFieldInt(  cakeSize,  checkInt("cake size", bounded(2, yes), bounded(255, yes), 'cakeSize :=')))
	].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- pred parse(game, list(int), list(int)).
:- mode parse(in, out, in) is det.
:- mode parse(out, in, out) is semidet.

parse(G) -->
	{G = game(CakeSize)},
	[CakeSize]
	.

:- end_module gl.ultimatum.game.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
