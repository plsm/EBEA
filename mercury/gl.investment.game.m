/**
 * 

 * @author Pedro Mariano
 * @version 1.0 2014/02/06
 */
:- module gl.investment.game.

:- interface.

:- type game --->
	game(
		players :: int ,
		factor  :: float
	) .

:- instance parseable(game).

/**
 * Return a default value of {@code game}.
 */
:- func default = game.

:- func dialog = list(dialogItem(game)).

/**
 * numberPlayers(Game) = Result.
  
 * Return the number of players in the given game.
 
 */
:- func numberPlayers(game) = int.

/**
 * lowestPayoff(Game) = Result
  
 * Return the lowest payoff that can be obtained in the given game.
  
 */
:- func lowestPayoff(game) = float.

/**
 * highestPayoff(Game) = Result

 * Return the highest payoff that can be obtained in the given game.
  
 */
:- func highestPayoff(game) = float.

/**
 * paretoPayoff(Game) = Result

 * Return the Pareto payoff that can be obtained in the given game.
  
 * <p> This function is part of type class {@code game(game,strategy)}.
  
 */
:- func paretoPayoff(game) = float.

:- implementation.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

:- instance parseable(game) where
[
	pred(parse/3) is gl.investment.game.parse
].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

default = Result :-
	Result^players = default_players,
	Result^factor = default_factor.

dialog =
	[
	di(label("number of players"),  updateFieldInt(      get_players,  checkInt(   "players",  bounded(2, yes), unbound, set_players))),
	di(label("multiplying factor"), updateFieldFloat(    get_factor,   checkFloat( "factor",   bounded(1.0, no), unbound, set_factor)))
	].

numberPlayers(Game) = Game^players.

lowestPayoff(Game) = 1.0 / float(Game^players) * Game^factor - 1.0.

highestPayoff(Game) = float(Game^players - 1) / float(Game^players) * Game^factor.

paretoPayoff(Game) = Game^factor - 1.0.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- func default_players = int.

default_players = 2.

:- func default_factor = float.

default_factor = 2.0.

:- func get_players(game) = int.

get_players(P) = P^players.


:- func set_players(game, int) = game.

set_players(P, V) = 'players :='(P, V).



:- func get_factor(game) = float.

get_factor(P) = P^factor.


:- func set_factor(game, float) = game.

set_factor(P, V) = 'factor :='(P, V).



:- pred parse(game, list(int), list(int)).
:- mode parse(in, out, in) is det.
:- mode parse(out, in, out) is semidet.

parse(P) -->
	{P = game(_, _)},
	parseable.int32(P^players),
	parseable.float32(P^factor)
	.

:- end_module gl.investment.game.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:

