/**
 * 

 * @author Pedro Mariano
 * @version 1.0 2013/12/12
 */
:- module gl.pgp.game.

:- interface.

:- import_module parseable.

/**
 * The parameters of a Public Good Provision game are the number of
 * players, the good value and the provision cost.
 */

:- type game --->
	pgp(
		players       :: int,
		good          :: float,
		provisionCost :: float
	).

:- instance parseable(game).

:- pred init(int, float, float, game).
:- mode init(in, in, in, out) is semidet.

:- func init(int, float, float) = game.

:- func default = game.

/**
 * The lowest payoff obtained in the PGP game is obtained by the cooperator
 * when there is a single contributor to the good.

 * <p> Function of the type class {@code game(game,pgpStrategy)}.
 */

:- func lowestPayoff(game) = float.

/**
 * The highest payoff obtained in the PGP game is obtained by the defector
 * when all but one player contribute to the good.

 * <p> Function of the type class {@code game(game pgpStrategy)}.
 */
:- func highestPayoff(game) = float.

/**
 * The pareto payoff obtained in the PGP game is obtained when all players
 * contribute to the good.

 * <p> Function of the type class {@code game(game pgpStrategy)}.
 */
:- func paretoPayoff(game) = float.

% /**
%  * numberPlayers(Game) = Result
 
%  * Return the value of attribute {@code players} of {@code Game}.

%  * <p> Function of the type class {@code game(game pgpStrategy)}.
%  */
% :- func numberPlayers(game) = int.

:- func dialog = list(dialogItem(game)).

% :- func players(game) = int.

% :- func good(game) = float.

% :- func provisionCost(game) = float.


:- implementation.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

:- instance parseable(game) where
[
	pred(parse/3) is gl.pgp.game.parse
].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

init(Players, Good, ProvisionCost, Game) :-
	Players > 0,
	ProvisionCost > 0.0,
	Good > ProvisionCost,
	Good - ProvisionCost < float(Players - 1) * Good / float(Players),
	Game = pgp(Players, Good, ProvisionCost).

init(Players, Good, ProvisionCost) = Result :-
	(if
		init(Players, Good, ProvisionCost, Game)
	then
		Result = Game
	else
		throw("gl.pgp.game.init/3: invalid parameters")
	).

default = init( 3, 1.0, 0.5).

%:- pragma memo(lowestPayoff/1).

lowestPayoff(Game) = Game^good / float(Game^players) - Game^provisionCost.

%:- pragma memo(highestPayoff/1).

highestPayoff(Game) = float(Game^players - 1) * Game^good / float(Game^players).

%:- pragma memo(paretoPayoff/1).

paretoPayoff(Game) = Game^good - Game^provisionCost.

% numberPlayers(Game) = Game^players.

dialog =
	[
	di(label("number of players"),  updateFieldInt(      get_players,        set_players)),
	di(label("good"),               updateFieldFloat(    get_good,           set_good)),
	di(label("provision cost"),     updateFieldFloat(    get_provisionCost,  set_provisionCost))
	].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions


:- pred parse(game, list(int), list(int)).
:- mode parse(in, out, in) is det.
:- mode parse(out, in, out) is semidet.

parse(P) -->
	{P = pgp(NumberPlayers, Good, ProvisionCost)},
	[NumberPlayers],
	parseable.float32(Good),
	parseable.float32(ProvisionCost)
	.

:- func get_players(game) = int.

get_players(P) = P^players.


:- func set_players(game, int) = setResult(game).

set_players(P, V) =
	(if
		init(V, P^good, P^provisionCost, G)
	then
		ok(G)
	else
		error("Invalid combination of game parameters")
	).



:- func get_good(game) = float.

get_good(P) = P^good.


:- func set_good(game, float) = setResult(game).

set_good(P, V) =
	(if
		init(P^players, V, P^provisionCost, G)
	then
		ok(G)
	else
		error("Invalid combination of game parameters")
	).



:- func get_provisionCost(game) = float.

get_provisionCost(P) = P^provisionCost.


:- func set_provisionCost(game, float) = setResult(game).

set_provisionCost(P, V) =
	(if
		init(P^players, P^good, V, G)
	then
		ok(G)
	else
		error("Invalid combination of game parameters")
	).

:- end_module gl.pgp.game.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
