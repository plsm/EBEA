/**
 * 

 * @author Pedro Mariano
 * @version 1.0 2013/12/ 5
 */
:- module gl.'2x2'.game.

:- interface.

/**
 * Represents a 2-player game with two actions.  The payoff matrix has two parameters:
 * [ 1 S ]
 * [ T 0 ]
  
 * Parameter S ranges from -1 to 1, while parameter T ranges from 0 to 2.
 * Parameter S is the sucker's payoff, while T is the temptation payoff.
 */

:- type game --->
	game(
		temptation :: float,
		sucker     :: float
	).

:- instance parseable(game).

/**
 * Return a default game.  This can be used to construct a default batch
 * run, a default value for a user interface
 */

:- func default = game.

/**
 * initGame(Temptation, Sucker) = Result
  
 * Return an instance of a 2-player game with two actions.  Throws an
 * exception if parameter {@code Temptation} is outside interval {@code
 * [0,2]} or parameter {@code Sucker} is outside interval {@code [-1,1]}.
 */
:- func initGame(float, float) = game.

:- func lowestPayoff(game) = float.

:- func highestPayoff(game) = float.

:- func paretoPayoff(game) = float.

:- func numberPlayers(game) = int.

:- pred errors(game, string).
:- mode errors(in, out) is semidet.

/**
 * Return a user dialog to edit and view a 2x2 game.
 */
:- func dialog = list(dialogItem(game)).

:- func temptation(game) = float.

:- func sucker(game) = float.

:- implementation.

:- import_module exception, float.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

:- instance parseable(game) where
[
	pred(parse/3) is gl.'2x2'.game.parse
].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

default = game(1.0, 0.0).

initGame(Temptation, Sucker) = Result :-
	(if
		Temptation >= 0.0,
		Temptation =< 2.0,
		Sucker >= -1.0,
		Sucker =< 1.0
	then
		Result = game(Temptation, Sucker)
%		Result^temptation = Temptation,
%		Result^sucker = Sucker
	else
		throw("gl.2x2.initGame/2: Parameters are outside their interval")
	).

lowestPayoff(Game) = float.min(0.0, Game^sucker).

highestPayoff(Game) = float.max(1.0, Game^temptation).

paretoPayoff(Game) = float.max(1.0, (Game^temptation + Game^sucker) / 2.0).

numberPlayers(_) = 2.


errors(Game, Msg) :-
	(if
		Game^temptation > 2.0
		;
		Game^temptation < 0.0
	then
		MsgTemp = "Temptation payoff must be between zero and two"
	else
		MsgTemp = ""
	),
	(if
		Game^sucker > 1.0
		;
		Game^sucker < -1.0
	then
		MsgSuck = "Sucker payoff must be between minus one and one"
	else
		MsgSuck = ""
	),
	(
		MsgTemp \= ""
		;
		MsgSuck \= ""
	),
	Msg = MsgTemp ++ " " ++ MsgSuck
	.

dialog =
	[
	di(label("temptation payoff"), updateFieldFloat( temptation, setTemptation)),
	di(label("sucker payoff"),     updateFieldFloat( sucker,     setSucker))
	].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- pred parse(game, list(int), list(int)).
:- mode parse(in, out, in) is det.
:- mode parse(out, in, out) is semidet.

parse(P) -->
	{P = game(Temptation, Sucker)},
	parseable.float32(Temptation),
	parseable.float32(Sucker)
	.

:- func setTemptation(game, float) = setResult(game).

setTemptation(Game, Temptation) = Result :-
	(if
		Temptation =< 2.0,
		Temptation >= 0.0
	then
		Result = ok('temptation :='(Game, Temptation))
	else
		Result = error("Temptation payoff must be between zero and two")
	).

:- func setSucker(game, float) = setResult(game).

setSucker(Game, Sucker) = Result :-
	(if
		Sucker =< 1.0,
		Sucker >= -1.0
	then
		Result = ok('sucker :='(Game, Sucker))
	else
		Result = error("Sucker payoff must be between minus one and one")
	).

:- end_module gl.'2x2'.game.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
