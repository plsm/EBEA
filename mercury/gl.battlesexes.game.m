/**
 * 

 * @author Pedro Mariano
 * @version 1.0 2014/01/13
 */
:- module gl.battlesexes.game.

:- interface.

:- import_module fraction.

:- import_module userInterface.

:- import_module parseable.

:- type game --->
	game(
		payoffDiffPlace        :: fraction ,
		payoffSamePlaceLike    :: fraction ,
		payoffSamePlaceDislike :: fraction
	) .

:- instance parseable(game).

/**
 * Return a default value of {@code game}.
 */
:- func default = game.

:- func dialog = list(dialogItem(game)).

/**
 * The lowest payoff obtained in the Battle of Sexes game is obtained by
 * when both players go to separate events.

 * <p> Function of the type class {@code game(game,pgpStrategy)}.
 */

:- func lowestPayoff(game) = float.

/**
 * The highest payoff obtained in the PGP game is obtained when both
 * players go to the same event and it is equal by the player that likes
 * the event.

 * <p> Function of the type class {@code game(game pgpStrategy)}.
 */
:- func highestPayoff(game) = float.

/**
 * The pareto payoff obtained in the PGP game is obtained when players go
 * together and alternate between the two events.

 * <p> Function of the type class {@code game(game pgpStrategy)}.
 */
:- func paretoPayoff(game) = float.

/**
 * numberPlayers(Game) = Result
 
 * Return the value of attribute {@code players} of {@code Game}.

 * <p> Function of the type class {@code game(game pgpStrategy)}.
 */
:- func numberPlayers(game) = int.



:- implementation.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

:- instance parseable(game) where
[
	pred(parse/3) is gl.battlesexes.game.parse
].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

default = Result :-
	Result^payoffDiffPlace = default_payoffDiffPlace,
	Result^payoffSamePlaceLike = default_payoffSamePlaceLike,
	Result^payoffSamePlaceDislike = default_payoffSamePlaceDislike.

dialog =
	[
	di(label("payoffDiffPlace"),         fraction.dialogAction(    get_payoffDiffPlace,         set(set_payoffDiffPlace))),
	di(label("payoffSamePlaceLike"),     fraction.dialogAction(    get_payoffSamePlaceLike,     set(set_payoffSamePlaceLike))),
	di(label("payoffSamePlaceDislike"),  fraction.dialogAction(    get_payoffSamePlaceDislike,  set(set_payoffSamePlaceDislike)))
	].

:- pragma memo(lowestPayoff/1).

lowestPayoff(Game) = fraction.float(Game^payoffDiffPlace).

:- pragma memo(highestPayoff/1).

highestPayoff(Game) = fraction.float(Game^payoffSamePlaceLike).

:- pragma memo(paretoPayoff/1).

paretoPayoff(Game) = fraction.float(Game^payoffSamePlaceLike + Game^payoffSamePlaceDislike // 2).

numberPlayers(_Game) = 2.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- func default_payoffDiffPlace = fraction.

default_payoffDiffPlace = fraction.zero.

:- func default_payoffSamePlaceLike = fraction.

default_payoffSamePlaceLike = fraction.one.

:- func default_payoffSamePlaceDislike = fraction.

default_payoffSamePlaceDislike = fraction(1, 2).

:- func get_payoffDiffPlace(game) = fraction.

get_payoffDiffPlace(P) = P^payoffDiffPlace.


:- func set_payoffDiffPlace(game, fraction) = game.

set_payoffDiffPlace(P, V) = 'payoffDiffPlace :='(P, V).



:- func get_payoffSamePlaceLike(game) = fraction.

get_payoffSamePlaceLike(P) = P^payoffSamePlaceLike.


:- func set_payoffSamePlaceLike(game, fraction) = game.

set_payoffSamePlaceLike(P, V) = 'payoffSamePlaceLike :='(P, V).



:- func get_payoffSamePlaceDislike(game) = fraction.

get_payoffSamePlaceDislike(P) = P^payoffSamePlaceDislike.


:- func set_payoffSamePlaceDislike(game, fraction) = game.

set_payoffSamePlaceDislike(P, V) = 'payoffSamePlaceDislike :='(P, V).



:- pred parse(game, list(int), list(int)).
:- mode parse(in, out, in) is det.
:- mode parse(out, in, out) is semidet.

parse(P) -->
	{P = game(_, _, _)},
	fraction.parse(P^payoffDiffPlace),
	fraction.parse(P^payoffSamePlaceLike),
	fraction.parse(P^payoffSamePlaceDislike)
	.

:- end_module gl.battlesexes.game.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:

