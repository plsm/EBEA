/**
 * 

 * @author Pedro Mariano
 * @version 1.0 2013/12/ 5
 */
:- module gl.'2x2'.strategy.

:- interface.

:- import_module rng, probability.
:- import_module bool.

/**
 * Represents a strategy in a 2x2 game. 
 */
:- type strategy --->
%	prob(firstProbability :: probability) ;
	prob(firstProbability :: float) ;
	dete(firstAction      :: bool).

:- instance printable(strategy).

:- instance parseable(strategy).

:- func default = strategy.

:- func dialog = list(dialogItem(strategy)).

:- pred errors(strategy, string).
:- mode errors(in, out) is semidet.

/**
 * initProbabilisticStrategy(FirstActionProbability) = Result
  
 * Return a stochastic strategy with the given probability to play the
 * first action.  Throws an exception if parameter {@code
 * FirstActionProbability} is outside interval {@code [0,1]}.
 */
:- func initProbabilisticStrategy(float) = strategy.

:- func numberParameters(strategy) = int.

/**
 * action(Strategy, FirstAction, !Random)
  
 * Calculate the action done by a 2x2 strategy.  Parameter {@code
 * FirstAction} is unified with {@code yes} if that strategy plays the
 * first action.
 */

:- pred action(strategy, bool, R, R)
	<= ePRNG(R).
:- mode action(in, out, in, out) is det.

:- implementation.

:- import_module float, list, maybe.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

:- instance printable(strategy)
	where
[
	pred(print/4) is gl.'2x2'.strategy.printStrategy
].

:- instance parseable(strategy)
	where
[
	pred(parse/3) is gl.'2x2'.strategy.parse
].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

default = dete(yes).

dialog =
	[
	di(label("first action probability"), updateFieldFloat( getFirstProbability, setFirstProbability)),
	di(label("perform first action"),     updateFieldBool(  getFirstAction,      setFirstAction))
	].

initProbabilisticStrategy(FirstActionProbability) = Result :-
%	Result^firstProbability = probability.init(FirstActionProbability).
	(if
		FirstActionProbability >= 0.0,
		FirstActionProbability =< 1.0
	then
		Result^firstProbability = FirstActionProbability
	else
		throw("gl.2x2.initProbabilisticStrategy/1: Parameter is outside its interval")
	).

errors(prob(FirstProbability), Message) :-
	(
		FirstProbability < 0.0
		;
		FirstProbability > 1.0
	),
	Message = "Probability must be between zero an one".

action(Strategy, FirstAction, !Random) :-
	Strategy = dete(FirstAction)
	;
	Strategy = prob(FirstProbability),
	rng.flipCoin(FirstProbability, FirstAction, !Random).

numberParameters(_) = 1.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- pred parse(strategy, list(int), list(int)).
:- mode parse(in, out, in) is det.
:- mode parse(out, in, out) is semidet.

parse(prob(FirstProbability)) -->
	[0],
	parseable.float32(FirstProbability).

parse(dete(no)) -->
	[1, 0].

parse(dete(yes)) -->
	[1, 1].

:- func getFirstProbability(strategy) = float.

getFirstProbability(prob(Result)) = Result.
getFirstProbability(dete(yes)) = 1.0.
getFirstProbability(dete(no)) = 0.0.

:- func getFirstAction(strategy) = bool.

getFirstAction(dete(Result)) = Result.
getFirstAction(prob(FirstProbability)) =
	(if
		FirstProbability < 0.5
	then
		no
	else
		yes
	).

:- func setFirstProbability(strategy, float) = setResult(strategy).

setFirstProbability(_, FirstProbability) = Result :-
	(if
		FirstProbability >= 0.0,
		FirstProbability =< 1.0
	then
		Result = ok(prob(FirstProbability))
	else
		Result = error("Probability must be between zero an one")
	).

:- func setFirstAction(strategy, bool) = setResult(strategy).

setFirstAction(_, FirstAction) = ok(dete(FirstAction)).


/**
 * <p> This predicate is part of type classes {@code strategy(strategy)}
 * and {@code chromosome(strategy,unit,parameters,ac)}.
 */

:- pred printStrategy(io.output_stream, strategy, io, io).
:- mode printStrategy(in, in, di, uo) is det.

printStrategy(Stream, Strategy, !IO) :-
	Strategy = prob(FirstProbability),
	io.print(Stream, "p ", !IO),
	io.print(Stream, FirstProbability, !IO)
	;
	Strategy = dete(FirstAction),
	io.print(Stream, "d ", !IO),
	io.print(Stream, FirstAction, !IO)
	.

:- end_module gl.'2x2'.strategy.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
