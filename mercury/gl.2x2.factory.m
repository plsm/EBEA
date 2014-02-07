/**
 * 

 * @author Pedro Mariano
 * @version 1.0 2013/12/ 5
 */
:- module gl.'2x2'.factory.

:- interface.

:- type factory --->
	factory(
		vTemptation       :: list(float),
		vSucker           :: list(float),
		vQuantityStrategy :: list(int),
		vFirstProbability :: list(float),
		vFirstAction      :: list(bool),
		vFractionFirstYes :: list(float)
	).

/**
 * Return a default factory that creates strategies and games for a batch run.
 */
:- func default = factory.

:- func dialog = list(dialogItem(factory)).

:- func vTemptation(factory) = list(float).

:- func vSucker(factory) = list(float).

:- func vQuantityStrategy(factory) = list(int).

:- func vFirstProbability(factory) = list(float).

:- func vFirstAction(factory) = list(bool).

:- func vFractionFirstYes(factory) = list(float).

:- implementation.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

default = factory([0.5, 1.5], [-0.5, 0.5], [10], [0.5], [yes, no], [0.5]).

dialog =
	[
	di(label("temptation payoff"),  updateListFieldFloat( vTemptation,       setVTemptation)),
	 di(label("sucker payoff"),      updateListFieldFloat( vSucker,           setVSucker)),
%	 di(label("strategy quantity"),  updateListFieldInt(   vQuantityStrategy, setVQuantityStrategy)),
%	 di(label("first probability"),  updateListFieldFloat( vFirstProbability, setVFirstProbability)),
%	 di(label("first action"),       updateListFieldBool(  vFirstAction,      setVFirstAction)),
	 di(label("fraction first"),     updateListFieldFloat( vFractionFirstYes, setVFractionFirstYes))
	].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- func setVTemptation(factory, list(float)) = maybe_error(factory).

setVTemptation(Factory, VTemptation) = Result :-
	(if
		all [Temptation]
		list.member(Temptation, VTemptation)
		=>
		(
			Temptation =< 2.0,
			Temptation >= 0.0
		)
	then
		Result = ok('vTemptation :='(Factory, VTemptation))
	else
		Result = error("Temptation payoff must be between zero and two")
	).

:- func setVSucker(factory, list(float)) = maybe_error(factory).

setVSucker(Factory, VSucker) = Result :-
	(if
		all [Sucker]
		list.member(Sucker, VSucker)
		=>
		(
			Sucker =< 1.0,
			Sucker >= -1.0
		)
	then
		Result = ok('vSucker :='(Factory, VSucker))
	else
		Result = error("Sucker payoff must be between minus one and one")
	).

:- func setVFractionFirstYes(factory, list(float)) = maybe_error(factory).

setVFractionFirstYes(Factory, VFractionFirstYes) = Result :-
	(if
		all [FractionFirstYes]
		list.member(FractionFirstYes, VFractionFirstYes)
		=>
		(
			FractionFirstYes =< 1.0,
			FractionFirstYes >= 0.0
		)
	then
		Result = ok('vFractionFirstYes :='(Factory, VFractionFirstYes))
	else
		Result = error("Fraction of players playing first action must be between zero and one")
	).


:- end_module gl.'2x2'.factory.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
