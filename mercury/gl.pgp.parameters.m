/**
 * 

 * @author Pedro Mariano
 * @version 1.0 2013/12/12
 */
:- module gl.pgp.parameters.

:- interface.


/**
 * Parameters that control strategy mutation.
 */
:- type parameters --->
	parameters(
		stdev::float
	).

:- instance parseable(parameters).

:- func default = parameters.

:- func dialog = list(dialogItem(parameters)).

:- func stdev(parameters) = float.


:- pred errors(parameters, string).
:- mode errors(in, out) is semidet.


:- implementation.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

:- instance parseable(parameters) where
[
	pred(parse/3) is gl.pgp.parameters.parse
].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

default = parameters(0.1).

dialog =
	[di(label("standard deviation Gaussian noise"), updateFieldFloat(  stdev, setStddev))].

errors(Parameters, Msg) :-
	Parameters^stdev < 0.0,
	Msg = "standard deviation of Gaussian noise must be positive"
	.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions


:- pred parse(parameters, list(int), list(int)).
:- mode parse(in, out, in) is det.
:- mode parse(out, in, out) is semidet.

parse(P) -->
	{P = parameters(StdDev)},
	parseable.float32(StdDev)
	.

:- func setStddev(parameters, float) = setResult(parameters).

setStddev(_Parameters, Stdev) = Result :-
	(if
		Stdev >= 0.0
	then
		Result = ok(parameters(Stdev))
	else
		Result = error("standard deviation must be greater than or equal to zero")
	).

:- end_module gl.pgp.parameters.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
