/**
 * 

 * @author Pedro Mariano
 * @version 1.0 2014/02/06
 */
:- module gl.givetake.parameters.

:- interface.

:- type parameters --->
	parameters(
		timeStdDev :: float ,
		probStdDev :: float ,
		maxTime    :: int
	) .

:- instance parseable(parameters).

/**
 * Return a default value of {@code parameters}.
 */
:- func default = parameters.

:- func dialog = list(dialogItem(parameters)).

:- func timeStdDev(parameters) = float.
:- func probStdDev(parameters) = float.
:- func maxTime(parameters) = int.

:- func 'timeStdDev :='(parameters, float) = parameters.
:- func 'probStdDev :='(parameters, float) = parameters.
:- func 'maxTime :='(parameters, int) = parameters.

:- implementation.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

:- instance parseable(parameters) where
[
	pred(parse/3) is gl.givetake.parameters.parse
].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

default = parameters(default_timeStdDev, default_probStdDev, default_maxTime).

dialog =
	[
	di(label("timeStdDev"),  updateFieldFloat( timeStdDev,  checkFloat( "time Std Dev",  bounded(0.0, yes), unbound, 'timeStdDev :='))),
	di(label("probStdDev"),  updateFieldFloat( probStdDev,  checkFloat( "prob Std Dev",  bounded(0.0, yes), unbound, 'probStdDev :='))),
	di(label("max time"),    updateFieldInt(   maxTime,     checkInt(   "max time",      bounded(0, yes),   bounded(100, yes),   'maxTime :=')))
	].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- func default_timeStdDev = float.

default_timeStdDev = 1.0.

:- func default_probStdDev = float.

default_probStdDev = 0.1.

:- func default_maxTime = int.

default_maxTime = 5.



:- pred parse(parameters, list(int), list(int)).
:- mode parse(in, out, in) is det.
:- mode parse(out, in, out) is semidet.

parse(P) -->
	{P = parameters(TimeStdDev, ProbStdDev, MaxTime)},
	parseable.float32(TimeStdDev),
	parseable.float32(ProbStdDev),
	[MaxTime]
	.

:- end_module gl.givetake.parameters.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:

