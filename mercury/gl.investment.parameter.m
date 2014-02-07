/**
 * 

 * @author Pedro Mariano
 * @version 1.0 2014/02/06
 */
:- module gl.investment.parameter.

:- interface.

:- type parameter --->
	parameter(
		stddev :: float
	) .

:- instance parseable(parameter).

/**
 * Return a default value of {@code parameter}.
 */
:- func default = parameter.

:- func dialog = list(dialogItem(parameter)).


:- implementation.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

:- instance parseable(parameter) where
[
	pred(parse/3) is gl.investment.parameter.parse
].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

default = Result :-
	Result^stddev = default_stddev.

dialog =
	[
	di(label("standard deviation"),  updateFieldFloat(    get_stddev,  checkFloat( "stddev",  bounded(0.0, yes), unbound, set_stddev)))
	].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- func default_stddev = float.

default_stddev = 0.1.

:- func get_stddev(parameter) = float.

get_stddev(P) = P^stddev.


:- func set_stddev(parameter, float) = parameter.

set_stddev(P, V) = 'stddev :='(P, V).



:- pred parse(parameter, list(int), list(int)).
:- mode parse(in, out, in) is det.
:- mode parse(out, in, out) is semidet.

parse(P) -->
	{P = parameter(_)},
	parseable.float32(P^stddev)
	.

:- end_module gl.investment.parameter.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:

