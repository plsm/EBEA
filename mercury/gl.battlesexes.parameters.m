/**
 * 

 * @author Pedro Mariano
 * @version 1.0 2014/01/13
 */
:- module gl.battlesexes.parameters.

:- interface.

:- import_module fraction.

:- import_module userInterface.

:- import_module parseable.

:- type parameters --->
	parameters(
		stddev :: fraction
	) .

:- instance parseable(parameters).

/**
 * Return a default value of {@code parameters}.
 */
:- func default = parameters.

:- func dialog = list(dialogItem(parameters)).


:- implementation.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

:- instance parseable(parameters) where
[
	pred(parse/3) is gl.battlesexes.parameters.parse
].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

default = Result :-
	Result^stddev = default_stddev.

dialog =
	[
	di(label("stddev"),  fraction.dialogAction(    get_stddev,  set(set_stddev)))
	].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- func default_stddev = fraction.

default_stddev = fraction(1, 2).

:- func get_stddev(parameters) = fraction.

get_stddev(P) = P^stddev.


:- func set_stddev(parameters, fraction) = parameters.

set_stddev(P, V) = 'stddev :='(P, V).



:- pred parse(parameters, list(int), list(int)).
:- mode parse(in, out, in) is det.
:- mode parse(out, in, out) is semidet.

parse(P) -->
	{P = parameters(_)},
	fraction.parse(P^stddev)
	.

:- end_module gl.battlesexes.parameters.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:

