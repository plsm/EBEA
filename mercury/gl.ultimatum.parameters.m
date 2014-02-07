/**
 * 

 * @author Pedro Mariano
 * @version 1.0 2013/12/17
 */
:- module gl.ultimatum.parameters.

:- interface.

:- type parameters --->
	parameters(
		cakeSizeCopy :: int,
		stdev        :: float
	).

:- instance parseable(parameters).

/**
 * Return a default Centipede parameters that can be used to run
 * evolutionary games.
  
 */
:- func default = parameters.

:- func init(int, float) = parameters.

:- pred init(int, float, parameters).
:- mode init(in, in, out) is semidet.

:- func dialog = list(dialogItem(parameters)).

:- implementation.

:- import_module exception.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

:- instance parseable(parameters) where
[
	pred(parse/3) is gl.ultimatum.parameters.parse
].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

default = init(10, 0.1).

init(CakeSize, StdDev) =
	(if
		init(CakeSize, StdDev, P)
	then
		P
	else
		throw("gl.ultimatum.parameters/2: invalid set of parameters")
	).

init(CakeSize, StdDev, Parameters) :-
	CakeSize > 0,
	StdDev >= 0.0,
	Parameters^cakeSizeCopy = CakeSize,
	Parameters^stdev = StdDev.

dialog =
	[
	di(label("cakeSizeCopy"),  updateFieldInt( get_cakeSizeCopy,  checkInt( "cakeSizeCopy",  unbound, unbound, set_cakeSizeCopy))),
	di(label("stdev"),        updateFieldFloat( get_stdev,        checkFloat( "stdev",        unbound, unbound, set_stdev)))
	].
	


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- pred parse(parameters, list(int), list(int)).
:- mode parse(in, out, in) is det.
:- mode parse(out, in, out) is semidet.

parse(P) -->
	{P = parameters(CakeSize, StdDev)},
	[CakeSize],
	parseable.float32(StdDev)
	.

:- func get_cakeSizeCopy(parameters) = int.

get_cakeSizeCopy(P) = P^cakeSizeCopy.


:- func set_cakeSizeCopy(parameters, int) = parameters.

set_cakeSizeCopy(P, V) = 'cakeSizeCopy :='(P, V).



:- func get_stdev(parameters) = float.

get_stdev(P) = P^stdev.


:- func set_stdev(parameters, float) = parameters.

set_stdev(P, V) = 'stdev :='(P, V).


:- end_module gl.ultimatum.parameters.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
