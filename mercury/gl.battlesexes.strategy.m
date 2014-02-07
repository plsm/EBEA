/**
 * 

 * @author Pedro Mariano
 * @version 1.0 2014/01/13
 */
:- module gl.battlesexes.strategy.

:- interface.

:- import_module probability.

:- import_module userInterface.

:- import_module parseable.

:- type strategy --->
	male(
		probabilityRugby :: probability
	) ;
	female(
		probabilityOpera :: probability
	) .

:- instance parseable(strategy).

/**
 * Return a default value of {@code strategy}.
 */
:- func default = strategy.

:- func dialog = list(dialogItem(strategy)).

/**
 * Returns value {@code 1}.  The strategy of the Battle of Sexes game only
 * contains one parameter: the probability to go to the favourite event.

 * <p> This function is part of type class {@code
 * chromosome(strategy,strategy)}.
 */

:- func numberParameters(strategy) = int.

/**
 * mutateGene(Parameters, Index, !Distribution, !Random, Strategy, Result)
 
 * Mutate the single gene of a Public Good Provision chromosome.  Currently
 * we sum an uniform random variable.

 * <p> This predicate is part of type class {@code chromosome(strategy,strategy)}.
 *
 */

:- pred mutateGene(parameters, int, distribution, distribution, R, R, strategy, strategy)
	<= ePRNG(R).
:- mode mutateGene(in, in, in, out, in, out, in, out) is det.

/**
 * Print this strategy to the given text stream.
  
 * <p> This predicate is part of type class {@code print(strategy)}.

 */
:- pred print(io.output_stream, strategy, io, io).
:- mode print(in, in, di, uo) is det.


:- implementation.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

:- instance parseable(strategy) where
[
	pred(parse/3) is gl.battlesexes.strategy.parse
].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

default = Result :-
	Result^probabilityRugby = default_probabilityRugby.

dialog =
	[
	di(label("male"),    subdialog( [
		di(label("probability go to rugby"),  probability.dialogAction( get_probabilityRugby,  set(set_probabilityRugby)))
		])),
	di(label("female"),  subdialog( [
		di(label("probability go to opera"),  probability.dialogAction( get_probabilityOpera,  set(set_probabilityOpera)))
		]))
	].

numberParameters(female(_)) = 1.
numberParameters(male(_)) = 1.

mutateGene(Parameters, Index, !Distribution, !Random, Strategy, Result) :-
	(if
		Index = 0
	then
		Strategy = female(P),
		probability.addGaussianNoise(float(Parameters^stddev), P, R, !Distribution, !Random),
		Result = female(R)
		;
		Strategy = male(P),
		probability.addGaussianNoise(float(Parameters^stddev), P, R, !Distribution, !Random),
		Result = male(R)
	else
		throw("gl.battlesexes.mutateGene/5: Invalid gene index")
	).

print(Stream, female(P), !IO) :-
	io.print(Stream, "f\t", !IO),
	probability.print(Stream, P, !IO).

print(Stream, male(P), !IO) :-
	io.print(Stream, "m\t", !IO),
	probability.print(Stream, P, !IO).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- func default_probabilityRugby = probability.

default_probabilityRugby = probability.one.

:- func default_probabilityOpera = probability.

default_probabilityOpera = probability.zero.

:- func get_probabilityRugby(strategy) = probability.

get_probabilityRugby(P) = R :-
	P = male(_),
	R = P^probabilityRugby
	;
	P = female(_),
	R = default_probabilityRugby
	.

:- func set_probabilityRugby(strategy, probability) = strategy.

set_probabilityRugby(P, V) = R :-
	P = male(_),
	R = 'probabilityRugby :='(P, V)
	;
	P = female(_),
	R = male(V)
	.


:- func get_probabilityOpera(strategy) = probability.

get_probabilityOpera(P) = R :-
	P = male(_),
	R = default_probabilityOpera
	;
	P = female(_),
	R = P^probabilityOpera
	.

:- func set_probabilityOpera(strategy, probability) = strategy.

set_probabilityOpera(P, V) = R :-
	P = male(_),
	R = female(V)
	;
	P = female(_),
	R = 'probabilityOpera :='(P, V)
	.


:- pred parse(strategy, list(int), list(int)).
:- mode parse(in, out, in) is det.
:- mode parse(out, in, out) is semidet.

parse(P) -->
	{P = male(_)},
	[0],
	probability.parse(P^probabilityRugby)
	.

parse(P) -->
	{P = female(_)},
	[1],
	probability.parse(P^probabilityOpera)
	.

:- end_module gl.battlesexes.strategy.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
