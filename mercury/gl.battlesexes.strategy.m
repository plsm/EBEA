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
		probabilityTennis :: probability
	) ;
	female(
		probabilityOpera :: probability
	) ;
	person_pure(
		role_p      :: role,
		goFavourite :: bool
	) ;
	person_mixed(
		role_m               :: role,
		probabilityFavourite :: probability
	).

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
	Result^probabilityTennis = default_probabilityTennis.

dialog =
	[di(label("strategy chromosome"),  selectOneOf(
		getCurrentChoice,
		setChoice,
		[
		ci(label("female"),
			[
			di(label("probability go to opera"),  probability.dialogAction( get_probabilityOpera,  set(set_probabilityOpera)))
			]),
		ci(label("male"),
			[
			di(label("probability go to tennis"),  probability.dialogAction( get_probabilityTennis,  set(set_probabilityTennis)))
			]),
		ci(label("mixed person"),
			[
			di(label("male"),                         updateFieldBool(          get_role_m,                set(set_role_m))),
			di(label("probability go to favourite"),  probability.dialogAction( get_probabilityFavourite,  set(set_probabilityFavourite)))
			]),
		ci(label("pure person"),
			[
			 di(label("male"),          updateFieldBool( get_role_p,       set(set_role_p))),
			 di(label("go favourite"),  updateFieldBool( get_goFavourite,  set(set_goFavourite)))
			])
		])
	)
	].
% dialog =
% 	[
% 	di(label("male"),    subdialog( [
% 		di(label("probability go to tennis"),  probability.dialogAction( get_probabilityTennis,  set(set_probabilityTennis)))
% 		])),
% 	di(label("female"),  subdialog( [
% 		di(label("probability go to opera"),  probability.dialogAction( get_probabilityOpera,  set(set_probabilityOpera)))
% 		]))
% 	].

numberParameters(female(_)) = 1.
numberParameters(male(_)) = 1.
numberParameters(person_pure(_, _)) = 2.
numberParameters(person_mixed(_, _)) = 2.

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
		;
		Strategy = person_pure(IM, P),
		Result = person_pure(mutateRole(IM), P)
		;
		Strategy = person_mixed(IM, P),
		Result = person_mixed(mutateRole(IM), P)
	else if
		Index = 1,
		Strategy = person_mixed(IM, P)
	then
		probability.addGaussianNoise(float(Parameters^stddev), P, R, !Distribution, !Random),
		Result = person_mixed(IM, R)
	else if
		Index = 1,
		Strategy = person_pure(IM, P)
	then
		Result = person_pure(IM, bool.not(P))
	else
		throw("gl.battlesexes.strategy.mutateGene/8: Invalid gene index")
	).

print(Stream, female(P), !IO) :-
	io.print(Stream, "f\t", !IO),
	probability.print(Stream, P, !IO).

print(Stream, male(P), !IO) :-
	io.print(Stream, "m\t", !IO),
	probability.print(Stream, P, !IO).

print(Stream, person_mixed(I, P), !IO) :-
%	io.print(Stream, "p\t", !IO),
	printRole(Stream, I, !IO),
	io.print(Stream, ' ', !IO),
	probability.print(Stream, P, !IO).

print(Stream, person_pure(I, GF), !IO) :-
	printRole(Stream, I, !IO),
	io.print(Stream, ' ', !IO),
	(
		GF = yes,
		io.print(Stream, '1', !IO)
		;
		GF = no,
		io.print(Stream, '0', !IO)
	).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- func getCurrentChoice(strategy) = maybe(int).

getCurrentChoice(male(_))            = yes(0).
getCurrentChoice(female(_))          = yes(1).
getCurrentChoice(person_mixed(_, _)) = yes(2).
getCurrentChoice(person_pure(_, _))  = yes(3).

:- func setChoice(strategy, int) = setResult(strategy).

setChoice(Strategy, Index) = ok(Result) :-
	Strategy = male(_),
	(if
		Index = 0, R = Strategy ;
		Index = 1, R = female(default_probabilityOpera) ;
		Index = 2, R = person_mixed(default_role_m, default_probabilityFavourite) ;
		Index = 3, R = person_pure(default_role_p, default_goFavourite)
	then
		Result = R
	else
		throw("setChoice/2: invalid index")
	)
	;
	Strategy = female(_),
	(if
		Index = 0, R = male(default_probabilityTennis) ;
		Index = 1, R = Strategy ;
		Index = 2, R = person_mixed(default_role_m, default_probabilityFavourite) ;
		Index = 3, R = person_pure(default_role_p, default_goFavourite)
	then
		Result = R
	else
		throw("setChoice/2: invalid index")
	)
	;
	Strategy = person_mixed(_, _),
	(if
		Index = 0, R = male(default_probabilityTennis) ;
		Index = 1, R = female(default_probabilityOpera) ;
		Index = 2, R = Strategy ;
		Index = 3, R = person_pure(default_role_p, default_goFavourite)
	then
		Result = R
	else
		throw("setChoice/2: invalid index")
	)
	;
	Strategy = person_pure(_, _),
	(if
		Index = 0, R = male(default_probabilityTennis) ;
		Index = 1, R = female(default_probabilityOpera) ;
		Index = 2, R = person_mixed(default_role_m, default_probabilityFavourite) ;
		Index = 3, R = Strategy
	then
		Result = R
	else
		throw("setChoice/2: invalid index")
	).

/*
	(if
		Strategy = male(_),      Index = 0, R = Strategy ;
		Strategy = female(_),    Index = 0, R = male(default_probabilityTennis) ;
		Strategy = person(_, _), Index = 0, R = male(default_probabilityTennis) ;
		Strategy = male(_),      Index = 1, R = female(default_probabilityOpera) ;
		Strategy = female(_),    Index = 1, R = Strategy ;
		Strategy = person(_, _), Index = 1, R = female(default_probabilityOpera) ;
		Strategy = male(_),      Index = 2, R = person(default_isMale, default_probabilityFavourite) ;
		Strategy = female(_),    Index = 2, R = person(default_isMale, default_probabilityFavourite) ;
		Strategy = person(_, _), Index = 2, R = Strategy
	then
		Result = R
	else
		throw("setChoice/2: invalid index")
	).
*/

:- func default_probabilityTennis = probability.

default_probabilityTennis = probability.zero.

:- func default_probabilityOpera = probability.

default_probabilityOpera = probability.zero.

:- func default_role_p = role.

default_role_p = female.

:- func default_probabilityFavourite = probability.

default_probabilityFavourite = probability.zero.

:- func default_role_m = role.

default_role_m = female.

:- func default_goFavourite = bool.

default_goFavourite = no.







:- func get_probabilityTennis(strategy) = probability.

get_probabilityTennis(P) = R :-
	P = male(_),
	R = P^probabilityTennis
	;
	P = female(_),
	R = default_probabilityTennis
	;
	P = person_mixed(_, _),
	R = default_probabilityTennis
	;
	P = person_pure(_, _),
	R = default_probabilityTennis
	.

:- func set_probabilityTennis(strategy, probability) = strategy.

set_probabilityTennis(P, V) = R :-
	P = male(_),
	R = 'probabilityTennis :='(P, V)
	;
	P = female(_),
	R = male(V)
	;
	P = person_mixed(_, _),
	R = male(V)
	;
	P = person_pure(_, _),
	R = male(V)
	.


:- func get_probabilityOpera(strategy) = probability.

get_probabilityOpera(P) = R :-
	P = male(_),
	R = default_probabilityOpera
	;
	P = female(_),
	R = P^probabilityOpera
	;
	P = person_mixed(_, _),
	R = default_probabilityOpera
	;
	P = person_pure(_, _),
	R = default_probabilityOpera
	.

:- func set_probabilityOpera(strategy, probability) = strategy.

set_probabilityOpera(P, V) = R :-
	P = male(_),
	R = female(V)
	;
	P = female(_),
	R = 'probabilityOpera :='(P, V)
	;
	P = person_mixed(_, _),
	R = female(V)
	;
	P = person_pure(_, _),
	R = female(V)
	.


:- func get_role_p(strategy) = bool.

get_role_p(P) = toBool(R) :-
	P = male(_),
	R = default_role_p
	;
	P = female(_),
	R = default_role_p
	;
	P = person_mixed(_, _),
	R = default_role_p
	;
	P = person_pure(_, _),
	R = P^role_p
	.

:- func set_role_p(strategy, bool) = strategy.

set_role_p(P, VB) = R :-
	V = fromBool(VB),
	(
		P = male(_),
		R = person_pure(V, default_goFavourite)
	;
		P = female(_),
		R = person_pure(V, default_goFavourite)
	;
		P = person_mixed(_, _),
		R = person_pure(V, default_goFavourite)
	;
		P = person_pure(_, _),
		R = 'role_p :='(P, V)
	)
	.


:- func get_probabilityFavourite(strategy) = probability.

get_probabilityFavourite(P) = R :-
	P = male(_),
	R = default_probabilityFavourite
	;
	P = female(_),
	R = default_probabilityFavourite
	;
	P = person_mixed(_, _),
	R = P^probabilityFavourite
	;
	P = person_pure(_, _),
	R = default_probabilityFavourite
	.

:- func set_probabilityFavourite(strategy, probability) = strategy.

set_probabilityFavourite(P, V) = R :-
	P = male(_),
	R = person_mixed(default_role_p, V)
	;
	P = female(_),
	R = person_mixed(default_role_p, V)
	;
	P = person_mixed(_, _),
	R = 'probabilityFavourite :='(P, V)
	;
	P = person_pure(_, _),
	R = person_mixed(default_role_p, V)
	.


:- func get_role_m(strategy) = bool.

get_role_m(P) = toBool(R) :-
	P = male(_),
	R = default_role_m
	;
	P = female(_),
	R = default_role_m
	;
	P = person_mixed(_, _),
	R = P^role_m
	;
	P = person_pure(_, _),
	R = default_role_m
	.

:- func set_role_m(strategy, bool) = strategy.

set_role_m(P, VB) = R :-
	V = fromBool(VB),
	(
		P = male(_),
		R = person_mixed(V, default_probabilityFavourite)
	;
		P = female(_),
		R = person_mixed(V, default_probabilityFavourite)
	;
		P = person_mixed(_, _),
		R = 'role_m :='(P, V)
	;
		P = person_pure(_, _),
		R = person_mixed(V, default_probabilityFavourite)
	)
	.


:- func get_goFavourite(strategy) = bool.

get_goFavourite(P) = R :-
	P = male(_),
	R = default_goFavourite
	;
	P = female(_),
	R = default_goFavourite
	;
	P = person_mixed(_, _),
	R = default_goFavourite
	;
	P = person_pure(_, _),
	R = P^goFavourite
	.

:- func set_goFavourite(strategy, bool) = strategy.

set_goFavourite(P, V) = R :-
	P = male(_),
	R = person_pure(default_role_m, V)
	;
	P = female(_),
	R = person_pure(default_role_m, V)
	;
	P = person_mixed(_, _),
	R = person_pure(default_role_m, V)
	;
	P = person_pure(_, _),
	R = 'goFavourite :='(P, V)
	.























/*

:- func get_probabilityTennis(strategy) = probability.

get_probabilityTennis(P) = R :-
	P = male(_),
	R = P^probabilityTennis
	;
	P = female(_),
	R = default_probabilityTennis
	;
	P = person(_, _),
	R = default_probabilityTennis
	.

:- func set_probabilityTennis(strategy, probability) = strategy.

set_probabilityTennis(P, V) = R :-
	P = male(_),
	R = 'probabilityTennis :='(P, V)
	;
	P = female(_),
	R = male(V)
	;
	P = person(_, _),
	R = male(V)
	.


:- func get_probabilityOpera(strategy) = probability.

get_probabilityOpera(P) = R :-
	P = male(_),
	R = default_probabilityOpera
	;
	P = female(_),
	R = P^probabilityOpera
	;
	P = person(_, _),
	R = default_probabilityOpera
	.

:- func set_probabilityOpera(strategy, probability) = strategy.

set_probabilityOpera(P, V) = R :-
	P = male(_),
	R = female(V)
	;
	P = female(_),
	R = 'probabilityOpera :='(P, V)
	;
	P = person(_, _),
	R = female(V)
	.

:- func get_isMale(strategy) = bool.

get_isMale(P) = R :-
	P = male(_),
	R = default_isMale
	;
	P = female(_),
	R = default_isMale
	;
	P = person(_, _),
	R = P^isMale
	.

:- func set_isMale(strategy, bool) = strategy.

set_isMale(P, V) = R :-
	P = male(_),
	R = person(V, default_probabilityFavourite)
	;
	P = female(_),
	R = person(V, default_probabilityFavourite)
	;
	P = person(_, _),
	R = 'isMale :='(P, V)
	.


:- func get_probabilityFavourite(strategy) = probability.

get_probabilityFavourite(P) = R :-
	P = male(_),
	R = default_probabilityFavourite
	;
	P = female(_),
	R = default_probabilityFavourite
	;
	P = person(_, _),
	R = P^probabilityFavourite
	.

:- func set_probabilityFavourite(strategy, probability) = strategy.

set_probabilityFavourite(P, V) = R :-
	P = male(_),
	R = person(default_isMale, V)
	;
	P = female(_),
	R = person(default_isMale, V)
	;
	P = person(_, _),
	R = 'probabilityFavourite :='(P, V)
	.
*/

:- func toBool(role) = bool.
toBool(male) = yes.
toBool(female) = no.

:- func fromBool(bool) = role.
fromBool(yes) = male.
fromBool(no) = female.

:- func mutateRole(role) = role.

mutateRole(male) = female.
mutateRole(female) = male.


:- pred printRole(io.output_stream, role, io.state, io.state).
:- mode printRole(in, in, di, uo) is det.

printRole(Stream, male, !IO) :-
	io.print(Stream, "M", !IO).

printRole(Stream, female, !IO) :-
	io.print(Stream, "F", !IO).


:- pred parse(strategy, list(int), list(int)).
:- mode parse(in, out, in) is det.
:- mode parse(out, in, out) is semidet.

parse(P) -->
	{P = male(_)},
	[0],
	probability.parse(P^probabilityTennis)
	.

parse(P) -->
	{P = female(_)},
	[1],
	probability.parse(P^probabilityOpera)
	.

parse(P) -->
	{P = person_mixed(Role, ProbabilityFavourite)},
	[2],
	parseRole(Role),
	probability.parse(ProbabilityFavourite)
	.

parse(P) -->
	{P = person_pure(Role, GoFavourite)},
	[3],
	parseRole(Role),
	parseable.bool(GoFavourite)
	.

:- pred parseRole(role, list(int), list(int)).
:- mode parseRole(in, out, in) is det.
:- mode parseRole(out, in, out) is semidet.

parseRole(male) -->
	[1].
parseRole(female) -->
	[0].

:- end_module gl.battlesexes.strategy.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
