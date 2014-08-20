/**
 * Seed used in pseudo-random number generators.

 * @author Pedro Mariano
 * @version 1.0 2014/03/30
 */
:- module data.seed.

:- interface.

:- import_module userInterface.
:- import_module io, list.
:- import_module data.prng.

:- type seed --->
	clock ;
	value(
		v :: int
	).

:- func dialog = list(dialogItem(supplyParameter)).

:- pred init(seed, int, io.state, io.state).
:- mode init(in, out, di, uo) is det.

:- pred parse(seed, list(int), list(int)).
:- mode parse(in, out, in) is det.
:- mode parse(out, in, out) is semidet.

:- func v(seed) = int.
:- mode v(in(bound(value(ground)))) = out is det.

:- func 'v :='(seed, int) = seed.
:- mode 'v :='(in(bound(value(ground))), in) = out is det.

:- implementation.

:- import_module probability.
:- import_module exception, maybe.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

dialog =
	[di(
		label("seed"),
		'new selectOneOf'(
			getCurrentChoice_seed,
			setChoice_seed,
			set('seed :='),
			[
			ci(label("Clock"),   []),
			ci(label("Value"),
				[di(label(""), updateFieldInt(v, set('v :=')))])
			]
		))
	].

init(clock, Seed, !IO) :-
	probability.seedClock(Seed, !IO).

init(value(Seed), Seed, !IO).
	
parse(clock) -->
	[0].

parse(value(Value)) -->
	[1],
	parseable.int32(Value).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- func getCurrentChoice_seed(supplyParameter) = maybe(currentChoice(seed)).

getCurrentChoice_seed(SupplyParameter) = yes(cc(Index, Seed)) :-
	% (
	% 	SupplyParameter = mt(Seed)
	% ;
	% 	SupplyParameter = random(Seed)
	% ),
	Seed = SupplyParameter^seed,
	(
		Seed = clock,
		Index = 0
	;
		Seed = value(_),
		Index = 1
	)
	.

:- func setChoice_seed(supplyParameter, int) = setResult(selectChoice(supplyParameter, seed)).

setChoice_seed(OldSupplyParameter, Index) = ok(sc(NewSupplyParameter, NewSeed)) :-
	OldSeed = OldSupplyParameter^seed,
	(	%
		OldSeed = clock,
		(if
			Index = 0, S = OldSeed,       SP = OldSupplyParameter ;
			Index = 1, S = value(123709), SP = 'seed :='(OldSupplyParameter, S)
		then
			NewSupplyParameter = SP,
			NewSeed = S
		else
			throw("setChoice_seed/2: invalid index")
		)
	;
		OldSeed = value(_),
		(if
			Index = 0, S = clock,   SP = 'seed :='(OldSupplyParameter, S) ;
			Index = 1, S = OldSeed, SP = OldSupplyParameter
		then
			NewSupplyParameter = SP,
			NewSeed = S
		else
			throw("setChoice_seed/2: invalid index")
		)
	).

/*
  
:- func getCurrentChoice_seed(seed) = maybe(int).

getCurrentChoice_seed(clock)   = yes(0).
getCurrentChoice_seed(seed(_)) = yes(1).

:- func setChoice_seed(seed, int) = setResult(seed).

setChoice_seed(Seed, Index) = ok(Result) :-
	Seed = clock,
	(if
		Index = 0, R = Seed ;
		Index = 1, R = seed(123709)
	then
		Result = R
	else
		throw("setChoice_seed/2: invalid index")
	)
	;
	Seed = seed(_),
	(if
		Index = 0, R = clock ;
		Index = 1, R = Seed
	then
		Result = R
	else
		throw("setChoice_seed/2: invalid index")
	)
	.


  */

:- end_module data.seed.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
