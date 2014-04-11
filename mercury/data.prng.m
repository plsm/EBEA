/**
 * Pseudo-random number generators used by EBEA.

 * @author Pedro Mariano
 * @version 1.0 2014/02/ 7
 */
:- module data.prng.

:- interface.

:- import_module rng.
:- import_module userInterface.
:- import_module io, list, maybe.

:- type supply --->
	some [R] (supply(R) => ePRNG(R)).

:- type supplyParameter --->
	mt(seed) ;
	random(seed).

:- type seed --->
	clock ;
	value(int).

:- pred init(supplyParameter, maybe_error({supply, int}), io.state, io.state).
:- mode init(in, out, di, uo) is det.

/**
 * Return an user dialog to edit a pseudo-random number generator parameters.
 */
:- func dialog = list(dialogItem(supplyParameter)).

:- pred parse(supplyParameter, list(int), list(int)).
:- mode parse(in, out, in) is det.
:- mode parse(out, in, out) is semidet.

:- implementation.

:- import_module random, mersenneTwister.
:- import_module parseable, probability.
:- import_module list.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

:- type supplyType --->
	st_mt ;
	st_random.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

init(mt(clock), MSupply, !IO) :-
	probability.seedClock(Seed, !IO),
	(if
		mersenneTwister.init(Seed, Supply)
	then
		MSupply = ok({'new supply'(Supply), Seed})
	else
		MSupply = error("Invalid Mersenne Twister seed")
	).

init(mt(value(Seed)), MSupply, !IO) :-
	(if
		mersenneTwister.init(Seed, Supply)
	then
		MSupply = ok({'new supply'(Supply), Seed})
	else
		MSupply = error("Invalid Mersenne Twister seed")
	).

init(random(clock), ok({'new supply'(Supply), Seed}), !IO) :-
	probability.seedClock(Seed, !IO),
	random.init(Seed, Supply).

init(random(value(Seed)), ok({'new supply'(Supply), Seed}), !IO) :-
	random.init(Seed, Supply).


dialog =
	[
	di(label("Mersenne Twister"), subdialog(
		[
		di(label("Clock"),   newValue(mt(clock))),
		di(label("Seed"),    updateFieldInt(getSeed, setSeed(st_mt)))
		])),
	di(label("Linear Congruential Generator"),       subdialog(
		[
		di(label("Clock"),   newValue(random(clock))),
		di(label("Seed"),    updateFieldInt(getSeed, setSeed(st_random)))
		]))
	].
														 

parse(mt(Seed)) -->
	[0],
	parseSeed(Seed).

parse(random(Seed)) -->
	[1],
	parseSeed(Seed).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- pred parseSeed(seed, list(int), list(int)).
:- mode parseSeed(in, out, in) is det.
:- mode parseSeed(out, in, out) is semidet.

parseSeed(clock) -->
	[0].

parseSeed(value(Value)) -->
	[1],
	parseable.int32(Value).

:- func getSeed(supplyParameter) = int.

getSeed(mt(Seed)) = getValueSeed(Seed).
getSeed(random(Seed)) = getValueSeed(Seed).

:- func getValueSeed(seed) = int.

getValueSeed(clock) = 123709.
getValueSeed(value(Result)) = Result.

:- func setSeed(supplyType, supplyParameter, int) = setResult(supplyParameter).

setSeed(st_mt, _, Value) = ok(mt(value(Value))).
setSeed(st_random, _, Value) = ok(random(value(Value))).


:- end_module data.prng.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
