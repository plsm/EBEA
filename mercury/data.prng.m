/**
 * Pseudo-random number generators used by EBEA.

 * @author Pedro Mariano
 * @version 1.0 2014/02/ 7
 */
:- module data.prng.

:- interface.

:- import_module data.config, data.seed.
:- import_module rng.
:- import_module userInterface.
:- import_module io, list, maybe.

:- type supply --->
	some [R] (supply(R) => ePRNG(R)).

:- type supplyParameter --->
	mt(seed) ;
	lcg(seed).

:- pred init(supplyParameter, maybe_error({supply, int}), io.state, io.state).
:- mode init(in, out, di, uo) is det.

/**
 * Return a dialog item to edit the pseudo-random number generator in a
 * EBEA configuration.
  
 */
:- func dialogItem = dialogItem(data.config.config).

% /**
%  * Return an user dialog to edit a pseudo-random number generator parameters.
%  */
% :- func dialog = list(dialogItem(supplyParameter)).

:- pred parse(supplyParameter, list(int), list(int)).
:- mode parse(in, out, in) is det.
:- mode parse(out, in, out) is semidet.

% :- func value(seed) = int.
% :- func 'value :='(seed, int) = seed.

:- func seed(supplyParameter) = seed.

:- func 'seed :='(supplyParameter, seed) = supplyParameter.

:- implementation.

:- import_module random, mersenneTwister.
:- import_module parseable, probability.
:- import_module exception, list.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

:- type supplyType --->
	st_mt ;
	st_random.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

init(mt(Seed), MSupply, !IO) :-
	data.seed.init(Seed, Value, !IO),
	(if
		mersenneTwister.init(Value, Supply)
	then
		MSupply = ok({'new supply'(Supply), Value})
	else
		MSupply = error("Invalid Mersenne Twister seed")
	).

init(lcg(Seed), ok({'new supply'(Supply), Value}), !IO) :-
	data.seed.init(Seed, Value, !IO),
	random.init(Value, Supply).


dialogItem =
	di(
		label("pseudo-random number generator"),
		'new selectOneOf'(
			getCurrentChoice_supplyParameter,
			setChoice_supplyParameter,
			set('random :='),
			[
				ci(label("Mersenne Twister"),              data.seed.dialog),
				ci(label("Linear Congruential Generator"), data.seed.dialog)
			]
		)
	).



seed(mt(Result))  = Result.
seed(lcg(Result)) = Result.

'seed :='(mt(_),  Seed) = mt(Seed).
'seed :='(lcg(_), Seed) = lcg(Seed).

parse(mt(Seed)) -->
	[0],
	data.seed.parse(Seed).

parse(lcg(Seed)) -->
	[1],
	data.seed.parse(Seed).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- func getCurrentChoice_supplyParameter(data.config.config) = maybe(currentChoice(supplyParameter)).

getCurrentChoice_supplyParameter(Config) = yes(cc(Index, Random)) :-
	Random = Config^random,
	(	%
		Random = mt(_),
		Index = 0
	;
		Random = lcg(_),
		Index = 1
	).

:- func setChoice_supplyParameter(data.config.config, int) = setResult(selectChoice(config, supplyParameter)).

setChoice_supplyParameter(OldConfig, Index) = ok(sc(NewConfig, NewSupplyParameter)) :-
	OldConfig^random = OldSupplyParameter,
	(
		OldSupplyParameter = mt(_),
		(if
			Index = 0, SP = OldSupplyParameter, C = OldConfig ;
			Index = 1, SP = lcg(clock),         C = 'random :='(OldConfig, SP)
		then
			NewConfig = C,
			NewSupplyParameter = SP
		else
			throw("setChoice_supplyParameter/2: invalid index")
		)
	;
		OldSupplyParameter = lcg(_),
		(if
			Index = 0, SP = mt(clock),          C = 'random :='(OldConfig, SP) ;
			Index = 1, SP = OldSupplyParameter, C = OldConfig
		then
			NewConfig = C,
			NewSupplyParameter = SP
		else
			throw("setChoice_supplyParameter/2: invalid index")
		)
	)
	.

:- end_module data.prng.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
