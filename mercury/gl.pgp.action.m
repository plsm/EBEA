/**
 * Provides the type that represents the set of actions for all PGP
 * players.

 * @author Pedro Mariano
 * @version 1.0 2014/08/18
 */
:- module gl.pgp.action.

:- interface.

:- import_module ebea, ebea.population, ebea.population.site.

%% ************************************************************************
%% Set of actions for all players in the Public Good Provision Game.
:- type action --->
	cooperate ;
	defect.

:- type accumulator.

:- type updateSiteState --->
	decayHighDefects(
		decreaseFactor :: float,
		recoveryFactor :: float
	).

:- instance foldable(action, accumulator).

:- instance parseable(updateSiteState).

%% ************************************************************************
%% updateSiteState(Accumulator, Site) = Result
%%
%% Updates the site's state given the actions done by players in the given
%% site.
%%
:- func updateSiteState(updateSiteState, accumulator, ebea.population.site.site) = ebea.population.site.state.

%:- pred mapUpdateSiteState(int, updateSiteState, updateState(accumulator)).
%:- mode mapUpdateSiteState(in,  out, out) is semidet.
%:- mode mapUpdateSiteState(out, in,  out) is det.

:- func mapUpdateSiteState(updateSiteState) = updateState(accumulator).

:- func defaultSiteUpdateFunction = updateSiteState.

:- func dialogSiteUpdateFunction = list(dialogItem(updateSiteState)).

:- implementation.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

:- type accumulator --->
	ac(
		numberCooperates :: int,
		numberDefects    :: int
	).

:- instance foldable(action, accumulator) where
[
	func(fold/2)   is gl.pgp.action.fold,
	func(initAC/0) is gl.pgp.action.init
].

:- instance parseable(updateSiteState) where
[
	pred(parse/3) is gl.pgp.action.parse
].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

updateSiteState(decayHighDefects(DecreaseFactor, RecoveryFactor), Accumulator, Site) = Result :-
	(if
		Accumulator^numberCooperates > Accumulator^numberDefects
	then
		Result = 'carryingCapacity :='(
			Site^currentState,
			Site^currentState^carryingCapacity * DecreaseFactor
		)
	else
		Result = 'carryingCapacity :='(
			Site^currentState,
			Site^currentState^carryingCapacity
			+ (Site^normalState^carryingCapacity - Site^currentState^carryingCapacity) * RecoveryFactor
		)
	)
	.

%mapUpdateSiteState(0, UpdateSiteState, updateSiteState(UpdateSiteState)).
mapUpdateSiteState(UpdateSiteState) = updateSiteState(UpdateSiteState).

defaultSiteUpdateFunction = decayHighDefects(0.98, 0.97).

dialogSiteUpdateFunction =
	[
	di(
		label("decay if defects are majority"),
		updateFieldFloat( getDecreaseFactor, userInterface.checkFloat("decrease factor", bounded(0.0, no), bounded(1.0, no), setDecreaseFactor))),
	di(
		label("recovery factor"),
		updateFieldFloat( getRecoveryFactor, userInterface.checkFloat("recovery factor", bounded(0.0, no), bounded(1.0, no), setRecoveryFactor)))
	].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- func init = accumulator.
init = ac(0, 0).

:- func fold(action, accumulator) = accumulator.

fold(cooperate, AC) = 'numberCooperates :='(AC, AC^numberCooperates + 1).
fold(defect, AC)    = 'numberDefects :='(   AC, AC^numberDefects    + 1).

:- pred parse(updateSiteState, parseable.state, parseable.state).
:- mode parse(in, out, in) is det.
:- mode parse(out, in, out) is semidet.

parse(decayHighDefects(DecreaseFactor, RecoveryFactor)) -->
	[0],
	parseable.float32(DecreaseFactor),
	parseable.float32(RecoveryFactor).


:- func getDecreaseFactor(updateSiteState) = float.

getDecreaseFactor(decayHighDefects(DecreaseFactor, _)) = DecreaseFactor.

:- func setDecreaseFactor(updateSiteState, float) = updateSiteState.

setDecreaseFactor(UpdateSiteState, Value) = 'decreaseFactor :='(UpdateSiteState, Value).



:- func getRecoveryFactor(updateSiteState) = float.

getRecoveryFactor(decayHighDefects(_, RecoveryFactor)) = RecoveryFactor.

:- func setRecoveryFactor(updateSiteState, float) = updateSiteState.

setRecoveryFactor(UpdateSiteState, Value) = 'recoveryFactor :='(UpdateSiteState, Value).

:- end_module gl.pgp.action.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
