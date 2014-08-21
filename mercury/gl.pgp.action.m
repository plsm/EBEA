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
	decayHighDefects.

:- instance foldable(action, accumulator).

:- instance parseable(updateSiteState).

%% ************************************************************************
%% updateSiteState(Accumulator, Site) = Result
%%
%% Updates the site's state given the actions done by players in the given
%% site.
%%
:- func updateSiteState(accumulator, ebea.population.site.site) = ebea.population.site.state.

:- pred mapUpdateSiteState(int, updateSiteState, updateState(accumulator)).
:- mode mapUpdateSiteState(in,  out, out) is semidet.
:- mode mapUpdateSiteState(out, in,  out) is det.

:- func mapUpdateSiteState(updateSiteState) = updateState(accumulator).

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

updateSiteState(Accumulator, Site) = Result :-
	(if
		Accumulator^numberCooperates > Accumulator^numberDefects
	then
		Result = 'carryingCapacity :='(
			Site^state,
			Site^state^carryingCapacity * 0.9
		)
	else
		Result = Site^state
	)
	.

mapUpdateSiteState(0, decayHighDefects, updateSiteState).
mapUpdateSiteState(decayHighDefects) = updateSiteState.

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

parse(decayHighDefects) -->
	[0].

:- end_module gl.pgp.action.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
