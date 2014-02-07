/**
 * 

 * @author Pedro Mariano
 * @version 1.0 2014/01/31
 */
:- module mabs2014.

:- interface.

:- import_module io.

:- pred main(io.state, io.state).
:- mode main(di, uo) is det.

:- implementation.

:- import_module list.
:- import_module ebea, ebea.core, ebea.population, ebea.player.

:- import_module  ebea.player.selection, ebea.player.energy, ebea.player.selection.pcv.

:- import_module gl.ultimatum,  gl.ultimatum.game,  gl.ultimatum.strategy,  gl.ultimatum.parameters.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

main(!IO) :-
	solutions.solutions(parameters_ultimatum, run_ultimatum, !IO).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- pred parameters_ultimatum(list(initialPlayers(gl.ultimatum.strategy))).
:- mode parameters_ultimatum(out) is multi.

parameters_ultimatum([
	initialPlayers(10, chromosome(
		deathByOldAge(150, 1.0),
		plain,
		partnerSelection(4, 8, 0.5, 0.5)),
		d(dictator(CakeDivision))),
	initialPlayers(10, chromosome(
		deathByOldAge(150, 1.0),
		plain,
		partnerSelection(4, 8, 0.5, 0.5)),
		s(simple(CakeAcceptanceThreshold)))]) :-
	list.member(CakeDivision, [0, 2, 4, 5, 6]),
	list.member(CakeAcceptanceThreshold, [0, 2, 4, 5, 6]),
	CakeDivision + CakeAcceptanceThreshold =< 10.

run_ultimatum(InitialPlayers, !IO) :-
	

:- end_module mabs2014.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
