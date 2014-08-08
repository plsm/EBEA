/**
 * This module contains all the modules that implement the tools in the
 * Energy Based Evolutionary Algorithm Toolkit.
 * 

 * @author Pedro Mariano
 * @version 1.0 2014/03/ 6
 */
:- module tools.

:- interface.

%:- include_module export_playerProfiles_gefx.
:- include_module export_playerProfiles_graphviz.
:- include_module 'PCVNetwork'.
:- include_module populationDynamics.
:- include_module processPlayerProfile.
:- include_module processPhenotype.
:- include_module utils.

:- implementation.

% :- import_module game.
% :- import_module parseable, parseable.iou.
% :- import_module io, list, maybe, string.

%:- import_module ebea, ebea.streams, ebea.streams.birth, ebea.player, ebea.player.chromosome, ebea.player.selection, ebea.player.selection.chromosome.
%:- import_module printable.
%:- import_module bool, io, list.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions



% /**
%  * Read the birth file to retrieve a list with the initial players.
%  */
% :- pred readInitialPopulation(
% 	io.binary_input_stream,
% 	G,
% 	pred(
% 		list(playerBirthRecord(C)),
% 		maybe(iterationBirthRecords(C)),  maybe(iterationBirthRecords(C)),
% 		parseable.iou.cache,              parseable.iou.cache,
% 		list(string),                     list(string),
% 		io.state,                         io.state
% 	),
% 	list(string), list(string),
% 	io.state, io.state
% )
% 	<= (
% 	asymmetricGame(G, C),
% 	parseable(C)).
% :- mode readInitialPopulation(
% 	in, in, in(pred(in, in, out, in, out, in, out, di, uo) is det),
% 	in, out, di, uo) is det.

% readInitialPopulation(Stream, Game, Pred, !FeedbackAsList, !IO) :-
% 	ebea.streams.birth.readInitialPopulation(Stream, Game, RIIterationBirthRecords, InitialBirthAdvancedResult, InitialBirthCache, !IO),
% 	(
% 		RIIterationBirthRecords = delayed
% 		;
% 		RIIterationBirthRecords = ok(ok(IterationBirthRecords)),
% 		IterationBirthRecords = ibr(_, ListBirthRecords),
% 		Pred(
% 			ListBirthRecords,
% 			InitialBirthAdvancedResult, FinalBirthAdvancedResult,
% 			InitialBirthCache, FinalBirthCache,
% 			!FeedbackAsList,
% 			!IO)
% 		;
% 		RIIterationBirthRecords = ok(eof),
% 		list.cons("birth file is empty", !FeedbackAsList)
% 		;
% 		RIIterationBirthRecords = ok(error(Error)),
% 		list.cons(string.format("io error while reading birth file: %s", [s(io.error_message(Error))]), !FeedbackAsList)
% 		;
% 		RIIterationBirthRecords = parseError,
% 		list.cons("parse error while reading birth file", !FeedbackAsList)
% 	).

% /*
% 	(
% 		(
% 			RIIterationBirthRecords = delayed,
% 			ListBirthRecords = []
% 			;
% 			RIIterationBirthRecords = ok(ok(IterationBirthRecords)),
% 			IterationBirthRecords = ibr(_, ListBirthRecords)
% 		),
% 		Pred(
% 			ListBirthRecords,
% 			InitialBirthAdvancedResult, FinalBirthAdvancedResult,
% 			InitialBirthCache, FinalBirthCache,
% 			!FeedbackAsList,
% 			!IO),
% 		io.print(FinalBirthAdvancedResult, !IO),
% 		io.nl(!IO),
% 		io.print(FinalBirthCache, !IO),
% 		io.nl(!IO)
% 		;
% 		RIIterationBirthRecords = ok(eof),
% 		list.cons("birth file is empty", !FeedbackAsList)
% 		;
% 		RIIterationBirthRecords = ok(error(Error)),
% 		list.cons(string.format("io error while reading birth file: %s", [s(io.error_message(Error))]), !FeedbackAsList)
% 		;
% 		RIIterationBirthRecords = parseError,
% 		list.cons("parse error while reading birth file", !FeedbackAsList)
% 	).*/

:- end_module tools.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
