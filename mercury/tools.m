/**
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

:- implementation.

% :- import_module game.
% :- import_module parseable, parseable.iou.
% :- import_module io, list, maybe, string.

:- import_module ebea, ebea.streams, ebea.streams.birth, ebea.player, ebea.player.selection, ebea.player.selection.chromosome.
:- import_module printable.
:- import_module bool, io, list.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions


:- pred printGraphVizHeader(io.output_stream, io.state, io.state).
:- mode printGraphVizHeader(in, di, uo) is det.

printGraphVizHeader(Stream, !IO) :-
	io.print(Stream, "graph {
size=\"16,9\"
page=\"16,9\"
center=1
", !IO).

/**
 * Print a node of graphviz network that represents a player.
 */
:- pred printGraphVizNode(io.output_stream, bool, bool, list(playerBirthRecord(C)), int, io.state, io.state)
	<= printable(C).
:- mode printGraphVizNode(in, in, in, in, in, di, uo) is det.

printGraphVizNode(Stream, PrintSelectionGenes, PrintStrategyGenes, ListBirthRecords, ID, !IO) :-
	io.print(Stream, "P", !IO),
	io.print(Stream, ID, !IO),
	io.print(Stream, " [label=\"", !IO),
	io.print(Stream, ID, !IO),
	(if
		(
			PrintSelectionGenes = yes ;
			PrintStrategyGenes = yes
		),
		ebea.streams.birth.search(ID, ListBirthRecords, BR)
	then
		(
			PrintSelectionGenes = yes,
			io.print(Stream, "\\n", !IO),
			printable.print(Stream, BR^chromosome^selectionGenes, !IO)
			;
			PrintSelectionGenes = no
		),
		(
			PrintStrategyGenes = yes,
			io.print(Stream, "\\n", !IO),
			printable.print(Stream, BR^chromosome^strategyGenes, !IO)
			;
			PrintStrategyGenes = no
		)
	else
		true
	),
	io.print(Stream, "\"]\n", !IO)
	.

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
