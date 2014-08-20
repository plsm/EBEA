/**
 * 

 * @author Pedro Mariano
 * @version 1.0 2014/04/08
 */
:- module tools.utils.

:- interface.

:- import_module ebea, ebea.population, ebea.population.players, ebea.streams, ebea.streams.birth.
:- import_module printable.
:- import_module bool, io, list, maybe.

:- pred printGraphVizHeader(io.output_stream, io.state, io.state).
:- mode printGraphVizHeader(in, di, uo) is det.

/**
 * Print a node of graphviz network that represents a player.
 */
:- pred printGraphVizNode(io.output_stream, bool, bool, list(playerBirthRecord(C)), key, io.state, io.state)
	<= printable(C).
:- mode printGraphVizNode(in, in, in, in, in, di, uo) is det.

/**
 * Opens a stream to write the average number of partners of a player if
 * the corresponding parameter is true.
 */

:- pred openMaybeStream(
	bool   :: in,
	string :: in,
	string :: in,
	string :: in,
	int    :: in,
	maybe(io.output_stream) :: out,
	list(string) :: in, list(string) :: out,
	io.state     :: di, io.state     :: uo
) is det.

:- pred closeMaybeStream(maybe(io.output_stream), io.state, io.state).
:- mode closeMaybeStream(in, di, uo) is det.

:- implementation.

:- import_module ebea.player, ebea.player.chromosome, ebea.player.selection, ebea.player.selection.chromosome.
:- import_module string.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

printGraphVizHeader(Stream, !IO) :-
	io.print(Stream, "graph {
size=\"16,9\"
page=\"16,9\"
center=1
", !IO).

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

closeMaybeStream(no, !IO).
closeMaybeStream(yes(Stream), !IO) :-
	io.close_output(Stream, !IO).

openMaybeStream(no, _, _, _, _, no, !FeedbackAsList, !IO).
openMaybeStream(yes, Directory, Prefix, BaseName, RunIndex, MStream, !FeedbackAsList, !IO) :-
	FileName = string.format("%s%s_%s_R%d.txt", [s(Directory), s(Prefix), s(BaseName), i(RunIndex)]),
	io.open_output(FileName, IStream, !IO),
	(	%
		IStream = ok(Stream),
		MStream = yes(Stream)
	;	
		IStream = error(Error),
		list.cons(string.format("IO error opening `%s` file: %s", [s(FileName), s(io.error_message(Error))]), !FeedbackAsList),
		MStream = no
	)
	.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- end_module tools.utils.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
