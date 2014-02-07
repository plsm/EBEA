/**
 * This tool edits a batch file.

 * @author Pedro Mariano
 * @version 1.0 2013/06/ 7
 */
:- module tool.editFileBatch.

:- interface.

:- import_module io, maybe.

:- pred go(maybe(string), io.state, io.state).
:- mode go(in, di, uo) is det.

:- implementation.

:- import_module file, file.batch, file.batch.view.
:- import_module ui_console, userInterface.
:- import_module int, math, float, list, string.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

go(MFilename, !IO) :-
	readBatch(MFilename, Filename, Batch, !IO),
	ui_console.show(d(file.batch.view.userInterface), Batch, NewBatch, !IO),
	writeBatch(Filename, convert(NewBatch), !IO).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- pred readBatch(maybe(string), string, batch, io.state, io.state).
:- mode readBatch(in, out, out(latest), di, uo) is det.

readBatch(MFilename, Filename, Batch, !IO) :-
	(
		MFilename = yes(Filename)
		;
		MFilename = no,
		Filename = "batch.txt"
	),
	io.open_input(Filename, RInputStream, !IO),
	(
		RInputStream = ok(InputStream),
		io.read(InputStream, IBatch, !IO),
		(
			IBatch = ok(VBatch),
			Batch = file.batch.convert(VBatch)
			;
			IBatch = eof,
			io.print("File is empty.\nUsing default batch data\n", !IO),
			Batch = file.batch.defaultBatch
			;
			IBatch = error(Message, Line),
			io.format("Parse error at line %d of file %s:\n%s\nUsing default batch data\n", [i(Line), s(Filename), s(Message)], !IO),
			Batch = file.batch.defaultBatch
		),
		io.close_input(InputStream, !IO)
		;
		RInputStream = error(Msg),
		io.format("IO error while reading %s:\n%s\nUsing default batch data\n", [s(io.error_message(Msg)), s(Filename)], !IO),
		Batch = file.batch.defaultBatch
	).


:- pred writeBatch(string, batch, io.state, io.state).
:- mode writeBatch(in, in(latest), di, uo) is det.

writeBatch(Filename, Batch, !IO) :-
	io.open_output(Filename, ROutputStream, !IO),
	(
		ROutputStream = ok(OutputStream),
		io.write(OutputStream, Batch, !IO),
		io.print(OutputStream, ".\n", !IO),
		io.close_output(OutputStream, !IO)
		;
		ROutputStream = error(Msg),
		io.format("IO error while opening %s:\n%s\nPlease enter a new filename: ", [s(Filename), s(io.error_message(Msg))], !IO),
		io.read_line_as_string(RFilename, !IO),
		(if
			RFilename = ok(NewFilename)
		then
			writeBatch(string.strip(NewFilename), Batch, !IO)
		else
			io.print("Exiting without saving batch data\n", !IO)
		)
	).

:- end_module tool.editFileBatch.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
