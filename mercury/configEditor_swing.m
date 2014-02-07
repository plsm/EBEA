/**
 * Main module of the editor of a EBEA simulation parameters.  These
 * parameters are represented by type {@code config} defined in module
 * {@code file.config}.

 * @author Pedro Mariano
 * @version 1.0 2013/12/ 7
 */
:- module configEditor_swing.

:- interface.

:- import_module io.

:- pred main(io.state, io.state).
:- mode main(di, uo) is det.

:- implementation.

:- import_module data, data.config, data.config.io.
:- import_module userInterface.
:- import_module ui_console.
:- import_module ui_swing.
:- import_module maybe.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

main(!IO) :-
	Config = data.config.default,
	ui_swing.show("EBEA config editor", d(dialog(data.config.dialog)), Config, Result2, !IO),
	data.config.io.write("config.txt", Result2, MErrors, !IO),
	(
		MErrors = no
		;
		MErrors = yes(Errors),
		io.print(Errors, !IO),
		io.nl(!IO)
	)
	.
	% data.config.io.read("config.txt", MConfig, !IO),
	% (
	% 	MConfig = ok(Config)
	% 	;
	% 	MConfig = error(Msg),
	% 	io.print(Msg, !IO),
	% 	io.nl(!IO),
	% 	Config = data.config.default
	% ),
	% ui_swing.show("EBEA config editor", d(data.config.dialog), Config, Result2, !IO),
	% data.config.io.write("config.txt", Result2, MErrors, !IO),
	% (
	% 	MErrors = no
	% 	;
	% 	MErrors = yes(Errors),
	% 	io.print(Errors, !IO),
	% 	io.nl(!IO)
	% ),
	% data.config.runBackground(Result2, !IO)
	% .

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- end_module configEditor_swing.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
