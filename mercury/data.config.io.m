/**
 * Provides predicates to read and write a {@code data.config.config} instance.

 * @author Pedro Mariano
 * @version 1.0 2014/01/ 3
 */
:- module data.config.io.

:- interface.

:- import_module io, maybe, string.

:- type format --->
	mercury ;
	binary.

:- pred read(string, maybe_error(config), io.state, io.state).
:- mode read(in, out, di, uo) is det.

:- pred read(format, string, maybe_error(config), io.state, io.state).
:- mode read(in, in, out, di, uo) is det.


%% ************************************************************************
%% write(Format, Filename, Config, MErrors, !IO)
%%
%% Write the EBEA configuration to a file.  The format depends on parameter
%% {@code Format}.  IO errors are returned in parameter {@code MErrors} as
%% {@code yes(Message)}.  If writing is sucessfull parameter {@code
%% MErrors} is unified with {@code no}.
%%
%% @warn Format {@code mercury} does not work in the java grade.
%%
%% @param Format the format to write the EBEA configuration
%%
%% @param Filename The file-name.
%%
%% @param Config The EBEA configuration
%%
%% @param MErrors IO errors are returned in this parameter
%%
%% @param !IO IO state
%%
:- pred write(format, string, config, maybe(string), io.state, io.state).
:- mode write(in, in, in, out, di, uo) is det.

%% ************************************************************************
%% standardFileNameExtension(Filename) = Result
%%
%% Given a filename for an EBEA data configuration see if it has the correct
%% extension.
%%
:- func standardFileNameExtension(string) = string.

:- implementation.

:- include_module filterV1000, filterV1001, filterV1002, filterV1003, filterV1004.

:- import_module data.config.io.filterV1000, data.config.io.filterV1001,
data.config.io.filterV1002, data.config.io.filterV1003,
data.config.io.filterV1004.

:- import_module ebea.player.chromosome, ebea.population.site, ebea.population.site.parameters.
:- import_module parseable.
:- import_module exception, float.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

:- type configFile --->
	configFileV1000(data.config.io.filterV1000.configFile) ;
	configFileV1001(data.config.io.filterV1001.configFile) ;
	configFileV1002(data.config.io.filterV1002.configFile) ;
	configFileV1003(data.config.io.filterV1003.configFile) ;
	configFileV1004(data.config.io.filterV1004.configFile)
	.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

read(Filename, MConfig, !IO) :-
	read(mercury, Filename, MercuryMConfig, !IO),
	(if
		MercuryMConfig = ok(_)
	then
		MConfig = MercuryMConfig
	else
		read(binary, Filename, BinaryMConfig, !IO),
		(if
			BinaryMConfig = ok(_)
		then
			MConfig = BinaryMConfig
		else
			MConfig = error("Could not read file")
		)
	).

read(mercury, Filename, MConfig, !IO) :-
	io.open_input(Filename, IStream, !IO),
	(
		IStream = ok(Stream),
		io.read(Stream, IConfig, !IO),
		(
			IConfig = ok(Config),
			MConfig = ok(data.config.io.map(Config))
			;
			IConfig = eof,
			MConfig = error("end-of-file reached while reading config file")
			;
			IConfig = error(Message, Line),
			MConfig = error(string.format("%d: %s", [i(Line), s(Message)]))
		),
		io.close_input(Stream, !IO)
		;
		IStream = error(Error),
		MConfig = error(string.format("IO error opening `%s` file: %s", [s(Filename), s(io.error_message(Error))]))
	).

read(binary, Filename, MConfig, !IO) :-
	io.open_binary_input(Filename, IStream, !IO),
	(
		IStream = ok(Stream),
		io.read_binary_file(Stream, IListBytes, !IO),
		(
			IListBytes = ok(ListBytes),
			(if
				data.config.io.parse(Config, ListBytes, [])
			then
				MConfig = ok(map(Config))
			else
				MConfig = error("parse error while processing config file")
			)
			;
			IListBytes = eof,
			MConfig = error("end-of-file reached while reading config file")
			;
			IListBytes = error(Error),
			MConfig = error(string.format("IO error reading `%s` file: %s", [s(Filename), s(io.error_message(Error))]))
		),
		io.close_binary_input(Stream, !IO)
		;
		IStream = error(Error),
		MConfig = error(string.format("IO error opening `%s` file: %s", [s(Filename), s(io.error_message(Error))]))
	).



write(mercury, Filename, Config, MErrors, !IO) :-
	io.open_output(Filename, IStream, !IO),
	(
		IStream = ok(Stream),
		io.write(Stream, pam(Config), !IO),
		io.print(Stream, ".\n", !IO),
		io.close_output(Stream, !IO),
		MErrors = no
		;
		IStream = error(Error),
		MErrors = yes(string.format("IO error opening `%s` file: %s", [s(Filename), s(io.error_message(Error))]))
	).

write(binary, Filename, Config, MErrors, !IO) :-
	io.open_binary_output(Filename, IStream, !IO),
	(
		IStream = ok(Stream),
		data.config.io.parse(pam(Config), ListBytes, []),
		list.foldl(io.write_byte(Stream), ListBytes, !IO),
		io.close_binary_output(Stream, !IO),
		MErrors = no
		;
		IStream = error(Error),
		MErrors = yes(string.format("IO error opening `%s` file: %s", [s(Filename), s(io.error_message(Error))]))
	).

standardFileNameExtension(Filename) = Result :-
	 (if
		string.to_upper(string.right(Filename, 5)) = ".EBEA"
	 then
		Result = Filename
	 else
		Result = Filename ++ ".ebea"
	 ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- pred parse(data.config.io.configFile, list(int), list(int)).
:- mode parse(in, out, in) is det.
:- mode parse(out, in, out) is semidet.

parse(configFileV1000(C)) -->	[0],	data.config.io.filterV1000.parse(C).
parse(configFileV1001(C)) -->	[1],	data.config.io.filterV1001.parse(C).
parse(configFileV1002(C)) -->	[2],	data.config.io.filterV1002.parse(C).
parse(configFileV1003(C)) -->	[3],	data.config.io.filterV1003.parse(C).
parse(configFileV1004(C)) -->	[0xEB, 0xEA,  4],	data.config.io.filterV1004.parse(C).

/**
 * Convert the configuration parameters as stored in a text stream to the
 * internal representation.
 */

:- func map(data.config.io.configFile) = config.

map(configFileV1000(C)) = data.config.io.filterV1000.map(C).
map(configFileV1001(C)) = data.config.io.filterV1001.map(C).
map(configFileV1002(C)) = data.config.io.filterV1002.map(C).
map(configFileV1003(C)) = data.config.io.filterV1003.map(C).
map(configFileV1004(C)) = data.config.io.filterV1004.map(C).

/**
 * Convert the config parameters from the internal representation to the
 * text stream representation.  This is the inverse function of {@code
 * map/1}.
 */

:- func pam(data.config.config) = data.config.io.configFile.

pam(config(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19)) =
	configFileV1004(configFile(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19)).

:- end_module data.config.io.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
