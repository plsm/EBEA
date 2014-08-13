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

:- pred write(format, string, config, maybe(string), io.state, io.state).
:- mode write(in, in, in, out, di, uo) is det.

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

/*						
	config_v1001(
		data.prng.supplyParameter,         %  1 random
		int,                               %  2 numberRuns
		int,                               %  3 numberIterations
		ebea.streams.level,                %  4 level
		ebea.population.dynamic,           %  5 dynamic
		probability,                       %  6 mutationProbability
		probability,                       %  7 migrationProbability
		ebea.player.age.parameters,        %  8 ageParameters
		ebea.player.energy.parameters,     %  9 energyParameters
		ebea_player_selection_parameters_v1002,  % 10 selectionParameters
		games_old_v1001,                         % 11 selectedGame
		gameConfig_old_v1003_2x2,                    % 12 cfg_2x2
		gameConfig_old_v1003_battlesexes,            % 13 battlesexes
		gameConfig_old_v1003_centipede,              % 14 centipede
		gameConfig_old_v1003_pgp,                    % 15 pgp
		'gameConfig_old_v1003_pgp+pa',               % 16 'pgp+pa'
		gameConfig_old_v1003_ultimatum               % 17 ultimatum
	) ;
	config_v1000(
		data.prng.supplyParameter,			 %  1 random
		int,										 %  2 numberRuns
		int,										 %  3 numberIterations
		ebea.streams.level,					 %  4 level
		float,									 %  5 carryingCapacity
		ebea.population.dynamic,			 %  6 dynamic
		float,									 %  7 mutationProbability
		ebea.player.age.parameters,		 %  8 ageParameters
		ebea.player.energy.parameters,	 %  9 energyParameters
		ebea_player_selection_parameters_v1002, % 10 selectionParameters
		games_old_v1001,							 % 11 selectedGame
		config_old_v1000_2x2,				 % 12 cfg_2x2
		config_old_v1000_battlesexes,	 % 13 battlesexes
		config_old_v1000_centipede,		 % 14 centipede
		config_old_v1000_pgp,				 % 15 pgp
		'config_old_v1000_pgp+pa',		 % 16 pgp+pa
		config_old_v1000_ultimatum		 % 19 ultimatum
	 ).
*/




:- type chromosome --->
	random ;
	partnerSelection(
		poolSize                :: int ,
		bitsPerProbability      :: int ,
		probabilityUpdateFactor :: float ,
		payoffThreshold_PS      :: float
	) ;
	opinion(
		payoffThreshold_O :: float
	)
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- pred parse(data.config.io.configFile, list(int), list(int)).
:- mode parse(in, out, in) is det.
:- mode parse(out, in, out) is semidet.

parse(configFileV1000(C)) -->	[0],	data.config.io.filterV1000.parse(C).
parse(configFileV1001(C)) -->	[1],	data.config.io.filterV1001.parse(C).
parse(configFileV1002(C)) -->	[2],	data.config.io.filterV1002.parse(C).
parse(configFileV1003(C)) -->	[3],	data.config.io.filterV1003.parse(C).
parse(configFileV1004(C)) -->	[4],	data.config.io.filterV1004.parse(C).


/*
parse(C) -->
	{C = config_v1001(
		Random,
		NumberRuns,
		NumberIterations,
		Level,
		Dynamic,
		MutationProbability,
		MigrationProbability,
		AgeParameters,
		EnergyParameters,
		SelectionParameters,
		SelectedGame,
		Cfg_2x2,
		BattleSexes,
		Centipede,
		PGP,
		PGP_PA,
		Ultimatum)},
	[1],
	data.prng.parse(Random),
	parseable.int32(NumberRuns),
	parseable.int32(NumberIterations),
	ebea.streams.parse(Level),
	ebea.population.parseDynamic(Dynamic),
	probability.parse(MutationProbability),
	probability.parse(MigrationProbability),
	ebea.player.age.parseParameters(AgeParameters),
	ebea.player.energy.parseParameters(EnergyParameters),
	ebea_player_selection_parseParameters_v1002(SelectionParameters),
	parseGames_v1001(SelectedGame),
	parse_GameConfig_old_v1003(Cfg_2x2),
	parse_GameConfig_old_v1003(BattleSexes),
	parse_GameConfig_old_v1003(Centipede),
	parse_GameConfig_old_v1003(PGP),
	parse_GameConfig_old_v1003(PGP_PA),
	parse_GameConfig_old_v1003(Ultimatum)
	.
*/







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

/*
map(config_v1001(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10_in, A11, A12, A13, A14, A15, A16, A17)) =
	config(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10_out, mapGames_v1001(A11), A12, A13, A14, B1, B2, A15, A16, A17) :-
	B1 = gameConfig(
		gl.givetake.game.default,
		gl.givetake.parameters.default,
		ebea.population.configuration.default(gl.givetake.strategy.default)
	),
	B2 = gameConfig(
		gl.investment.game.default,
		gl.investment.parameter.default,
		ebea.population.configuration.default(gl.investment.strategy.default)
	),
	A10_out = map_ebea_player_selection_parseParameters_v1002(A10_in)
	.



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
