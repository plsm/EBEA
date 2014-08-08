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

:- import_module ebea.player.chromosome, ebea.population.site, ebea.population.site.parameters.
:- import_module parseable.
:- import_module exception, float.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

:- type config_file --->
	config_v1003(
		data.prng.supplyParameter,        %  1 random
		int,                              %  2 numberRuns
		int,                              %  3 numberIterations
		ebea.streams.level,               %  4 level
		ebea.population.dynamic,          %  5 dynamic
		probability,                      %  6 mutationProbability
		probability,                      %  7 migrationProbability
		ebea.player.age.parameters,       %  8 ageParameters
		ebea.player.energy.parameters,    %  9 energyParameters
		ebea.player.selection.parameters, % 10 selectionParameters
		games,                            % 11 selectedGame
		config_2x2,                       % 12 cfg_2x2
		config_battlesexes,               % 13 battlesexes
		config_centipede,                 % 14 centipede
		config_givetake,                  % 15 givetake
		config_investment,                % 16 investment
		config_pgp,                       % 17 pgp
		'config_pgp+pa',                  % 18 pgp+pa
		config_ultimatum                  % 19 ultimatum
	) ;
	config_v1002(
		data.prng.supplyParameter,        %  1 random
		int,                              %  2 numberRuns
		int,                              %  3 numberIterations
		ebea.streams.level,               %  4 level
		ebea.population.dynamic,          %  5 dynamic
		probability,                      %  6 mutationProbability
		probability,                      %  7 migrationProbability
		ebea.player.age.parameters,       %  8 ageParameters
		ebea.player.energy.parameters,    %  9 energyParameters
		ebea_player_selection_parameters_v1002, % 10 selectionParameters
		games,                            % 11 selectedGame
		config_2x2,                       % 12 cfg_2x2
		config_battlesexes,               % 13 battlesexes
		config_centipede,                 % 14 centipede
		config_givetake,                  % 15 givetake
		config_investment,                % 16 investment
		config_pgp,                       % 17 pgp
		'config_pgp+pa',                  % 18 pgp+pa
		config_ultimatum                  % 19 ultimatum
	) ;
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
		games_old_v1001,                     % 11 selectedGame
		config_2x2,                        % 12 cfg_2x2
		config_battlesexes,                % 13 battlesexes
		config_centipede,                  % 14 centipede
		config_pgp,                        % 15 pgp
		'config_pgp+pa',                   % 16 'pgp+pa'
		config_ultimatum                   % 17 ultimatum
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

:- type ebea_player_selection_parameters_v1002 --->
	sp_v1002(
		poolSizeStdDev                :: float,
		bitsPerProbabilityStdDev      :: float,
		probabilityUpdateFactorStdDev :: float,
		payoffThresholdStdDev         :: float,
		
		uncertaintyIncreaseFactor     :: float,
		mu                            :: float
	).


:- type games_old_v1001 --->
	'2x2' ;
	battlesexes ;
	centipede ;
	pgp ;
	'pgp+pa' ;
	ultimatum.

:- type config_old_v1000(G, CS, P) --->
	config_old_v1000(
		G,                                   %  game
		P,                                   % parameters
		list(initialPlayers_old_v1000(CS)) % initialPopulation
	).

:- type initialPlayers_old_v1000(CS) --->
	initialPlayers_old_v1000(
		int,                       % quantity
		ebea.player.chromosome.chromosome(CS) % chromosome 
	).

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


:- type config_old_v1000_2x2         == config_old_v1000(gl.'2x2'.game.game,       gl.'2x2'.strategy.strategy,       gl.'2x2'.parameters.parameters).
:- type config_old_v1000_battlesexes == config_old_v1000(gl.battlesexes.game.game, gl.battlesexes.strategy.strategy, gl.battlesexes.parameters.parameters).
:- type config_old_v1000_centipede   == config_old_v1000(gl.centipede.game.game,   gl.centipede.strategy.strategy,   gl.centipede.parameters.parameters).
:- type config_old_v1000_pgp         == config_old_v1000(gl.pgp.game.game,         gl.pgp.strategy.strategy,         gl.pgp.parameters.parameters).
:- type 'config_old_v1000_pgp+pa'    == config_old_v1000(gl.'pgp+pa'.game.game,    gl.'pgp+pa'.strategy.strategy,    gl.'pgp+pa'.parameters.parameters).
:- type config_old_v1000_ultimatum   == config_old_v1000(gl.ultimatum.game.game,   gl.ultimatum.strategy.strategy,   gl.ultimatum.parameters.parameters).

:- instance parseable(initialPlayers_old_v1000(CS))
	<= parseable(CS)
	where
	[
	 pred(parse/3) is parse_initialPlayers_old_v1000
	 ].

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
			MConfig = ok(map(Config))
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

:- pred parse(config_file, list(int), list(int)).
:- mode parse(in, out, in) is det.
:- mode parse(out, in, out) is semidet.

parse(C) -->
	{C = config_v1000(
		Random,
		NumberRuns,
		NumberIterations,
		Level,
		CarryingCapacity,
		Dynamic,
		MutationProbability,
		AgeParameters,
		EnergyParameters,
		SelectionParameters,
		SelectedGame,
		Cfg_2x2,
		BattleSexes,
		Centipede,
		PGP,
		PGP_PA,
		Ultimatum
	)},
	[0],
	data.prng.parse(Random),
	parseable.int32(NumberRuns),
	parseable.int32(NumberIterations),
	ebea.streams.parse(Level),
	parseable.float32(CarryingCapacity),
	ebea.population.parseDynamic(Dynamic),
	parseable.float32(MutationProbability),
	ebea.player.age.parseParameters(AgeParameters),
	ebea.player.energy.parseParameters(EnergyParameters),
	ebea_player_selection_parseParameters_v1002(SelectionParameters),
	parseGames_v1001(SelectedGame),
	parse_GameConfig_old_v1000(Cfg_2x2),
	parse_GameConfig_old_v1000(BattleSexes),
	parse_GameConfig_old_v1000(Centipede),
	parse_GameConfig_old_v1000(PGP),
	parse_GameConfig_old_v1000(PGP_PA),
	parse_GameConfig_old_v1000(Ultimatum)
	.

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
	parse_GameConfig(Cfg_2x2),
	parse_GameConfig(BattleSexes),
	parse_GameConfig(Centipede),
	parse_GameConfig(PGP),
	parse_GameConfig(PGP_PA),
	parse_GameConfig(Ultimatum)
	.

parse(C) -->
	{C = config_v1002(
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
		GiveTake,
		Investment,
		PGP,
		PGP_PA,
		Ultimatum)},
	[2],
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
	parseGames(SelectedGame),
	parse_GameConfig(Cfg_2x2),
	parse_GameConfig(BattleSexes),
	parse_GameConfig(Centipede),
	parse_GameConfig(GiveTake),
	parse_GameConfig(Investment),
	parse_GameConfig(PGP),
	parse_GameConfig(PGP_PA),
	parse_GameConfig(Ultimatum)
	.

parse(C) -->
	{C = config_v1003(
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
		GiveTake,
		Investment,
		PGP,
		PGP_PA,
		Ultimatum)},
	[3],
	data.prng.parse(Random),
	parseable.int32(NumberRuns),
	parseable.int32(NumberIterations),
	ebea.streams.parse(Level),
	ebea.population.parseDynamic(Dynamic),
	probability.parse(MutationProbability),
	probability.parse(MigrationProbability),
	ebea.player.age.parseParameters(AgeParameters),
	ebea.player.energy.parseParameters(EnergyParameters),
	ebea.player.selection.parseParameters(SelectionParameters),
	parseGames(SelectedGame),
	parse_GameConfig(Cfg_2x2),
	parse_GameConfig(BattleSexes),
	parse_GameConfig(Centipede),
	parse_GameConfig(GiveTake),
	parse_GameConfig(Investment),
	parse_GameConfig(PGP),
	parse_GameConfig(PGP_PA),
	parse_GameConfig(Ultimatum)
	.


:- pred parse_GameConfig(gameConfig(G, CS, P), list(int), list(int))
	<= (parseable(G), parseable(CS), parseable(P)).
:- mode parse_GameConfig(in, out, in) is det.
:- mode parse_GameConfig(out, in, out) is semidet.

parse_GameConfig(Config) -->
	parseable.parse(Config^game),
	parseable.parse(Config^parameters),
	ebea.population.parameters.parse(Config^initialPopulation)
	.

:- pred parse_GameConfig_old_v1000(config_old_v1000(G, CS, P), list(int), list(int))
	<= (parseable(G), parseable(CS), parseable(P)).
:- mode parse_GameConfig_old_v1000(in, out, in) is det.
:- mode parse_GameConfig_old_v1000(out, in, out) is semidet.

parse_GameConfig_old_v1000(Config) -->
	{Config = config_old_v1000(Game, Parameters, InitialPopulation)},
	parseable.parse(Game),
	parseable.parse(Parameters),
	parseable.parseList(normalType, InitialPopulation)
	.
:- pred parse_initialPlayers_old_v1000(initialPlayers_old_v1000(CS), list(int), list(int))
	<= parseable(CS).
:- mode parse_initialPlayers_old_v1000(in, out, in) is det.
:- mode parse_initialPlayers_old_v1000(out, in, out) is semidet.

parse_initialPlayers_old_v1000(InitialPlayers) -->
	{InitialPlayers = initialPlayers_old_v1000(Quantity, Chromosome)},
	parseable.int32(Quantity),
	ebea.player.chromosome.parse(Chromosome)
	.

:- pred ebea_player_selection_parseParameters_v1002(ebea_player_selection_parameters_v1002, list(int), list(int)).
:- mode ebea_player_selection_parseParameters_v1002(in, out, in) is det.
:- mode ebea_player_selection_parseParameters_v1002(out, in, out) is semidet.

ebea_player_selection_parseParameters_v1002(P) -->
	{P = sp_v1002(PoolSizeStdDev, BitsPerProbabilityStdDev, ProbabilityUpdateFactorStdDev, PayoffThresholdStdDev, UncertaintyIncreaseFactor, MU)},
	parseable.float32(PoolSizeStdDev),
	parseable.float32(BitsPerProbabilityStdDev),
	parseable.float32(ProbabilityUpdateFactorStdDev),
	parseable.float32(PayoffThresholdStdDev),
	parseable.float32(UncertaintyIncreaseFactor),
	parseable.float32(MU).


/**
 * Convert the configuration parameters as stored in a text stream to the
 * internal representation.
 */

:- func map(config_file) = config.

map(config_v1000(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17)) =
	config(
		A1,
		A2,
		A3,
		A4,
		A6,
		probability.init(A7),
		probability.zero,
		A8,
		A9,
		map_ebea_player_selection_parseParameters_v1002(A10),
		mapGames_v1001(A11),
		map_v1000(A5, A12),
		map_v1000(A5, A13),
		map_v1000(A5, A14),
		gameConfig(
			gl.givetake.game.default,
			gl.givetake.parameters.default,
			ebea.population.parameters.default(gl.givetake.strategy.default)
		),
		gameConfig(
			gl.investment.game.default,
			gl.investment.parameter.default,
			ebea.population.parameters.default(gl.investment.strategy.default)
		),
		map_v1000(A5, A15),
		map_v1000(A5, A16),
		map_v1000(A5, A17)
	).
/*
Result :-
	Result^random = A1,
	Result^numberRuns = A2,
	Result^numberIterations = A3,
	Result^level = A4,
	Result^dynamic = A6,
	Result^mutationProbability = probability.init(A7),
	Result^migrationProbability = probability.zero,
	Result^ageParameters = A8,
	Result^energyParameters = A9,
	Result^selectionParameters = A10_out,
	Result^selectedGame = mapGames_v1001(A11),
	Result^cfg_2x2 = map_v1000(A5, A12),
	Result^battlesexes = map_v1000(A5, A13),
	Result^centipede = map_v1000(A5, A14),
	Result^givetake = gameConfig(
		gl.givetake.game.default,
		gl.givetake.parameters.default,
		ebea.population.parameters.default(gl.givetake.strategy.default)
		),
	Result^investment = gameConfig(
		gl.investment.game.default,
		gl.investment.parameter.default,
		ebea.population.parameters.default(gl.investment.strategy.default)
		),
	Result^pgp = map_v1000(A5, A15),
	Result^'pgp+pa' = map_v1000(A5, A16),
	Result^ultimatum = map_v1000(A5, A17),
	A10_out = map_ebea_player_selection_parseParameters_v1002(A10_in)
	.
*/
map(config_v1001(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10_in, A11, A12, A13, A14, A15, A16, A17)) =
	config(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10_out, mapGames_v1001(A11), A12, A13, A14, B1, B2, A15, A16, A17) :-
	B1 = gameConfig(
		gl.givetake.game.default,
		gl.givetake.parameters.default,
		ebea.population.parameters.default(gl.givetake.strategy.default)
	),
	B2 = gameConfig(
		gl.investment.game.default,
		gl.investment.parameter.default,
		ebea.population.parameters.default(gl.investment.strategy.default)
	),
	A10_out = map_ebea_player_selection_parseParameters_v1002(A10_in)
	.

map(config_v1002(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10_in, A11, A12, A13, A14, A15, A16, A17, A18, A19)) =
config(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10_out, A11, A12, A13, A14, A15, A16, A17, A18, A19) :-
	A10_out = map_ebea_player_selection_parseParameters_v1002(A10_in)
	.

map(config_v1003(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19)) =
	config(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19).


/**
 * Convert the config parameters from the internal representation to the
 * text stream representation.  This is the inverse function of {@code
 * map/1}.
 */

:- func pam(data.config.config) = config_file.

pam(config(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19)) =
	config_v1003(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19).


:- func mapGames_v1001(games_old_v1001) = games.

mapGames_v1001('2x2')       = '2x2'.
mapGames_v1001(battlesexes) = battlesexes.
mapGames_v1001(centipede)   = centipede.
mapGames_v1001(pgp)         = pgp.
mapGames_v1001('pgp+pa')    = 'pgp+pa'.
mapGames_v1001(ultimatum)   = ultimatum.



:- func map_v1000(float, config_old_v1000(G, CS, P)) = gameConfig(G, CS, P).

map_v1000(CarryingCapacity, config_old_v1000(A1, A2, A3)) = Result :-
	Result^game = A1,
	Result^parameters = A2,
	Result^initialPopulation = IP,
	IP^geometry = wellmixed,
	IP^sites = [Site],
	IP^defaultCarryingCapacity = float.round_to_int(CarryingCapacity),
	
	Site^id = 0,
	Site^carryingCapacity = float.round_to_int(CarryingCapacity),
	Site^chromosomes = list.map(Map, A3),
	Site^neighbourhood = [],
	
	Map =
	(func(X) = R :-
		X = initialPlayers_old_v1000(B1, B2),
		R^quantity = B1,
		R^chromosome = B2
	)
	.

:- pred parseGames_v1001(games_old_v1001, list(int), list(int)).
:- mode parseGames_v1001(in, out, in) is det.
:- mode parseGames_v1001(out, in, out) is semidet.

parseGames_v1001('2x2')       --> [0].
parseGames_v1001(battlesexes) --> [1].
parseGames_v1001(centipede)   --> [2].
parseGames_v1001(pgp)         --> [3].
parseGames_v1001('pgp+pa')    --> [4].
parseGames_v1001(ultimatum)   --> [5].




:- func map_ebea_player_selection_parseParameters_v1002(ebea_player_selection_parameters_v1002) = ebea.player.selection.parameters.

map_ebea_player_selection_parseParameters_v1002(
	sp_v1002(
		PoolSizeStdDev,
		BitsPerProbabilityStdDev,
		ProbabilityUpdateFactorStdDev,
		PayoffThresholdStdDev,
		UncertaintyIncreaseFactor,
		MU
	)) =
	sp(
		PoolSizeStdDev,
		BitsPerProbabilityStdDev,
		ProbabilityUpdateFactorStdDev,
		PayoffThresholdStdDev,
		UncertaintyIncreaseFactor,
		MU,
		0
	).

:- end_module data.config.io.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
