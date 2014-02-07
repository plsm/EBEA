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

:- import_module ebea.population.site, ebea.population.site.parameters.
:- import_module parseable.
:- import_module exception, float.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

:- type config_file --->
	config992(
		random                :: data.prng.supplyParameter,
		numberRuns            :: int,
		numberIterations      :: int,
		level                 :: ebea.streams.level,
		dynamic               :: ebea.population.dynamic,
		mutationProbability   :: probability,
		migrationProbability  :: probability,
		ageParameters         :: ebea.player.age.parameters,
		energyParameters      :: ebea.player.energy.parameters,
		selectionParameters   :: ebea.player.selection.parameters,
 		selectedGame          :: games,
 		cfg_2x2               :: config_2x2,
		battlesexes           :: config_battlesexes,
 		centipede             :: config_centipede,
 		pgp                   :: config_pgp,
 		'pgp+pa'              :: 'config_pgp+pa',
 		ultimatum             :: config_ultimatum
	) ;
	config1238605(
		data.prng.supplyParameter,			 %  1 random
		int,										 %  2 numberRuns
		int,										 %  3 numberIterations
		ebea.streams.level,					 %  4 level
		float,									 %  5 carryingCapacity
		ebea.population.dynamic,			 %  6 dynamic
		float,									 %  7 mutationProbability
		ebea.player.age.parameters,		 %  8 ageParameters
		ebea.player.energy.parameters,	 %  9 energyParameters
		ebea.player.selection.parameters, % 10 selectionParameters
		games,									 % 11 selectedGame
		config_old_1238605_2x2,				 % 12 cfg_2x2
		config_old_1238605_battlesexes,	 % 13 battlesexes
		config_old_1238605_centipede,		 % 14 centipede
		config_old_1238605_pgp,				 % 15 pgp
		'config_old_1238605_pgp+pa',		 % 16 pgp+pa
		config_old_1238605_ultimatum		 % 19 ultimatum
	 ).


:- type config_old_1238605(G, CS, P) --->
	config_old_1238605(
		G,                                   %  game
		P,                                   % parameters
		list(initialPlayers_old_1238605(CS)) % initialPopulation
	).

:- type initialPlayers_old_1238605(CS) --->
	initialPlayers_old_1238605(
		int,                       % quantity
		ebea.player.chromosome(CS) % chromosome 
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


:- type config_old_1238605_2x2          == config_old_1238605(gl.'2x2'.game.game,       gl.'2x2'.strategy.strategy,       gl.'2x2'.parameters.parameters).
:- type config_old_1238605_battlesexes  == config_old_1238605(gl.battlesexes.game.game,   gl.battlesexes.strategy.strategy,   gl.battlesexes.parameters.parameters).
:- type config_old_1238605_centipede    == config_old_1238605(gl.centipede.game.game,   gl.centipede.strategy.strategy,   gl.centipede.parameters.parameters).
:- type config_old_1238605_pgp          == config_old_1238605(gl.pgp.game.game,         gl.pgp.strategy.strategy,         gl.pgp.parameters.parameters).
:- type 'config_old_1238605_pgp+pa'     == config_old_1238605(gl.'pgp+pa'.game.game,    gl.'pgp+pa'.strategy.strategy,    gl.'pgp+pa'.parameters.parameters).
:- type config_old_1238605_ultimatum    == config_old_1238605(gl.ultimatum.game.game,   gl.ultimatum.strategy.strategy,   gl.ultimatum.parameters.parameters).

:- instance parseable(initialPlayers_old_1238605(CS))
	<= parseable(CS)
	where
	[
	 pred(parse/3) is parse_initialPlayers_old_1238605
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
	{C = config1238605(
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
	ebea.player.selection.parseParameters(SelectionParameters),
	parseGames(SelectedGame),
	parse_GameConfig_old_1238605(Cfg_2x2),
	parse_GameConfig_old_1238605(BattleSexes),
	parse_GameConfig_old_1238605(Centipede),
	parse_GameConfig_old_1238605(PGP),
	parse_GameConfig_old_1238605(PGP_PA),
	parse_GameConfig_old_1238605(Ultimatum)
	.

parse(C) -->
	{C = config992(
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
	ebea.player.selection.parseParameters(SelectionParameters),
	parseGames(SelectedGame),
	parse_GameConfig(Cfg_2x2),
	parse_GameConfig(BattleSexes),
	parse_GameConfig(Centipede),
	parse_GameConfig(PGP),
	parse_GameConfig(PGP_PA),
	parse_GameConfig(Ultimatum)
	.


:- pred parse_GameConfig(config(G, CS, P), list(int), list(int))
	<= (parseable(G), parseable(CS), parseable(P)).
:- mode parse_GameConfig(in, out, in) is det.
:- mode parse_GameConfig(out, in, out) is semidet.

parse_GameConfig(Config) -->
	parseable.parse(Config^game),
	parseable.parse(Config^parameters),
	ebea.population.parameters.parse(Config^initialPopulation)
	.

:- pred parse_GameConfig_old_1238605(config_old_1238605(G, CS, P), list(int), list(int))
	<= (parseable(G), parseable(CS), parseable(P)).
:- mode parse_GameConfig_old_1238605(in, out, in) is det.
:- mode parse_GameConfig_old_1238605(out, in, out) is semidet.

parse_GameConfig_old_1238605(Config) -->
	{Config = config_old_1238605(Game, Parameters, InitialPopulation)},
	parseable.parse(Game),
	parseable.parse(Parameters),
	parseable.parseList(normalType, InitialPopulation)
	.
:- pred parse_initialPlayers_old_1238605(initialPlayers_old_1238605(CS), list(int), list(int))
	<= parseable(CS).
:- mode parse_initialPlayers_old_1238605(in, out, in) is det.
:- mode parse_initialPlayers_old_1238605(out, in, out) is semidet.

parse_initialPlayers_old_1238605(InitialPlayers) -->
	{InitialPlayers = initialPlayers_old_1238605(Quantity, Chromosome)},
	parseable.int32(Quantity),
	ebea.player.parseChromosome(Chromosome)
	.


/**
 * Convert the configuration parameters as stored in a text stream to the
 * internal representation.
 */

:- func map(config_file) = config.

map(config1238605(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17)) = Result :-
	Result^random = A1,
	Result^numberRuns = A2,
	Result^numberIterations = A3,
	Result^level = A4,
	Result^dynamic = A6,
	Result^mutationProbability = probability.init(A7),
	Result^migrationProbability = probability.zero,
	Result^ageParameters = A8,
	Result^energyParameters = A9,
	Result^selectionParameters = A10,
	Result^selectedGame = A11,
	Result^cfg_2x2 = map1238605(A5, A12),
	Result^battlesexes = map1238605(A5, A13),
	Result^centipede = map1238605(A5, A14),
	Result^pgp = map1238605(A5, A15),
	Result^'pgp+pa' = map1238605(A5, A16),
	Result^ultimatum = map1238605(A5, A17)
	.

map(config992(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17)) =
	config(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17).

% map(cfg0024341313(A1, A2, A3, A4, A5, A6, A7, A8)) =
% 	config(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15) :-
% 	A9 = ebea.player.selection.defaultParameters,
% 	A10 = '2x2',
% 	A11 = config(
% 		gl.'2x2'.game.default,
% 		gl.'2x2'.parameters.default,
% 		[initialPlayers(10, ebea.player.defaultChromosome(gl.'2x2'.strategy.default))]
% 		),
% 	A12 = config(
% 		gl.centipede.game.default,
% 		gl.centipede.parameters.default,
% 		[initialPlayers(10, ebea.player.defaultChromosome(gl.centipede.strategy.default))]
% 		),
% 	A13 = config(
% 		gl.pgp.game.default,
% 		gl.pgp.parameters.default,
% 		[initialPlayers(10, ebea.player.defaultChromosome(gl.pgp.strategy.default))]
% 		),
% 	A14 = config(
% 		gl.'pgp+pa'.game.default,
% 		gl.'pgp+pa'.parameters.default,
% 		[initialPlayers(10, ebea.player.defaultChromosome(gl.'pgp+pa'.strategy.default))]
% 		),
% 	A15 = config(
% 		gl.ultimatum.game.default,
% 		gl.ultimatum.parameters.default,
% 		[initialPlayers(10, ebea.player.defaultChromosome(gl.ultimatum.strategy.default))]
% 		)
% 	.

/**
 * Convert the config parameters from the internal representation to the
 * text stream representation.  This is the inverse function of {@code
 * map/1}.
 */

:- func pam(data.config.config) = config_file.

pam(config(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17)) =
	config992(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17).

:- func map1238605(float, config_old_1238605(G, CS, P)) = config(G, CS, P).

map1238605(CarryingCapacity, config_old_1238605(A1, A2, A3)) = Result :-
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
		X = initialPlayers_old_1238605(B1, B2),
		R^quantity = B1,
		R^chromosome = B2
	)
	.

:- end_module data.config.io.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
