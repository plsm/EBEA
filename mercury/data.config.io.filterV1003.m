/**
 * Configuration file format used in the simulations of the MABS 2014 and
 * WCCS 2014 papers.

 * @author Pedro Mariano
 * @version 1.0 2014/08/13
 */
:- module data.config.io.filterV1003.

:- interface.

:- type configFile.

:- type configFileV1003 == data.config.io.filterV1003.configFile.

:- type gameConfig(G, CS, P).

:- type gameConfigV1003(G, CS, P) == data.config.io.filterV1003.gameConfig(G, CS, P).

:- type gameConfigV1003_2x2         == gameConfigV1003(gl.'2x2'.game.game,       gl.'2x2'.strategy.strategy,       gl.'2x2'.parameters.parameters).
:- type gameConfigV1003_battlesexes == gameConfigV1003(gl.battlesexes.game.game, gl.battlesexes.strategy.strategy, gl.battlesexes.parameters.parameters).
:- type gameConfigV1003_centipede   == gameConfigV1003(gl.centipede.game.game,   gl.centipede.strategy.strategy,   gl.centipede.parameters.parameters).
:- type gameConfigV1003_givetake    == gameConfigV1003(gl.givetake.game.game,    gl.givetake.strategy.strategy,    gl.givetake.parameters.parameters).
:- type gameConfigV1003_investment  == gameConfigV1003(gl.investment.game.game,  gl.investment.strategy.strategy,  gl.investment.parameter.parameter).
:- type gameConfigV1003_pgp         == gameConfigV1003(gl.pgp.game.game,         gl.pgp.strategy.strategy,         gl_pgp_parameters).
:- type 'gameConfigV1003_pgp+pa'    == gameConfigV1003(gl.'pgp+pa'.game.game,    gl.'pgp+pa'.strategy.strategy,    gl.'pgp+pa'.parameters.parameters).
:- type gameConfigV1003_ultimatum   == gameConfigV1003(gl.ultimatum.game.game,   gl.ultimatum.strategy.strategy,   gl.ultimatum.parameters.parameters).

%% ****************************************************************
%% Previous version of type {@code gl.pgp.parameters.parameters}.
%%
:- type gl_pgp_parameters --->
	parameters(
		stdev::float
	).

:- instance parseable(gl_pgp_parameters).


:- pred parse(configFileV1003, list(int), list(int)).
:- mode parse(in,  out, in)  is det.
:- mode parse(out, in,  out) is semidet.

:- pred parseGameConfig(gameConfigV1003(G, CS, P), list(int), list(int))
	<= (parseable(G), parseable(CS), parseable(P)).
:- mode parseGameConfig(in,  out, in)  is det.
:- mode parseGameConfig(out, in,  out) is semidet.

:- func map(configFileV1003) = config.

:- func mapGameConfig(gameConfigV1003(G, CS, P)) = data.config.gameConfig(G, CS, P, unit).

:- func mapGameConfigPGP(gameConfigV1003_pgp) = data.config.config_pgp.

:- implementation.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

:-type configFile --->
	configFile(
		data.prng.supplyParameter,             %  1 random
		int,                                   %  2 numberRuns
		int,                                   %  3 numberIterations
		ebea.streams.level,                    %  4 level
		ebea.population.dynamic,               %  5 dynamic
		probability,                           %  6 mutationProbability
		probability,                           %  7 migrationProbability
		ebea.player.age.parameters,            %  8 ageParameters
		ebea.player.energy.parameters,         %  9 energyParameters
		ebea_player_selection_parametersV1004, % 10 selectionParameters
		data.games,                            % 11 selectedGame
		gameConfigV1003_2x2,                   % 12 cfg_2x2
		gameConfigV1003_battlesexes,           % 13 battlesexes
		gameConfigV1003_centipede,             % 14 centipede
		gameConfigV1003_givetake,              % 15 givetake
		gameConfigV1003_investment,            % 16 investment
		gameConfigV1003_pgp,                   % 17 pgp
		'gameConfigV1003_pgp+pa',              % 18 pgp+pa
		gameConfigV1003_ultimatum              % 19 ultimatum
	).

:- type gameConfig(G, CS, P) --->
	gameConfig(
		G,                                               % game
		P,                                               % parameters
		ebea_population_configuration_configuration(CS)  % initialPopulation
	).

:- instance parseable(gl_pgp_parameters) where
[
	pred(parse/3) is parse_gl_pgp_parameters
].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%% ****************************************************************
%% Previous version of type {@code ebea.population.configuration.configuration/1}.
%%
:- type ebea_population_configuration_configuration(CS) --->
	configuration(
		ebea.population.configuration.geometry ,               % 1 Geometry
		list(ebea.population.site.parameters.parameters(CS)) , % 2 Sites
		int                                                    % 3 DefaultCarryingCapacity
	).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

parse(C) -->
	{C = configFile(
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
	data.prng.parse(Random),
	parseable.int32(NumberRuns),
	parseable.int32(NumberIterations),
	ebea.streams.parse(Level),
	ebea.population.parseDynamic(Dynamic),
	probability.parse(MutationProbability),
	probability.parse(MigrationProbability),
	ebea.player.age.parseParameters(AgeParameters),
	ebea.player.energy.parseParameters(EnergyParameters),
	data.config.io.filterV1004.parse_ebea_player_selection_parameters(SelectionParameters),
	data.parseGames(SelectedGame),
	data.config.io.filterV1003.parseGameConfig(Cfg_2x2),
	data.config.io.filterV1003.parseGameConfig(BattleSexes),
	data.config.io.filterV1003.parseGameConfig(Centipede),
	data.config.io.filterV1003.parseGameConfig(GiveTake),
	data.config.io.filterV1003.parseGameConfig(Investment),
	data.config.io.filterV1003.parseGameConfig(PGP),
	data.config.io.filterV1003.parseGameConfig(PGP_PA),
	data.config.io.filterV1003.parseGameConfig(Ultimatum)
	.

parseGameConfig(Config) -->
	{Config = gameConfig(Game, Parameters, InitialPopulation)},
	parseable.parse(Game),
	parseable.parse(Parameters),
	parse_ebea_population_configuration_configuration(InitialPopulation)
	.

map(configFile(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19)) =
	config(
		A1, A2, A3, A4, A5, A6, A7, A8, A9,
		data.config.io.filterV1004.map_ebea_player_selection_parameters(A10),
		A11,
		mapGameConfig(A12),
		mapGameConfig(A13),
		mapGameConfig(A14),
		mapGameConfig(A15),
		mapGameConfig(A16),
		data.config.io.filterV1003.mapGameConfigPGP(A17),
		mapGameConfig(A18),
		mapGameConfig(A19)
	).

mapGameConfig(gameConfig(Game, Parameters, Configuration)) = Result :-
	Configuration = configuration(Geometry, Sites, DefaultCarryingCapacity),
	Result = gameConfig(
		Game,
		Parameters,
		configuration(
			Geometry,
			Sites,
			DefaultCarryingCapacity,
			static
		)
	).

mapGameConfigPGP(gameConfig(Game, Parameters, Configuration)) = Result :-
	Parameters = parameters(StdDev),
	Configuration = configuration(Geometry, Sites, DefaultCarryingCapacity),
	Result = gameConfig(
		Game,
		parameters(StdDev),
		configuration(
			Geometry,
			Sites,
			DefaultCarryingCapacity,
			static
		)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- pred parse_ebea_population_configuration_configuration(ebea_population_configuration_configuration(CS), list(int), list(int))
	<= parseable(CS).
:- mode parse_ebea_population_configuration_configuration(in, out, in) is det.
:- mode parse_ebea_population_configuration_configuration(out, in, out) is semidet.

parse_ebea_population_configuration_configuration(P) -->
	{P = configuration(Geometry, Sites, DefaultCarryingCapacity)},
	ebea.population.configuration.parse_geometry(Geometry),
	parseable.parseList(normalType, Sites),
	parseable.int32(DefaultCarryingCapacity)
	.


:- pred parse_gl_pgp_parameters(gl_pgp_parameters, list(int), list(int)).
:- mode parse_gl_pgp_parameters(in, out, in) is det.
:- mode parse_gl_pgp_parameters(out, in, out) is semidet.

parse_gl_pgp_parameters(P) -->
	{P = parameters(StdDev)},
	parseable.float32(StdDev)
	.

:- end_module data.config.io.filterV1003.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
