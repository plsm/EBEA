/**
 * Predicates to filter version 1000 config files.

 * @author Pedro Mariano
 * @version 1.0 2014/08/13
 */
:- module data.config.io.filterV1000.

:- interface.

:- type configFile.

:- type configFileV1000 == data.config.io.filterV1000.configFile.

% :- type gameConfig(G, CS, P).

% :- type gameConfig_2x2.
% :- type gameConfig_battlesexes.
% :- type gameConfig_centipede.
% :- type gameConfig_pgp.
% :- type 'gameConfig_pgp+pa'.
% :- type gameConfig_ultimatum.

:- pred parse(configFileV1000, list(int), list(int)).
:- mode parse(in, out, in) is det.
:- mode parse(out, in, out) is semidet.

:- func map(configFileV1000) = config.

:- implementation.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

:- type configFile --->
	configFile(
		data.prng.supplyParameter,        %  1 random
		int,                              %  2 numberRuns
		int,                              %  3 numberIterations
		ebea.streams.level,               %  4 level
		float,                            %  5 carryingCapacity
		ebea.population.dynamic,          %  6 dynamic
		float,                            %  7 mutationProbability
		ebea.player.age.parameters,       %  8 ageParameters
		ebea.player.energy.parameters,    %  9 energyParameters
		ebea_player_selection_parameters, % 10 selectionParameters
		gamesV1001,	      	             % 11 selectedGame
		gameConfig_2x2,                   % 12 cfg_2x2
		gameConfig_battlesexes,           % 13 battlesexes
		gameConfig_centipede,             % 14 centipede
		gameConfig_pgp,                   % 15 pgp
		'gameConfig_pgp+pa',              % 16 pgp+pa
		gameConfig_ultimatum              % 19 ultimatum
	).

:- type gameConfig(G, CS, P) --->
	gameConfig(
		G,                            % Game
		P,                            % Parameters
		list(initialPlayersV1000(CS)) % InitialPopulation
	).

:- type gameConfigV1000(G, CS, P) == data.config.io.filterV1000.gameConfig(G, CS, P).

:- type gameConfig_2x2         == gameConfigV1000(gl.'2x2'.game.game,       gl.'2x2'.strategy.strategy,       gl.'2x2'.parameters.parameters).
:- type gameConfig_battlesexes == gameConfigV1000(gl.battlesexes.game.game, gl.battlesexes.strategy.strategy, gl.battlesexes.parameters.parameters).
:- type gameConfig_centipede   == gameConfigV1000(gl.centipede.game.game,   gl.centipede.strategy.strategy,   gl.centipede.parameters.parameters).
:- type gameConfig_pgp         == gameConfigV1000(gl.pgp.game.game,         gl.pgp.strategy.strategy,         gl.pgp.parameters.parameters).
:- type 'gameConfig_pgp+pa'    == gameConfigV1000(gl.'pgp+pa'.game.game,    gl.'pgp+pa'.strategy.strategy,    gl.'pgp+pa'.parameters.parameters).
:- type gameConfig_ultimatum   == gameConfigV1000(gl.ultimatum.game.game,   gl.ultimatum.strategy.strategy,   gl.ultimatum.parameters.parameters).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

:- type initialPlayersV1000(CS) --->
	initialPlayers(
		int,                                  % Quantity
		ebea.player.chromosome.chromosome(CS) % Chromosome 
	).

:- instance parseable(initialPlayersV1000(CS))
	<= parseable(CS)
where
[
	pred(parse/3) is parseInitialPlayers
].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

parse(C) -->
	{C = configFile(
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
	data.prng.parse(Random),
	parseable.int32(NumberRuns),
	parseable.int32(NumberIterations),
	ebea.streams.parse(Level),
	parseable.float32(CarryingCapacity),
	ebea.population.parseDynamic(Dynamic),
	parseable.float32(MutationProbability),
	ebea.player.age.parseParameters(AgeParameters),
	ebea.player.energy.parseParameters(EnergyParameters),
	data.config.io.filterV1002.parse_ebea_player_selection_parameters(SelectionParameters),
	data.config.io.filterV1001.parseGames(SelectedGame),
	parseGameConfig(Cfg_2x2),
	parseGameConfig(BattleSexes),
	parseGameConfig(Centipede),
	parseGameConfig(PGP),
	parseGameConfig(PGP_PA),
	parseGameConfig(Ultimatum)
	.

map(configFile(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17)) =
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
		map_ebea_player_selection_parameters(A10),
		data.config.io.filterV1001.mapGames(A11),
		mapGameConfig(A5, A12),
		mapGameConfig(A5, A13),
		mapGameConfig(A5, A14),
		gameConfig(
			gl.givetake.game.default,
			gl.givetake.parameters.default,
			ebea.population.configuration.default(gl.givetake.strategy.default)
		),
		gameConfig(
			gl.investment.game.default,
			gl.investment.parameter.default,
			ebea.population.configuration.default(gl.investment.strategy.default)
		),
		mapGameConfig(A5, A15),
		mapGameConfig(A5, A16),
		mapGameConfig(A5, A17)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- pred parseGameConfig(gameConfigV1000(G, CS, P), list(int), list(int))
	<= (parseable(G), parseable(CS), parseable(P)).
:- mode parseGameConfig(in, out, in) is det.
:- mode parseGameConfig(out, in, out) is semidet.

parseGameConfig(Config) -->
	{Config = gameConfig(Game, Parameters, InitialPopulation)},
	parseable.parse(Game),
	parseable.parse(Parameters),
	parseable.parseList(normalType, InitialPopulation)
	.

:- pred parseInitialPlayers(initialPlayersV1000(CS), list(int), list(int))
	<= parseable(CS).
:- mode parseInitialPlayers(in, out, in) is det.
:- mode parseInitialPlayers(out, in, out) is semidet.

parseInitialPlayers(InitialPlayers) -->
	{InitialPlayers = initialPlayers(Quantity, Chromosome)},
	parseable.int32(Quantity),
	ebea.player.chromosome.parse(Chromosome)
	.

:- func mapGameConfig(float, gameConfigV1000(G, CS, P)) = gameConfig(G, CS, P, A).

mapGameConfig(CarryingCapacity, gameConfig(A1, A2, A3)) = Result :-
	Result^game = A1,
	Result^parameters = A2,
	Result^initialPopulation = IP,
	IP^geometry = wellmixed,
	IP^sites = [Site],
	IP^defaultCarryingCapacity = float.round_to_int(CarryingCapacity),
	IP^siteDynamics = static,
	
	Site^id = 0,
	Site^carryingCapacity = float.round_to_int(CarryingCapacity),
	Site^chromosomes = list.map(Map, A3),
	Site^neighbourhood = [],
	
	Map =
	(func(X) = R :-
		X = initialPlayers(B1, B2),
		R^quantity = B1,
		R^chromosome = B2
	)
	.

:- end_module data.config.io.filterV1000.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
