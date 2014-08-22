/**
 * Provides types, predicates and functions to parse and map configuration
 * files in format <i>V1002</i>.

 * @author Pedro Mariano
 * @version 1.0 2014/08/13
 */
:- module data.config.io.filterV1002.

:- interface.

:- type configFile.

:- type configFileV1002 == data.config.io.filterV1002.configFile.

:- type ebea_player_selection_parameters.

:- pred parse(configFileV1002, list(int), list(int)).
:- mode parse(in,  out, in)  is det.
:- mode parse(out, in,  out) is semidet.

:- pred parse_ebea_player_selection_parameters(ebea_player_selection_parameters, list(int), list(int)).
:- mode parse_ebea_player_selection_parameters(in, out, in) is det.
:- mode parse_ebea_player_selection_parameters(out, in, out) is semidet.

:- func map(configFileV1002) = config.

:- func map_ebea_player_selection_parameters(ebea_player_selection_parameters) = ebea.player.selection.parameters.

:- implementation.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

:- type configFile --->
	configFile(
		data.prng.supplyParameter,        %  1 random
		int,                              %  2 numberRuns
		int,                              %  3 numberIterations
		ebea.streams.level,               %  4 level
		ebea.population.dynamic,          %  5 dynamic
		probability,                      %  6 mutationProbability
		probability,                      %  7 migrationProbability
		ebea.player.age.parameters,       %  8 ageParameters
		ebea.player.energy.parameters,    %  9 energyParameters
		ebea_player_selection_parameters, % 10 selectionParameters
		data.games,                       % 11 selectedGame
		gameConfigV1003_2x2,              % 12 cfg_2x2
		gameConfigV1003_battlesexes,      % 13 battlesexes
		gameConfigV1003_centipede,        % 14 centipede
		gameConfigV1003_givetake,         % 15 givetake
		gameConfigV1003_investment,       % 16 investment
		gameConfigV1003_pgp,              % 17 pgp
		'gameConfigV1003_pgp+pa',         % 18 pgp+pa
		gameConfigV1003_ultimatum         % 19 ultimatum
	).

:- type ebea_player_selection_parameters --->
	sp_v1002(
		poolSizeStdDev                :: float,
		bitsPerProbabilityStdDev      :: float,
		probabilityUpdateFactorStdDev :: float,
		payoffThresholdStdDev         :: float,
		
		uncertaintyIncreaseFactor     :: float,
		mu                            :: float
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

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
	parse_ebea_player_selection_parameters(SelectionParameters),
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

parse_ebea_player_selection_parameters(P) -->
	{P = sp_v1002(PoolSizeStdDev, BitsPerProbabilityStdDev, ProbabilityUpdateFactorStdDev, PayoffThresholdStdDev, UncertaintyIncreaseFactor, MU)},
	parseable.float32(PoolSizeStdDev),
	parseable.float32(BitsPerProbabilityStdDev),
	parseable.float32(ProbabilityUpdateFactorStdDev),
	parseable.float32(PayoffThresholdStdDev),
	parseable.float32(UncertaintyIncreaseFactor),
	parseable.float32(MU).

map(configFile(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10_in, A11, A12, A13, A14, A15, A16, A17, A18, A19)) =
	config(
		A1, A2, A3, A4, A5, A6, A7, A8, A9,
		A10_out,
		A11,
		mapGameConfig(A12),
		mapGameConfig(A13),
		mapGameConfig(A14),
		mapGameConfig(A15),
		mapGameConfig(A16),
%		data.config.io.filterV1003.mapGameConfigPGP(A17),
		mapGameConfig(A18),
		mapGameConfig(A19)
	) :-
	A10_out = map_ebea_player_selection_parameters(A10_in)
	.

map_ebea_player_selection_parameters(
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



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- end_module data.config.io.filterV1002.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
