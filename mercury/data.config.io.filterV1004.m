/**
 * 

 * @author Pedro Mariano
 * @version 1.0 2014/08/13
 */
:- module data.config.io.filterV1004.

:- interface.

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
		ebea.player.selection.parameters, % 10 selectionParameters
		data.games,                       % 11 selectedGame
		data.config.config_2x2,           % 12 cfg_2x2
		data.config.config_battlesexes,   % 13 battlesexes
		data.config.config_centipede,     % 14 centipede
		data.config.config_givetake,      % 15 givetake
		data.config.config_investment,    % 16 investment
		data.config.config_pgp,           % 17 pgp
		data.config.'config_pgp+pa',      % 18 pgp+pa
		data.config.config_ultimatum      % 19 ultimatum
	).

:- type configFileV1004 == data.config.io.filterV1004.configFile.

:- pred parse(configFileV1004, list(int), list(int)).
:- mode parse(in, out, in) is det.
:- mode parse(out, in, out) is semidet.

:- func map(configFileV1004) = config.

:- implementation.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

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
	data.parseGames(SelectedGame),
	parse_GameConfig(Cfg_2x2),
	parse_GameConfig(BattleSexes),
	parse_GameConfig(Centipede),
	parse_GameConfig(GiveTake),
	parse_GameConfig(Investment),
	parse_GameConfig(PGP),
	parse_GameConfig(PGP_PA),
	parse_GameConfig(Ultimatum)
	.

map(configFile(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19)) =
	config(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- pred parse_GameConfig(gameConfig(G, CS, P, MU), list(int), list(int))
	<= (parseable(G), parseable(CS), parseable(P), parseable(MU)).
:- mode parse_GameConfig(in,  out, in)  is det.
:- mode parse_GameConfig(out, in,  out) is semidet.

parse_GameConfig(Config) -->
	parseable.parse(Config^game),
	parseable.parse(Config^parameters),
	ebea.population.configuration.parse(Config^initialPopulation)
	.

:- end_module data.config.io.filterV1004.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
