/**
 * First version of the configuration file format.

 * The user can select six games: 2-player 2-action games, battle of sexes,
 * centipede, public good provision, public good provision with punishment
 * and abstaining, and ultimatum.  Only has panmictic population.

 * @author Pedro Mariano
 * @version 1.0 2014/08/13
 */
:- module data.config.io.filterV1001.

:- interface.

:- type configFile.

:- type configFileV1001 == data.config.io.filterV1001.configFile.

:- type games.

:- type gamesV1001 == data.config.io.filterV1001.games.

:- pred parse(configFileV1001, list(int), list(int)).
:- mode parse(in,  out, in)  is det.
:- mode parse(out, in,  out) is semidet.

:- pred parseGames(gamesV1001, list(int), list(int)).
:- mode parseGames(in, out, in)  is det.
:- mode parseGames(out, in, out) is semidet.

:- func map(configFileV1001) = config.

:- func mapGames(gamesV1001) = data.games.

:- implementation.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

:- type configFile --->
	configFile(
		data.prng.supplyParameter,              %  1 random
		int,                                    %  2 numberRuns
		int,                                    %  3 numberIterations
		ebea.streams.level,                     %  4 level
		ebea.population.dynamic,                %  5 dynamic
		probability,                            %  6 mutationProbability
		probability,                            %  7 migrationProbability
		ebea.player.age.parameters,             %  8 ageParameters
		ebea.player.energy.parameters,          %  9 energyParameters
		ebea_player_selection_parametersV1002,  % 10 selectionParameters
		gamesV1001,                             % 11 selectedGame
		gameConfigV1003_2x2,                    % 12 cfg_2x2
		gameConfigV1003_battlesexes,            % 13 battlesexes
		gameConfigV1003_centipede,              % 14 centipede
		gameConfigV1003_pgp,                    % 15 pgp
		'gameConfigV1003_pgp+pa',               % 16 'pgp+pa'
		gameConfigV1003_ultimatum               % 17 ultimatum
	).

:- type games --->
	'2x2' ;
	battlesexes ;
	centipede ;
	pgp ;
	'pgp+pa' ;
	ultimatum.

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
		PGP,
		PGP_PA,
		Ultimatum
	)},
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
	data.config.io.filterV1001.parseGames(SelectedGame),
	data.config.io.filterV1003.parseGameConfig(Cfg_2x2),
	data.config.io.filterV1003.parseGameConfig(BattleSexes),
	data.config.io.filterV1003.parseGameConfig(Centipede),
	data.config.io.filterV1003.parseGameConfig(PGP),
	data.config.io.filterV1003.parseGameConfig(PGP_PA),
	data.config.io.filterV1003.parseGameConfig(Ultimatum)
	.

parseGames('2x2')       --> [0].
parseGames(battlesexes) --> [1].
parseGames(centipede)   --> [2].
parseGames(pgp)         --> [3].
parseGames('pgp+pa')    --> [4].
parseGames(ultimatum)   --> [5].

mapGames('2x2')       = '2x2'.
mapGames(battlesexes) = battlesexes.
mapGames(centipede)   = centipede.
mapGames(pgp)         = pgp.
mapGames('pgp+pa')    = 'pgp+pa'.
mapGames(ultimatum)   = ultimatum.

map(configFile(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10_in, A11, A12, A13, A14, A15, A16, A17)) =
	config(
		A1, A2, A3, A4, A5, A6, A7, A8, A9,
		A10_out,
		mapGames(A11),
		mapGameConfig(A12),
		mapGameConfig(A13),
		mapGameConfig(A14),
		B1,
		B2,
		data.config.io.filterV1003.mapGameConfigPGP(A15),
		mapGameConfig(A16),
		mapGameConfig(A17))
:-
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
	A10_out = map_ebea_player_selection_parameters(A10_in)
	.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- end_module data.config.io.filterV1001.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
