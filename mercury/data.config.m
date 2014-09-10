/**
 * This module provides types to perform a series of runs of the Energy
 * Based Evolutionary Algorithm.  There is a type, {@code config} that is
 * used to edit the parameters of a run and to save these parameters to a
 * file.

 * @author Pedro Mariano
 * @version 1.0 2013/12/07
 */
:- module data.config.

:- interface.

:- include_module %runnable,
	io, pretty.
:- import_module gl.

:- import_module gl.'2x2',       gl.'2x2'.game,       gl.'2x2'.strategy,       gl.'2x2'.parameters.
:- import_module gl.battlesexes, gl.battlesexes.game, gl.battlesexes.strategy, gl.battlesexes.parameters.
:- import_module gl.centipede,   gl.centipede.game,   gl.centipede.strategy,   gl.centipede.parameters.
:- import_module gl.givetake,    gl.givetake.game,    gl.givetake.strategy,    gl.givetake.parameters.
:- import_module gl.investment,  gl.investment.game,  gl.investment.strategy,  gl.investment.parameter.
:- import_module gl.pgp,         gl.pgp.game,         gl.pgp.strategy,         gl.pgp.parameters,        gl.pgp.action.
:- import_module gl.'pgp+pa',    gl.'pgp+pa'.game,    gl.'pgp+pa'.strategy,    gl.'pgp+pa'.parameters.
:- import_module gl.ultimatum,   gl.ultimatum.game,   gl.ultimatum.strategy,   gl.ultimatum.parameters.

:- import_module ebea, ebea.core, ebea.player, ebea.population,
ebea.population.configuration, ebea.population.site, ebea.streams,
ebea.player.age, ebea.player.energy, ebea.player.selection.
:- import_module data.prng.
:- import_module chromosome, game.
:- import_module foldable, printable.
:- import_module probability, userInterface.
:- import_module bool, io, list, unit.

/**
 * Represents the data needed to perform a run of
 * the Energy Based Evolutionary Algorithm.
  
 */
:- type config --->
	config(
		random               :: data.prng.supplyParameter,
		numberRuns           :: int,
		numberIterations     :: int,
		level                :: ebea.streams.level,
		dynamic              :: ebea.population.dynamic,
		mutationProbability  :: probability,
		migrationProbability :: probability,
		ageParameters        :: ebea.player.age.parameters,
		energyParameters     :: ebea.player.energy.parameters,
		selectionParameters  :: ebea.player.selection.parameters,
		selectedGame         :: games,
		cfg_2x2              :: config_2x2,
		battlesexes          :: config_battlesexes,
		centipede            :: config_centipede,
		givetake             :: config_givetake,
		investment           :: config_investment,
		pgp                  :: config_pgp,
		'pgp+pa'             :: 'config_pgp+pa',
		ultimatum            :: config_ultimatum
	).

:- type runMode --->
	background ;
	'2x2'(
		processPred(     gl.'2x2'.strategy.strategy,       unit) ,
		interactivePred( gl.'2x2'.strategy.strategy,       unit) ,
		processPred(     gl.'2x2'.strategy.strategy,       unit)
	) ;
	battlesexes(
		processPred(     gl.battlesexes.strategy.strategy, unit) ,
		interactivePred( gl.battlesexes.strategy.strategy, unit) ,
		processPred(     gl.battlesexes.strategy.strategy, unit)
	) ;
	centipede(
		processPred(     gl.centipede.strategy.strategy,   unit) ,
		interactivePred( gl.centipede.strategy.strategy,   unit) ,
		processPred(     gl.centipede.strategy.strategy,   unit)
	) ;
	givetake(
		processPred(     gl.givetake.strategy.strategy,   unit) ,
		interactivePred( gl.givetake.strategy.strategy,   unit) ,
		processPred(     gl.givetake.strategy.strategy,   unit)
	) ;
	investment(
		processPred(     gl.investment.strategy.strategy,   unit) ,
		interactivePred( gl.investment.strategy.strategy,   unit) ,
		processPred(     gl.investment.strategy.strategy,   unit)
	) ;
	pgp(
		processPred(     gl.pgp.strategy.strategy,         unit) ,
		interactivePred( gl.pgp.strategy.strategy,         unit) ,
		processPred(     gl.pgp.strategy.strategy,         unit)
	) ;
	'pgp+pa'(
		processPred(     gl.'pgp+pa'.strategy.strategy,    unit) ,
		interactivePred( gl.'pgp+pa'.strategy.strategy,    unit) ,
		processPred(     gl.'pgp+pa'.strategy.strategy,    unit)
	) ;
	ultimatum(
		processPred(     gl.ultimatum.strategy.strategy,   unit) ,
		interactivePred( gl.ultimatum.strategy.strategy,   unit) ,
		processPred(     gl.ultimatum.strategy.strategy,   unit)
	)
	.

:- inst runMode ==
	bound(
		background ;
		'2x2'(       processPred, interactivePred, processPred ) ;
		battlesexes( processPred, interactivePred, processPred ) ;
		centipede(   processPred, interactivePred, processPred ) ;
		givetake(    processPred, interactivePred, processPred ) ;
		investment(  processPred, interactivePred, processPred ) ;
		pgp(         processPred, interactivePred, processPred ) ;
		'pgp+pa'(    processPred, interactivePred, processPred ) ;
		ultimatum(   processPred, interactivePred, processPred )
	).

:- type gameConfig(G, CS, P, MU) --->
	gameConfig(
		game              :: G,
		parameters        :: P,
		initialPopulation :: ebea.population.configuration.configuration(CS, MU)
	).

:- type config_2x2          == gameConfig(gl.'2x2'.game.game,       gl.'2x2'.strategy.strategy,       gl.'2x2'.parameters.parameters,       unit).
:- type config_battlesexes  == gameConfig(gl.battlesexes.game.game, gl.battlesexes.strategy.strategy, gl.battlesexes.parameters.parameters, unit).
:- type config_centipede    == gameConfig(gl.centipede.game.game,   gl.centipede.strategy.strategy,   gl.centipede.parameters.parameters,   unit).
:- type config_givetake     == gameConfig(gl.givetake.game.game,    gl.givetake.strategy.strategy,    gl.givetake.parameters.parameters,    unit).
:- type config_investment   == gameConfig(gl.investment.game.game,  gl.investment.strategy.strategy,  gl.investment.parameter.parameter,    unit).
:- type config_pgp          == gameConfig(gl.pgp.game.game,         gl.pgp.strategy.strategy,         gl.pgp.parameters.parameters,         gl.pgp.action.updateSiteState).
:- type 'config_pgp+pa'     == gameConfig(gl.'pgp+pa'.game.game,    gl.'pgp+pa'.strategy.strategy,    gl.'pgp+pa'.parameters.parameters,    unit).
:- type config_ultimatum    == gameConfig(gl.ultimatum.game.game,   gl.ultimatum.strategy.strategy,   gl.ultimatum.parameters.parameters,   unit).


% :- func initSelectedGamePred(config, interactivePred(C, T)) = selectedGamePred.
% :- mode initSelectedGamePred(in, in(interactivePred)) = out(selectedGamePred) is det.

:- func default = config.

:- func dialog = list(dialogItem(config)).

:- pred errors(config, string).
:- mode errors(in, out) is nondet.

:- pred runEBEA(data.config.runMode, config, io.state, io.state).
:- mode runEBEA(in(data.config.runMode), in, di, uo) is det.

:- func random(config) = data.prng.supplyParameter.
:- func 'random :='(config, data.prng.supplyParameter) = config.

:- implementation.

:- import_module data.seed.
:- import_module ebea.population.site.parameters.
:- import_module rng, rng.distribution.
:- import_module userInterface.util.
:- import_module bool, exception, int, maybe, string.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

default = config(
	mt(clock),
	30,
	100000,
	detailedTxt,
	birthPlusDeath,
	probability.init(0.1),
	probability.init(0.1),
	ebea.player.age.defaultParameters,
	ebea.player.energy.defaultParameters,
	ebea.player.selection.defaultParameters,
	battlesexes,
	gameConfig(
		gl.'2x2'.game.default,
		gl.'2x2'.parameters.default,
		ebea.population.configuration.default(gl.'2x2'.strategy.default)
		),
	gameConfig(
		gl.battlesexes.game.default,
		gl.battlesexes.parameters.default,
		ebea.population.configuration.default(gl.battlesexes.strategy.default)
		),
	gameConfig(
		gl.centipede.game.default,
		gl.centipede.parameters.default,
		ebea.population.configuration.default(gl.centipede.strategy.default)
		),
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
	gameConfig(
		gl.pgp.game.default,
		gl.pgp.parameters.default,
		ebea.population.configuration.default(gl.pgp.strategy.default)
		),
	gameConfig(
		gl.'pgp+pa'.game.default,
		gl.'pgp+pa'.parameters.default,
		ebea.population.configuration.default(gl.'pgp+pa'.strategy.default)
		),
	gameConfig(
		gl.ultimatum.game.default,
		gl.ultimatum.parameters.default,
		ebea.population.configuration.default(gl.ultimatum.strategy.default)
		)
	).

dialog =
	[
	di(label("pseudo-random number generator"),  subdialog([data.prng.dialogItem])),
	di(label("number runs"),                     updateFieldInt(   get_numberRuns,           checkInt(   "number runs",           bounded(10, yes),  unbound, set_numberRuns))),
	di(label("number iterations"),               updateFieldInt(   get_numberIterations,     checkInt(   "number iterations",     bounded(100, yes), unbound, set_numberIterations))),
	di(label("data to store"),                   'new editField'(  get_level,                set(set_level), ebea.streams.dialog)),
	di(label("population parameters"),           subdialog(
		[
		di(
			label("birth death sequence"),
			userInterface.util.makeSelectOneOf(
				get_dynamic,
				set(set_dynamic),
				[
					label("birth then death"),
					label("death then birth"),
					label("birth and death for parents")
				],
				[
					birthThenDeath,
					deathThenBirth,
					birthPlusDeath
				])),
		di(label("mutation probability"),   dialogAction( get_mutationProbability,  set_mutationProbability)),
		di(label("migration probability"),  dialogAction( get_migrationProbability, set_migrationProbability))
		])),
	di(label("player parameters"),  subdialog(
		[
		di(label("age parameters"),        'new editField'(  get_ageParameters,        set(set_ageParameters),       ebea.player.age.dialogParameters)),
		di(label("energy parameters"),     'new editField'(  get_energyParameters,     set(set_energyParameters),    ebea.player.energy.dialogParameters)),
		di(label("selection parameters"),  'new editField'(  get_selectionParameters,  set(set_selectionParameters), ebea.player.selection.dialogParameters))
		])),
	di(label("game"),    selectOneOf(
		get_selectedGame,
		set_selectedGame,
/*		getCurrentChoice,
		setChoice,
		setData,*/
		[
		 ci(label("2 player 2 action"),  [di(label("next"), 'new editField'( get_cfg_2x2,      set(set_cfg_2x2),     dialog_2x2))]),
		 ci(label("battle of sexes"),    [di(label("next"), 'new editField'( get_battlesexes,  set(set_battlesexes), dialog_battlesexes))]),
		 ci(label("centipede"),          [di(label("next"), 'new editField'( get_centipede,    set(set_centipede),   dialog_centipede))]),
		 ci(label("givetake"),           [di(label("next"), 'new editField'( get_givetake,     set(set_givetake),    dialog_givetake))]),
		 ci(label("investment"),         [di(label("next"), 'new editField'( get_investment,   set(set_investment),  dialog_investment))]),
		 ci(label("pgp"),                [di(label("next"), 'new editField'( get_pgp,          set(set_pgp),         dialog_pgp))]),
		 ci(label("pgp+pa"),             [di(label("next"), 'new editField'( 'get_pgp+pa',     set('set_pgp+pa'),    'dialog_pgp+pa'))]),
		 ci(label("ultimatum"),          [di(label("next"), 'new editField'( get_ultimatum,    set(set_ultimatum),   dialog_ultimatum))])
		]))
	].

errors(Config, Error) :-
	Config^numberRuns =< 0,
	Error = "number of runs is not positive"
	;
	Config^numberIterations =< 0,
	Error = "number of iterations is not positive"
	;
	gl.ultimatum.game.errors(Config^ultimatum^game, Error)
	.

% initSelectedGamePred(Config, Pred) = Result :-
% 	Config^selectedGame = '2x2',
% 	Result = 'new 2x2'(Pred)
% 	;
% 	Config^selectedGame = battlesexes,
% 	Result = battlesexes(Pred)
% 	;
% 	Config^selectedGame = centipede,
% 	Result = centipede(Pred)
% 	;
% 	Config^selectedGame = pgp,
% 	Result = pgp(Pred)
% 	;
% 	Config^selectedGame = 'pgp+pa',
% 	Result = 'pgp+pa'(Pred)
% 	;
% 	Config^selectedGame = ultimatum,
% 	Result = ultimatum(Pred)
% 	.

runEBEA(RunMode, Config, !IO) :-
	(if
		RunMode = '2x2'(_, _, _),
		Config^selectedGame \= '2x2'
	;
		RunMode = battlesexes(_, _, _),
		Config^selectedGame \= battlesexes
	;
		RunMode = centipede(_, _, _),
		Config^selectedGame \= centipede
	;
		RunMode = givetake(_, _, _),
		Config^selectedGame \= givetake
	;
		RunMode = investment(_, _, _),
		Config^selectedGame \= investment
	;
		RunMode = pgp(_, _, _),
		Config^selectedGame \= pgp
	;
		RunMode = 'pgp+pa'(_, _, _),
		Config^selectedGame \= 'pgp+pa'
	;
		RunMode = ultimatum(_, _, _),
		Config^selectedGame \= ultimatum
	% ;
	% 	RunMode = (_, _, _),
	% 	Config^selectedGame \= 
	then
		io.format(io.stderr_stream, "Overriding selected game %s.\n", [s(string(Config^selectedGame))], !IO)
	else
		true
	),
	runVS1(RunMode, Config, !IO)
	.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- func getCurrentChoice(config) = maybe(currentChoice(config)).

getCurrentChoice(Config) = Result :-
	trace [io(!IO)] (io.format("getCurrentChoice/1\n", [], !IO)),
	selectedGameIndex(Config^selectedGame, Index),
	Result = yes(cc(Index, Config))
	.

:- pred selectedGameIndex(games, int).
:- mode selectedGameIndex(in, out) is det.
:- mode selectedGameIndex(out, in) is semidet.

selectedGameIndex('2x2',       0).
selectedGameIndex(battlesexes, 1).
selectedGameIndex(centipede,   2).
selectedGameIndex(givetake,    3).
selectedGameIndex(investment,  4).
selectedGameIndex(pgp,         5).
selectedGameIndex('pgp+pa',    6).
selectedGameIndex(ultimatum,   7).

:- func setChoice(config, int) = setResult(selectChoice(config, config)).

setChoice(Config, Index) = ok(sc(NextConfig, NextConfig)) :-
	(if
		selectedGameIndex(Games, Index)
	then
		NextConfig = 'selectedGame :='(Config, Games)
	else
		throw("setChoice/2: Invalid index")
	).

:- func setData(config, config) = setResult(config).

setData(_, Config) = ok(Config).

%% ************************************************************************
%% runVS1(Config, !IO)
%%
%% The first step of an EBEA run is to initialise the pseudo-random number
%% generator.  The initialisation may fail because of an invalid seed.
%%
:- pred runVS1(
	data.config.runMode :: in(data.config.runMode),
	config              :: in,
	io.state :: di,  io.state :: uo
) is det.

runVS1(RunMode, Config, !IO) :-
	data.prng.init(Config^random, MRandom, !IO),
	(	%
		MRandom = ok({Supply, Seed}),
		Supply = supply(Random),
		io.format(io.stderr_stream, "RANDOM SEED %d\n", [i(Seed)], !IO),
		int.fold_up2(runVS2(RunMode, Config), 1, Config^numberRuns, Random, _, !IO)
		;
		MRandom = error(Msg),
		io.print(io.stderr_stream, Msg, !IO),
		io.nl(io.stderr_stream, !IO)
	).

%% ************************************************************************
%% runVS2(Config, RunIndex, !Random, !IO)
%%
%% The second step of an EBEA run is to open the output streams for the
%% given run index.
%%
:- pred runVS2(
	data.config.runMode :: in(data.config.runMode),
	config              :: in,
	int                 :: in,
	R        :: in,  R        :: out,
	io.state :: di,  io.state :: uo
) is det
	<= (
	ePRNG(R)
).

runVS2(RunMode, Config, RunIndex, !Random, !IO) :-
	io.format(io.stderr_stream, "Run %d ", [i(RunIndex)], !IO),
	io.flush_output(io.stderr_stream, !IO),
	ebea.streams.openOutputStreams(Config^level, yes(string.format("_R%d", [i(RunIndex)])), IMStreams, !IO),
	(
		IMStreams = ok(Streams),
		runVS3(RunMode, Config, Streams, !Random, !IO),
		io.print(io.stderr_stream, "finished\n", !IO)
		;
		IMStreams = error(Msg),
		io.print(io.stderr_stream, Msg, !IO),
		io.nl(io.stderr_stream, !IO)
	).

%% ************************************************************************
%% runVS3(Config, RunIndex, !Random, !IO)
%%
%% The third step of an EBEA run is to switch to the selected game.
%%
:- pred runVS3(
	data.config.runMode     :: in(data.config.runMode),
	config                  :: in,
	ebea.streams.outStreams :: in,
	R        :: in,  R        :: out,
	io.state :: di,  io.state :: uo
) is det
	<= (
	ePRNG(R)
).

runVS3(RunMode, Config, Streams, !Random, !IO) :-
	RunMode = background,
	(
		Config^selectedGame = '2x2',
		runVS4Game2(background, Config, Config^cfg_2x2, Streams, !Random, !IO)
	;
		Config^selectedGame = battlesexes,
		runVS4Game2(background, Config, Config^battlesexes, Streams, !Random, !IO)
	;
		Config^selectedGame = centipede,
		runVS4Game2(background, Config, Config^centipede, Streams, !Random, !IO)
	;
		Config^selectedGame = givetake,
		runVS4Game2(background, Config, Config^givetake, Streams, !Random, !IO)
	;
		Config^selectedGame = investment,
		runVS4Game2(background, Config, Config^investment, Streams, !Random, !IO)
	;
		Config^selectedGame = pgp,
		runVS4Game3(background, Config, Config^pgp, Streams, gl.pgp.action.mapUpdateSiteState, !Random, !IO)
	;
		Config^selectedGame = 'pgp+pa',
		runVS4Game2(background, Config, Config^'pgp+pa', Streams, !Random, !IO)
	;
		Config^selectedGame = ultimatum,
		runVS4Game2(background, Config, Config^ultimatum, Streams, !Random, !IO)
	)
	;
	RunMode = '2x2'(FirstPred, IteraPred, FinalPred),
	runVS4Game2(interactively(FirstPred, IteraPred, FinalPred), Config, Config^cfg_2x2, Streams, !Random, !IO)
	;
	RunMode = battlesexes(FirstPred, IteraPred, FinalPred),
	runVS4Game2(interactively(FirstPred, IteraPred, FinalPred), Config, Config^battlesexes, Streams, !Random, !IO)
	;
	RunMode = centipede(FirstPred, IteraPred, FinalPred),
	runVS4Game2(interactively(FirstPred, IteraPred, FinalPred), Config, Config^centipede, Streams, !Random, !IO)
	;
	RunMode = givetake(FirstPred, IteraPred, FinalPred),
	runVS4Game2(interactively(FirstPred, IteraPred, FinalPred), Config, Config^givetake, Streams, !Random, !IO)
	;
	RunMode = investment(FirstPred, IteraPred, FinalPred),
	runVS4Game2(interactively(FirstPred, IteraPred, FinalPred), Config, Config^investment, Streams, !Random, !IO)
	;
	RunMode = pgp(FirstPred, IteraPred, FinalPred),
	runVS4Game3(
		interactively(FirstPred, IteraPred, FinalPred),
		Config,
		Config^pgp,
		Streams,
		gl.pgp.action.mapUpdateSiteState,
		!Random,
		!IO
	)
	;
	RunMode = 'pgp+pa'(FirstPred, IteraPred, FinalPred),
	runVS4Game2(interactively(FirstPred, IteraPred, FinalPred), Config, Config^'pgp+pa', Streams, !Random, !IO)
	;
	RunMode = ultimatum(FirstPred, IteraPred, FinalPred),
	GameConfig0 = Config^ultimatum,
	GameConfig1 =
		'parameters :='(GameConfig0,
			'cakeSizeCopy :='(GameConfig0^parameters, GameConfig0^game^cakeSize)),
	runVS4Game2(interactively(FirstPred, IteraPred, FinalPred), Config, GameConfig1, Streams, !Random, !IO)
	.

%% ************************************************************************
%% runVS4Game2(Config, RunIndex, !Random, !IO)
%%
%% The fourth step of an EBEA run is to call {@code ebea.core.runGame2/12}
%% for games that implement the {@code asymmetricGame/2} type-class.
%%
:- pred runVS4Game2(
	ebea.core.runMode(CS, T)   :: in(ebea.core.runMode),
	config                     :: in,
	gameConfig(G, CS, P, unit) :: in,
	ebea.streams.outStreams    :: in,
	R        :: in,  R        :: out,
	io.state :: di,  io.state :: uo
) is det
	<= (
	ePRNG(R),
	asymmetricGame(G, CS),
	chromosome(CS, T, P),
	foldable(CS, ACS),
	parseable(CS),
	printable(CS),
	printable(T),
	printable(ACS)
).

runVS4Game2(RunMode, AllConfig, GameConfig, Streams, !Random, !IO) :-
	PlayerParameters^mutationProbability = float(AllConfig^mutationProbability),
	PlayerParameters^agePar = AllConfig^ageParameters,
	PlayerParameters^energyPar = AllConfig^energyParameters,
	PlayerParameters^selectionPar = AllConfig^selectionParameters,
	PlayerParameters^gamePar = GameConfig^parameters,

	Parameters^migrationProbability = float(AllConfig^migrationProbability),
	Parameters^dynamic = AllConfig^dynamic,
	Parameters^playerParameters = PlayerParameters,
	
	ebea.population.createInitialPopulation(PlayerParameters, GameConfig^initialPopulation, Population, !Random),
	ebea.core.runGame2(RunMode, GameConfig^game, Parameters, Streams, AllConfig^numberIterations, Population, rng.distribution.init, _, !Random, !IO),
	ebea.streams.closeOutputStreams(Streams, !IO)
	.

%% ************************************************************************
%% runVS4Game3(Config, RunIndex, !Random, !IO)
%%
%% The fourth step of an EBEA run is to call {@code ebea.core.runGame2/12}
%% for games that implement the {@code asymmetricGame/3} type-class.
%%
:- pred runVS4Game3(
	ebea.core.runMode(CS, T)   :: in(ebea.core.runMode),
	config                     :: in,
	gameConfig(G, CS, P, MU)   :: in,
	ebea.streams.outStreams    :: in,
	func(MU) = updateState(AA) :: in,
	R        :: in,  R        :: out,
	io.state :: di,  io.state :: uo
) is det
	<= (
	ePRNG(R),
	asymmetricGame(G, CS, A),
	chromosome(CS, T, P),
	foldable(CS, ACS),
	foldable(A, AA),
	parseable(CS),
	printable(CS),
	printable(T),
	printable(ACS)
).

runVS4Game3(RunMode, AllConfig, GameConfig, Streams, MapUpdateState, !Random, !IO) :-
	PlayerParameters^mutationProbability = float(AllConfig^mutationProbability),
	PlayerParameters^agePar = AllConfig^ageParameters,
	PlayerParameters^energyPar = AllConfig^energyParameters,
	PlayerParameters^selectionPar = AllConfig^selectionParameters,
	PlayerParameters^gamePar = GameConfig^parameters,

	Base^migrationProbability = float(AllConfig^migrationProbability),
	Base^dynamic = AllConfig^dynamic,
	Base^playerParameters = PlayerParameters,
	PopulationParameters^base = Base,

	GameConfig^initialPopulation^siteDynamics = SiteDynamics,
	(	%
		SiteDynamics = static,
		PopulationParameters^siteDynamics = static
	;
		SiteDynamics = dynamic(D),
		PopulationParameters^siteDynamics = dynamic(MapUpdateState(D))
	),

	ebea.population.createInitialPopulation(
		PlayerParameters,
		GameConfig^initialPopulation,
		Population,
		!Random
	),
	ebea.core.runGame3(
		RunMode,
		GameConfig^game,
		PopulationParameters,
		Streams,
		AllConfig^numberIterations,
		Population,
		rng.distribution.init, _,
		!Random,
		!IO
	),
	ebea.streams.closeOutputStreams(Streams, !IO)
	.



% :- pred callErrors(list(pred(string)), maybe(string), maybe(string)).
% :- mode callErrors(in(list_skel(pred(out) is semidet)), in, out) is det.
% :- mode callErrors(in(list_skel(pred(out) is nondet)), in, out) is det.

% callErrors([], !MErrors).
% callErrors([PredError | RestPreds], !MErrors) :-
% 	callError(PredError, !MErrors),
% 	callErrors(RestPreds, !MErrors).


% :- pred callError(pred(string), maybe(string), maybe(string)).
% :- mode callError(in(pred(out) is nondet), in, out) is det.

% callError(Pred, !MErrors) :-
% 	solutions.solutions(Pred, ListErrors)
% 	(if
% 		Pred(Msg)
% 	then
% 		!.MErrors = no,
% 		!:MErrors = yes(Msg)
% 		;
% 		!.MErrors = yes(Msgs),
% 		!:MErrors = yes(Msgs ++ "\n" ++ Msg)
% 	else
% 		true
% 	).

% :- pred callError(pred(string), maybe(string), maybe(string)).
% :- mode callError(in(pred(out) is nondet), in, out) is det.

% callError(Pred, !MErrors) :-
% 	(if
% 		Pred(Msg)
% 	then
% 		!.MErrors = no,
% 		!:MErrors = yes(Msg)
% 		;
% 		!.MErrors = yes(Msgs),
% 		!:MErrors = yes(Msgs ++ "\n" ++ Msg)
% 	else
% 		true
% 	).


:- func dialog(
	list(dialogItem(G)),
	list(dialogItem(P)),
	list(dialogItem(CS)),
	CS
	) = list(dialogItem(gameConfig(G, CS, P, unit))).

dialog(DialogGame, DialogParameters, DialogStrategyChromosome, DefaultStrategyChromosome) =
	[
	di(label("game parameters"),      'new editField'(get_game,       set_game,       DialogGame)),
	di(label("general parameters"),   'new editField'(get_parameters, set_parameters, DialogParameters)),
	di(label("initial population"),
		'new editField'(
			get_initialPopulation,
			set_initialPopulation,
			ebea.population.configuration.dialog(DefaultStrategyChromosome, DialogStrategyChromosome)))
	].

:- func dialog(
	list(dialogItem(G)),
	list(dialogItem(P)),
	list(dialogItem(CS)),
	CS,
	list(dialogItem(MU)),
	MU
	) = list(dialogItem(gameConfig(G, CS, P, MU))).

dialog(	 
	DialogGame,
	DialogParameters,
	DialogStrategyChromosome,
	DefaultStrategyChromosome,
	DialogSiteUpdateFunctions,
	DefaultSiteUpdateFunction
	) =
	[
	di(label("game parameters"),      'new editField'(get_game,       set_game,       DialogGame)),
	di(label("general parameters"),   'new editField'(get_parameters, set_parameters, DialogParameters)),
	di(label("initial population"),
		'new editField'(
			get_initialPopulation,
			set_initialPopulation,
			ebea.population.configuration.dialog(
				DefaultStrategyChromosome, DialogStrategyChromosome,
				DefaultSiteUpdateFunction, DialogSiteUpdateFunctions)))
	].


:- func dialog_2x2 = list(dialogItem(config_2x2)).

dialog_2x2 = dialog(
	gl.'2x2'.game.dialog,
	gl.'2x2'.parameters.dialog,
	gl.'2x2'.strategy.dialog,
	gl.'2x2'.strategy.default).

:- func dialog_battlesexes = list(dialogItem(config_battlesexes)).

dialog_battlesexes = dialog(
	gl.battlesexes.game.dialog,
	gl.battlesexes.parameters.dialog,
	gl.battlesexes.strategy.dialog,
	gl.battlesexes.strategy.default).

:- func dialog_centipede = list(dialogItem(config_centipede)).

dialog_centipede = dialog(
	gl.centipede.game.dialog,
	gl.centipede.parameters.dialog,
	gl.centipede.strategy.dialog,
	gl.centipede.strategy.default).

:- func dialog_givetake = list(dialogItem(config_givetake)).

dialog_givetake = dialog(
	gl.givetake.game.dialog,
	gl.givetake.parameters.dialog,
	gl.givetake.strategy.dialog,
	gl.givetake.strategy.default).

:- func dialog_investment = list(dialogItem(config_investment)).

dialog_investment = dialog(
	gl.investment.game.dialog,
	gl.investment.parameter.dialog,
	gl.investment.strategy.dialog,
	gl.investment.strategy.default).

:- func dialog_pgp = list(dialogItem(config_pgp)).

dialog_pgp = dialog(
	gl.pgp.game.dialog,
	gl.pgp.parameters.dialog,
	gl.pgp.strategy.dialog,
	gl.pgp.strategy.default,
	gl.pgp.action.dialogSiteUpdateFunction,
	gl.pgp.action.defaultSiteUpdateFunction).

:- func 'dialog_pgp+pa' = list(dialogItem('config_pgp+pa')).

'dialog_pgp+pa' = dialog(
	gl.'pgp+pa'.game.dialog,
	gl.'pgp+pa'.parameters.dialog,
	gl.'pgp+pa'.strategy.dialog,
	gl.'pgp+pa'.strategy.default).

:- func dialog_ultimatum = list(dialogItem(config_ultimatum)).

dialog_ultimatum = dialog(
	gl.ultimatum.game.dialog,
	gl.ultimatum.parameters.dialog,
	gl.ultimatum.strategy.dialog,
	gl.ultimatum.strategy.default).







                                                          % getters and setters for gameConfig/4

:- func get_game(gameConfig(G, CS, P, T)) = G.

get_game(Config) = Config^game.

:- func set_game(gameConfig(G, CS, P, T), G) = setResult(gameConfig(G, CS, P, T)).

set_game(Config, Game) = ok('game :='(Config, Game)).

:- func get_parameters(gameConfig(G, CS, P, T)) = P.

get_parameters(Config) = Config^parameters.

:- func set_parameters(gameConfig(G, CS, P, T), P) = setResult(gameConfig(G, CS, P, T)).

set_parameters(Config, Parameters) = ok('parameters :='(Config, Parameters)).

:- func get_initialPopulation(gameConfig(G, CS, P, T)) = ebea.population.configuration.configuration(CS, T).

get_initialPopulation(Config) = Config^initialPopulation.

:- func set_initialPopulation(gameConfig(G, CS, P, T), ebea.population.configuration.configuration(CS, T)) = setResult(gameConfig(G, CS, P, T)).

set_initialPopulation(Config, InitialPopulation) = ok('initialPopulation :='(Config, InitialPopulation)).



                                                          % getters and setters for config

:- func get_random(config) = data.prng.supplyParameter.

get_random(P) = P^random.


:- func set_random(config, data.prng.supplyParameter) = config.

set_random(P, V) = 'random :='(P, V).



:- func get_numberRuns(config) = int.

get_numberRuns(P) = P^numberRuns.


:- func set_numberRuns(config, int) = config.

set_numberRuns(P, V) = 'numberRuns :='(P, V).


:- func get_numberIterations(config) = int.

get_numberIterations(P) = P^numberIterations.


:- func set_numberIterations(config, int) = config.

set_numberIterations(P, V) = 'numberIterations :='(P, V).



:- func get_level(config) = ebea.streams.level.

get_level(P) = P^level.


:- func set_level(config, ebea.streams.level) = config.

set_level(P, V) = 'level :='(P, V).



:- func get_dynamic(config) = ebea.population.dynamic.

get_dynamic(P) = P^dynamic.


:- func set_dynamic(config, ebea.population.dynamic) = config.

set_dynamic(P, V) = 'dynamic :='(P, V).



:- func get_mutationProbability(config) = probability.

get_mutationProbability(P) = P^mutationProbability.


:- func set_mutationProbability(config, probability) = setResult(config).

set_mutationProbability(P, V) = ok('mutationProbability :='(P, V)).


:- func get_migrationProbability(config) = probability.

get_migrationProbability(P) = P^migrationProbability.


:- func set_migrationProbability(config, probability) = setResult(config).

set_migrationProbability(P, V) = ok('migrationProbability :='(P, V)).



:- func get_ageParameters(config) = ebea.player.age.parameters.

get_ageParameters(P) = P^ageParameters.


:- func set_ageParameters(config, ebea.player.age.parameters) = config.

set_ageParameters(P, V) = 'ageParameters :='(P, V).



:- func get_energyParameters(config) = ebea.player.energy.parameters.

get_energyParameters(P) = P^energyParameters.


:- func set_energyParameters(config, ebea.player.energy.parameters) = config.

set_energyParameters(P, V) = 'energyParameters :='(P, V).



:- func get_selectionParameters(config) = ebea.player.selection.parameters.

get_selectionParameters(P) = P^selectionParameters.


:- func set_selectionParameters(config, ebea.player.selection.parameters) = config.

set_selectionParameters(P, V) = 'selectionParameters :='(P, V).



:- func get_selectedGame(config) = maybe(int).

get_selectedGame(P) = Result :-
	selectedGameIndex(P^selectedGame, Index),
	Result = yes(Index)
	.


:- func set_selectedGame(config, int) = setResult(config).

set_selectedGame(P, Index) = Result :-
	(if
		selectedGameIndex(SelectedGame, Index)
	then
		Result = ok('selectedGame :='(P, SelectedGame))
	else
		Result = throw("data.config.set_selectedGame/2: invalid index")
	).



:- func get_cfg_2x2(config) = config_2x2.

get_cfg_2x2(P) = P^cfg_2x2.


:- func set_cfg_2x2(config, config_2x2) = config.

set_cfg_2x2(P, V) = 'cfg_2x2 :='('selectedGame :='(P, '2x2'), V).



:- func get_battlesexes(config) = config_battlesexes.

get_battlesexes(P) = P^battlesexes.


:- func set_battlesexes(config, config_battlesexes) = config.

set_battlesexes(P, V) = 'battlesexes :='('selectedGame :='(P, battlesexes), V).



:- func get_centipede(config) = config_centipede.

get_centipede(P) = P^centipede.


:- func set_centipede(config, config_centipede) = config.

set_centipede(P, V) = 'centipede :='('selectedGame :='(P, centipede), V).



:- func get_givetake(config) = config_givetake.

get_givetake(P) = P^givetake.


:- func set_givetake(config, config_givetake) = config.

set_givetake(P, V) = 'givetake :='('selectedGame :='(P, givetake), V).



:- func get_investment(config) = config_investment.

get_investment(P) = P^investment.


:- func set_investment(config, config_investment) = config.

set_investment(P, V) = 'investment :='('selectedGame :='(P, investment), V).



:- func get_pgp(config) = config_pgp.

get_pgp(P) = P^pgp.


:- func set_pgp(config, config_pgp) = config.

set_pgp(P, V) = 'pgp :='('selectedGame :='(P, pgp), V).


:- func 'get_pgp+pa'(config) = 'config_pgp+pa'.

'get_pgp+pa'(P) = P^'pgp+pa'.


:- func 'set_pgp+pa'(config, 'config_pgp+pa') = config.

'set_pgp+pa'(P, V) = 'pgp+pa :='('selectedGame :='(P, 'pgp+pa'), V).



:- func get_ultimatum(config) = config_ultimatum.

get_ultimatum(P) = P^ultimatum.


:- func set_ultimatum(config, config_ultimatum) = config.

set_ultimatum(P, V) = 'ultimatum :='('selectedGame :='(P, ultimatum), V).

:- end_module data.config.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
