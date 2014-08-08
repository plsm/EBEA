/**
 * This module provides types to perform a series of runs of the Energy
 * Based Evolutionary Algorithm.  There is a type, {@code config} that is
 * used to edit the parameters of a run and to save these parameters to a
 * file.

 * @author Pedro Mariano
 * @version 1.0 2013/12/ 7
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
:- import_module gl.pgp,         gl.pgp.game,         gl.pgp.strategy,         gl.pgp.parameters.
:- import_module gl.'pgp+pa',    gl.'pgp+pa'.game,    gl.'pgp+pa'.strategy,    gl.'pgp+pa'.parameters.
:- import_module gl.ultimatum,   gl.ultimatum.game,   gl.ultimatum.strategy,   gl.ultimatum.parameters.

:- import_module ebea, ebea.core, ebea.player, ebea.population, ebea.population.parameters, ebea.streams, ebea.player.age, ebea.player.energy, ebea.player.selection.
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

:- type selectedGamePred --->
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
%	investment(  processPred(gl.investment.strategy.strategy,  unit) ) ;
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
% 	'2x2'(       pred(population(gl.'2x2'.strategy.strategy,       unit), bool, io.state, io.state) ) ;
% 	battlesexes( pred(population(gl.battlesexes.strategy.strategy, unit), bool, io.state, io.state) ) ;
% 	centipede(   pred(population(gl.centipede.strategy.strategy,   unit), bool, io.state, io.state) ) ;
% %	investment(  pred(population(gl.investment.strategy.strategy,  unit), bool, io.state, io.state) ) ;
% 	pgp(         pred(population(gl.pgp.strategy.strategy,         unit), bool, io.state, io.state) ) ;
% 	'pgp+pa'(    pred(population(gl.'pgp+pa'.strategy.strategy,    unit), bool, io.state, io.state) ) ;
% 	ultimatum(   pred(population(gl.ultimatum.strategy.strategy,   unit), bool, io.state, io.state) )
	.

:- inst selectedGamePred ==
	bound(
		'2x2'(       processPred, interactivePred, processPred ) ;
		battlesexes( processPred, interactivePred, processPred ) ;
		centipede(   processPred, interactivePred, processPred ) ;
		givetake(    processPred, interactivePred, processPred ) ;
		investment(  processPred, interactivePred, processPred ) ;
		pgp(         processPred, interactivePred, processPred ) ;
		'pgp+pa'(    processPred, interactivePred, processPred ) ;
		ultimatum(   processPred, interactivePred, processPred )
		% '2x2'(       pred(in, out, di, uo) is det ) ;
		% battlesexes( pred(in, out, di, uo) is det ) ;
		% centipede(   pred(in, out, di, uo) is det ) ;
		% pgp(         pred(in, out, di, uo) is det ) ;
		% 'pgp+pa'(    pred(in, out, di, uo) is det ) ;
		% ultimatum(   pred(in, out, di, uo) is det )
	).

% :- type gameSet --->
% 	gameSet(
% 		selectedGame         :: games,
% 		cfg_2x2              :: config_2x2,
% 		battlesexes          :: config_battlesexes,
% 		centipede            :: config_centipede,
% 		pgp                  :: config_pgp,
% 		'pgp+pa'             :: 'config_pgp+pa',
% 		ultimatum            :: config_ultimatum
% 	).



:- type gameConfig(G, CS, P) --->
	gameConfig(
		game              :: G,
		parameters        :: P,
		initialPopulation :: ebea.population.parameters.parameters(CS)
	).


:- type config_2x2          == gameConfig(gl.'2x2'.game.game,       gl.'2x2'.strategy.strategy,       gl.'2x2'.parameters.parameters).
:- type config_battlesexes  == gameConfig(gl.battlesexes.game.game, gl.battlesexes.strategy.strategy, gl.battlesexes.parameters.parameters).
:- type config_centipede    == gameConfig(gl.centipede.game.game,   gl.centipede.strategy.strategy,   gl.centipede.parameters.parameters).
:- type config_givetake     == gameConfig(gl.givetake.game.game,    gl.givetake.strategy.strategy,    gl.givetake.parameters.parameters).
:- type config_investment   == gameConfig(gl.investment.game.game,  gl.investment.strategy.strategy,  gl.investment.parameter.parameter).
:- type config_pgp          == gameConfig(gl.pgp.game.game,         gl.pgp.strategy.strategy,         gl.pgp.parameters.parameters).
:- type 'config_pgp+pa'     == gameConfig(gl.'pgp+pa'.game.game,    gl.'pgp+pa'.strategy.strategy,    gl.'pgp+pa'.parameters.parameters).
:- type config_ultimatum    == gameConfig(gl.ultimatum.game.game,   gl.ultimatum.strategy.strategy,   gl.ultimatum.parameters.parameters).

/*
:- type gameConfig --->
	some [G, CS, T, P, A] (gameConfig(gameConfig(G, CS, P))
	=> (
		asymmetricGame(G, CS),
			chromosome(CS, T, P),
			foldable(CS, A),
			parseable(CS),
			printable(CS),
			printable(T),
			printable(A))).
*/

% :- func initSelectedGamePred(config, interactivePred(C, T)) = selectedGamePred.
% :- mode initSelectedGamePred(in, in(interactivePred)) = out(selectedGamePred) is det.

:- func default = config.

:- func dialog = list(dialogItem(config)).

:- pred errors(config, string).
:- mode errors(in, out) is nondet.

:- pred runBackground(config, io.state, io.state).
:- mode runBackground(in, di, uo) is det.

:- pred runInteractively(selectedGamePred, config, io.state, io.state).
:- mode runInteractively(in(selectedGamePred), in, di, uo) is det.

:- func random(config) = data.prng.supplyParameter.
:- func 'random :='(config, data.prng.supplyParameter) = config.

:- implementation.

:- import_module data.seed.
:- import_module rng, rng.distribution.
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
	pgp,
	gameConfig(
		gl.'2x2'.game.default,
		gl.'2x2'.parameters.default,
		ebea.population.parameters.default(gl.'2x2'.strategy.default)
		),
	gameConfig(
		gl.battlesexes.game.default,
		gl.battlesexes.parameters.default,
		ebea.population.parameters.default(gl.battlesexes.strategy.default)
		),
	gameConfig(
		gl.centipede.game.default,
		gl.centipede.parameters.default,
		ebea.population.parameters.default(gl.centipede.strategy.default)
		),
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
	gameConfig(
		gl.pgp.game.default,
		gl.pgp.parameters.default,
		ebea.population.parameters.default(gl.pgp.strategy.default)
		),
	gameConfig(
		gl.'pgp+pa'.game.default,
		gl.'pgp+pa'.parameters.default,
		ebea.population.parameters.default(gl.'pgp+pa'.strategy.default)
		),
	gameConfig(
		gl.ultimatum.game.default,
		gl.ultimatum.parameters.default,
		ebea.population.parameters.default(gl.ultimatum.strategy.default)
		)
	).

dialog =
	[
%	di(label("pseudo-random number generator"),  'new editField'(  get_random,               set(set_random), data.prng.dialog)),
	di(label("pseudo-random number generator"),  subdialog([data.prng.dialogItem])),
	di(label("number runs"),                     updateFieldInt(   get_numberRuns,           checkInt(   "number runs",           bounded(10, yes),  unbound, set_numberRuns))),
	di(label("number iterations"),               updateFieldInt(   get_numberIterations,     checkInt(   "number iterations",     bounded(100, yes), unbound, set_numberIterations))),
	di(label("data to store"),                   'new editField'(  get_level,                set(set_level), ebea.streams.dialog)),
	di(label("population parameters"),           subdialog(
		[
		di(label("birth death sequence"),   'new editField'( get_dynamic, set(set_dynamic),
			[
			di(label("birth then death"),             newValue(birthThenDeath)),
 			di(label("death then birth"),             newValue(deathThenBirth)),
			di(label("birth and death for parents"),  newValue(birthPlusDeath))
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
	di(label("game"),    'new selectOneOf'(
		getCurrentChoice,
		setChoice,
		setData,
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
	% di(label("2 player 2 action"),    'new editField'(  get_cfg_2x2,              set(set_cfg_2x2),   dialog_2x2)),
	% di(label("battle of sexes"),      'new editField'(  get_battlesexes,          set(set_battlesexes), dialog_battlesexes)),
	% di(label("centipede"),            'new editField'(  get_centipede,            set(set_centipede), dialog_centipede)),
	% di(label("pgp"),                  'new editField'(  get_pgp,                  set(set_pgp),       dialog_pgp)),
	% di(label("pgp+pa"),               'new editField'(  'get_pgp+pa',             set('set_pgp+pa'),  'dialog_pgp+pa')),
	% di(label("ultimatum"),            'new editField'(  get_ultimatum,            set(set_ultimatum), dialog_ultimatum))
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
	
runBackground(Config, !IO) :-
	Config^selectedGame = '2x2',
	run_s1(background, Config, Config^cfg_2x2, !IO)
	;
	Config^selectedGame = battlesexes,
	run_s1(background, Config, Config^battlesexes, !IO)
	;
	Config^selectedGame = centipede,
	run_s1(background, Config, Config^centipede, !IO)
	;
	Config^selectedGame = givetake,
	run_s1(background, Config, Config^givetake, !IO)
	;
	Config^selectedGame = investment,
	run_s1(background, Config, Config^investment, !IO)
	;
	Config^selectedGame = pgp,
	run_s1(background, Config, Config^pgp, !IO)
	;
	Config^selectedGame = 'pgp+pa',
	run_s1(background, Config, Config^'pgp+pa', !IO)
	;
	Config^selectedGame = ultimatum,
	run_s1(background, Config, Config^ultimatum, !IO)
	.


runInteractively(SelectedGamePred, Config, !IO) :-
	SelectedGamePred = '2x2'(FirstPred, IteraPred, FinalPred),
	run_s1(interactively(FirstPred, IteraPred, FinalPred), Config, Config^cfg_2x2, !IO)
	;
	SelectedGamePred = battlesexes(FirstPred, IteraPred, FinalPred),
	run_s1(interactively(FirstPred, IteraPred, FinalPred), Config, Config^battlesexes, !IO)
	;
	SelectedGamePred = centipede(FirstPred, IteraPred, FinalPred),
	run_s1(interactively(FirstPred, IteraPred, FinalPred), Config, Config^centipede, !IO)
	;
	SelectedGamePred = givetake(FirstPred, IteraPred, FinalPred),
	run_s1(interactively(FirstPred, IteraPred, FinalPred), Config, Config^givetake, !IO)
	;
	SelectedGamePred = investment(FirstPred, IteraPred, FinalPred),
	run_s1(interactively(FirstPred, IteraPred, FinalPred), Config, Config^investment, !IO)
	;
	SelectedGamePred = pgp(FirstPred, IteraPred, FinalPred),
	run_s1(interactively(FirstPred, IteraPred, FinalPred), Config, Config^pgp, !IO)
	;
	SelectedGamePred = 'pgp+pa'(FirstPred, IteraPred, FinalPred),
	run_s1(interactively(FirstPred, IteraPred, FinalPred), Config, Config^'pgp+pa', !IO)
	;
	SelectedGamePred = ultimatum(FirstPred, IteraPred, FinalPred),
	GameConfig0 = Config^ultimatum,
	GameConfig1 =
		'parameters :='(GameConfig0,
			'cakeSizeCopy :='(GameConfig0^parameters, GameConfig0^game^cakeSize)),
	run_s1(interactively(FirstPred, IteraPred, FinalPred), Config, GameConfig1, !IO)
	.

% runAnonymous(Pred, Config, !IO) :-
% 	Config^selectedGame = '2x2',
% 	run_s1(interactively(Pred), Config, Config^cfg_2x2, !IO)
% 	;
% 	Config^selectedGame = battlesexes,
% 	run_s1(interactively(Pred), Config, Config^battlesexes, !IO)
% 	;
% 	Config^selectedGame = centipede,
% 	run_s1(interactively(Pred), Config, Config^centipede, !IO)
% 	;
% 	Config^selectedGame = pgp,
% 	run_s1(interactively(Pred), Config, Config^pgp, !IO)
% 	;
% 	Config^selectedGame = 'pgp+pa',
% 	run_s1(interactively(Pred), Config, Config^'pgp+pa', !IO)
% 	;
% 	Config^selectedGame = ultimatum,
% 	run_s1(interactively(Pred), Config, Config^ultimatum, !IO).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- func getCurrentChoice(config) = maybe(currentChoice(config)).

getCurrentChoice(Config) = Result :-
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




/**
 * The first step of an EBEA run is to initialise the pseudo-random number
 * generator.  The initialisation may fail because of an invalid seed.
 */

:- pred run_s1(ebea.core.runMode(CS, T), config, gameConfig(G, CS, P), io.state, io.state)
	<= (asymmetricGame(G, CS), chromosome(CS, T, P), foldable(CS, A), parseable(CS), printable(CS), printable(T), printable(A)).
:- mode run_s1(in(runMode), in, in, di, uo) is det.

run_s1(RunMode, AllConfig, GameConfig, !IO) :-
	data.prng.init(AllConfig^random, MRandom, !IO),
	(
		MRandom = ok({Supply, Seed}),
		Supply = supply(Random),
		io.format(io.stderr_stream, "RANDOM SEED %d\n", [i(Seed)], !IO),
	  
		run_s2(RunMode, AllConfig, GameConfig, 1, Random, _, !IO)
%		list.foldl2(run_s2(RunMode, AllConfig, GameConfig), 1..AllConfig^numberRuns, Random, _, !IO)
		;
		MRandom = error(Msg),
		io.print(io.stderr_stream, Msg, !IO),
		io.nl(io.stderr_stream, !IO)
	).

/**
 * The second step of an EBEA run is to open the output streams for the
 * given run index.
 */

:- pred run_s2(ebea.core.runMode(CS, T), config, gameConfig(G, CS, P), int, R, R, io.state, io.state)
	<= (
		asymmetricGame(G, CS),
		chromosome(CS, T, P),
		foldable(CS, A),
		parseable(CS),
		printable(CS),
		printable(T),
		printable(A),
		ePRNG(R)).
:- mode run_s2(in(runMode), in, in,  in,  in, out, di, uo) is det.

run_s2(RunMode, AllConfig, ConfigGame, RunIndex, !Random, !IO) :-
	io.format(io.stderr_stream, "Run %d ", [i(RunIndex)], !IO),
	io.flush_output(io.stderr_stream, !IO),
	ebea.streams.openOutputStreams(AllConfig^level, yes(string.format("_R%d", [i(RunIndex)])), IMStreams, !IO),
	(
		IMStreams = ok(Streams),
		run_s3(RunMode, AllConfig, ConfigGame, Streams, !Random, !IO),
		io.print(io.stderr_stream, "finished\n", !IO),
		(if
			RunIndex < AllConfig^numberRuns
		then
			run_s2(RunMode, AllConfig, ConfigGame, RunIndex + 1, !Random, !IO)
		else
			true
		)
		;
		IMStreams = error(Msg),
		io.print(io.stderr_stream, Msg, !IO),
		io.nl(io.stderr_stream, !IO)
	).

/**
 * The third and last step we call predicate {@code ebea.core.run/12}.
 */

:- pred run_s3(ebea.core.runMode(CS, T), config, gameConfig(G, CS, P), ebea.streams.outStreams, R, R, io.state, io.state)
	<= (ePRNG(R), asymmetricGame(G, CS), chromosome(CS, T, P), foldable(CS, A), parseable(CS), printable(CS), printable(T), printable(A)).
:- mode run_s3(in(runMode), in, in, in,  in, out, di, uo) is det.

run_s3(RunMode, AllConfig, GameConfig, Streams, !Random, !IO) :-
	PlayerParameters^mutationProbability = float(AllConfig^mutationProbability),
	PlayerParameters^agePar = AllConfig^ageParameters,
	PlayerParameters^energyPar = AllConfig^energyParameters,
	PlayerParameters^selectionPar = AllConfig^selectionParameters,
	PlayerParameters^gamePar = GameConfig^parameters,

	Parameters^migrationProbability = float(AllConfig^migrationProbability),
	Parameters^dynamic = AllConfig^dynamic,
	Parameters^playerParameters = PlayerParameters,
	
	ebea.population.createInitialPopulation(PlayerParameters, GameConfig^initialPopulation, Population, !Random),
	ebea.core.run(RunMode, GameConfig^game, Parameters, Streams, AllConfig^numberIterations, Population, rng.distribution.init, _, !Random, !IO),
	ebea.streams.closeOutputStreams(Streams, !IO)
	.




% /**
%  * The first step of a background run is to select the game configuration
%  * and to construct an {@code ebea.core.runMode} value.
%  */
% :- pred runBackground_s1(config, int, io.state, io.state).
% :- mode runBackground_s1(in, in, di, uo) is det.

% runBackground_s1(Config, RunIndex, !IO) :-
% 	Config^selectedGame = '2x2',
% 	run_s2(background, Config, RunIndex, Config^cfg_2x2, !IO)
% 	;
% 	Config^selectedGame = battlesexes,
% 	run_s2(background, Config, RunIndex, Config^battlesexes, !IO)
% 	;
% 	Config^selectedGame = centipede,
% 	run_s2(background, Config, RunIndex, Config^centipede, !IO)
% 	;
% 	Config^selectedGame = pgp,
% 	run_s2(background, Config, RunIndex, Config^pgp, !IO)
% 	;
% 	Config^selectedGame = 'pgp+pa',
% 	run_s2(background, Config, RunIndex, Config^'pgp+pa', !IO)
% 	;
% 	Config^selectedGame = ultimatum,
% 	run_s2(background, Config, RunIndex, Config^ultimatum, !IO).

% :- pred runInteractively_s1(selectedGamePred, config, int, io.state, io.state).
% :- mode runInteractively_s1(in(selectedGamePred), in, in, di, uo) is det.

% runInteractively_s1(SelectedGamePred, Config, RunIndex, !IO) :-
% 	SelectedGamePred = '2x2'(Pred),
% 	run_s2(interactively(Pred), Config, RunIndex, Config^cfg_2x2, !IO)
% 	;
% 	SelectedGamePred = battlesexes(Pred),
% 	run_s2(interactively(Pred), Config, RunIndex, Config^battlesexes, !IO)
% 	;
% 	SelectedGamePred = centipede(Pred),
% 	run_s2(interactively(Pred), Config, RunIndex, Config^centipede, !IO)
% 	;
% 	SelectedGamePred = pgp(Pred),
% 	run_s2(interactively(Pred), Config, RunIndex, Config^pgp, !IO)
% 	;
% 	SelectedGamePred = 'pgp+pa'(Pred),
% 	run_s2(interactively(Pred), Config, RunIndex, Config^'pgp+pa', !IO)
% 	;
% 	SelectedGamePred = ultimatum(Pred),
% 	run_s2(interactively(Pred), Config, RunIndex, Config^ultimatum, !IO)
% 	.


% /**
%  * The second step of an EBEA run is to open the output streams and
%  * initialise the pseudo-random number generator.
%  */

% :- pred run_s2(ebea.core.runMode(CS, T), config, int, config(G, CS, P), io.state, io.state)
% 	<= (game(G, CS), chromosome(CS, T, P), foldable(CS, A), printable(CS), printable(T), printable(A)).
% :- mode run_s2(in(runMode), in, in, in, di, uo) is det.

% run_s2(RunMode, AllConfig, RunIndex, ConfigGame, !IO) :-
% 	ebea.streams.openOutputStreams(AllConfig^level, yes(string.format("_R%d", [i(RunIndex)])), IMStreams, !IO),
% 	my.random.init(AllConfig^random, MRandom, !IO),
% 	(
% 		IMStreams = ok(Streams),
% 		(
% 			MRandom = ok({Supply, Seed}),
% 			Supply = supply(Random),
% 			io.format(io.stderr_stream, "RANDOM SEED %d\n", [i(Seed)], !IO),
% 			run_s3(RunMode, AllConfig, Streams, Random, ConfigGame, !IO)
% 			;
% 			MRandom = error(Msg),
% 			io.print(io.stderr_stream, Msg, !IO),
% 			io.nl(io.stderr_stream, !IO)
% 		)
% 		;
% 		IMStreams = error(Msg),
% 		io.print(io.stderr_stream, Msg, !IO),
% 		io.nl(io.stderr_stream, !IO)
% 	).

% % :- pred runBackground_s2(config, ebea.streams.outStreams, R, io.state, io.state)
% % 	<= ePRNG(R).
% % :- mode runBackground_s2(in, in, in, di, uo) is det.

% % runBackground_s2(Config, Streams, Random, !IO) :-
% % 	Config^selectedGame = '2x2',
% % 	run_s3(background, Config, Streams, Random, Config^cfg_2x2, !IO)
% % 	;
% % 	Config^selectedGame = battlesexes,
% % 	run_s3(background, Config, Streams, Random, Config^battlesexes, !IO)
% % 	;
% % 	Config^selectedGame = centipede,
% % 	run_s3(background, Config, Streams, Random, Config^centipede, !IO)
% % 	;
% % 	Config^selectedGame = pgp,
% % 	run_s3(background, Config, Streams, Random, Config^pgp, !IO)
% % 	;
% % 	Config^selectedGame = 'pgp+pa',
% % 	run_s3(background, Config, Streams, Random, Config^'pgp+pa', !IO)
% % 	;
% % 	Config^selectedGame = ultimatum,
% % 	run_s3(background, Config, Streams, Random, Config^ultimatum, !IO)
% % 	.

% :- pred run_s3(ebea.core.runMode(CS, T), config, ebea.streams.outStreams, R, config(G, CS, P), io.state, io.state)
% 	<= (ePRNG(R), game(G, CS), chromosome(CS, T, P), foldable(CS, A), printable(CS), printable(T), printable(A)).
% :- mode run_s3(in(runMode), in, in, in, in, di, uo) is det.

% run_s3(RunMode, AllConfig, Streams, Random, GameConfig, !IO) :-
% 	PlayerParameters^mutationProbability = float(AllConfig^mutationProbability),
% 	PlayerParameters^agePar = AllConfig^ageParameters,
% 	PlayerParameters^energyPar = AllConfig^energyParameters,
% 	PlayerParameters^selectionPar = AllConfig^selectionParameters,
% 	PlayerParameters^gamePar = GameConfig^parameters,

% 	Parameters^migrationProbability = float(AllConfig^migrationProbability),
% 	Parameters^dynamic = AllConfig^dynamic,
% 	Parameters^playerParameters = PlayerParameters,
	
% 	ebea.population.createInitialPopulation(PlayerParameters, GameConfig^initialPopulation, Population, Random, NextRandom),
% 	ebea.core.run(RunMode, GameConfig^game, Parameters, Streams, AllConfig^numberIterations, Population, rng.distribution.init, _, NextRandom, _, !IO),
% 	ebea.streams.closeOutputStreams(Streams, !IO)
% 	.

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


:- func dialog(list(dialogItem(G)), list(dialogItem(P)), list(dialogItem(CS)), CS) = list(dialogItem(gameConfig(G, CS, P))).

dialog(DialogGame, DialogParameters, DialogStrategyChromosome, DefaultStrategyChromosome) =
	[
	di(label("game parameters"),      'new editField'(get_game,       set_game,       DialogGame)),
	di(label("general parameters"),   'new editField'(get_parameters, set_parameters, DialogParameters)),
	di(label("initial population"),
		'new editField'(
			get_initialPopulation,
			set_initialPopulation,
			ebea.population.parameters.dialog(DefaultStrategyChromosome, DialogStrategyChromosome)))
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
	gl.pgp.strategy.default).

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







                                                          % getters and setters for config/3

:- func get_game(gameConfig(G, CS, P)) = G.

get_game(Config) = Config^game.

:- func set_game(gameConfig(G, CS, P), G) = setResult(gameConfig(G, CS, P)).

set_game(Config, Game) = ok('game :='(Config, Game)).

:- func get_parameters(gameConfig(G, CS, P)) = P.

get_parameters(Config) = Config^parameters.

:- func set_parameters(gameConfig(G, CS, P), P) = setResult(gameConfig(G, CS, P)).

set_parameters(Config, Parameters) = ok('parameters :='(Config, Parameters)).

:- func get_initialPopulation(gameConfig(G, CS, P)) = ebea.population.parameters.parameters(CS).

get_initialPopulation(Config) = Config^initialPopulation.

:- func set_initialPopulation(gameConfig(G, CS, P), ebea.population.parameters.parameters(CS)) = setResult(gameConfig(G, CS, P)).

set_initialPopulation(Config, InitialPopulation) = ok('initialPopulation :='(Config, InitialPopulation)).



% :- func getSelectedGameIndex(data.config.config) = maybe(int).

% getSelectedGameIndex(Config) = yes(Index) :-
% 	selectedGameIndex(Config^selectedGame, Index).

% :- pred selectedGameIndex(games, int).
% :- mode selectedGameIndex(in, out) is det.
% :- mode selectedGameIndex(out, in) is semidet.


% selectedGameIndex('2x2', 0).
% selectedGameIndex(battlesexes, 1).
% selectedGameIndex(centipede, 2).
% selectedGameIndex(givetake, 3).
% selectedGameIndex(investment, 4).
% selectedGameIndex(pgp, 5).
% selectedGameIndex('pgp+pa', 6).
% selectedGameIndex(ultimatum, 7).

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
