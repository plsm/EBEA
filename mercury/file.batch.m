/**
 * 

 * @author Pedro Mariano
 * @version 1.0 2013/06/ 1
 */
:- module file.batch.

:- interface.

:- include_module view.
:- import_module ebea, ebea.energy, ebea.player, ebea.population, ebea.streams.
:- import_module game, chromosome, gfactory.
:- import_module foldable, printable, scanable.
:- import_module tool.
:- import_module gl, gl.'2x2', gl.'2x2'.factory, gl.centipede, gl.investment, gl.pgp, gl.'pgp+pa', gl.ultimatum.
:- import_module rng.
:- import_module my, my.random.
:- import_module io, list, maybe, pair.

/**
 * Represents the data necessary for a batch run that is stored in a file.
 */
:- type batch --->
	'batch.0.1'(
		my.random.supplyParameter,
		commonParameterValues,
		maybe(tool.games),
		gl.centipede.factory
	) ;
	'batch.0.2'(
		my.random.supplyParameter,
		commonParameterValues,
		maybe(tool.games),
		gl.centipede.factory,
		gl.'2x2'.factory.factory
	) ;
	'batch.0.3'(
		my.random.supplyParameter,
		commonParameterValues,
		maybe(tool.games),
		gl.centipede.factory,
		gl.'2x2'.factory.factory,
		gl.'pgp+pa'.factory
	) ;
	batch_1(
		random                 :: my.random.supplyParameter,
		commonParameterValues  :: commonParameterValues,
		selectedGame           :: maybe(tool.games),
		gameFactory_2x2        :: gl.'2x2'.factory.factory,
		gameFactory_centipede  :: gl.centipede.factory,
		gameFactory_investment :: gl.investment.factory,
		gameFactory_pgp        :: gl.pgp.factory,
		'gameFactory_pgp+pa'   :: gl.'pgp+pa'.factory,
		gameFactory_ultimatum  :: gl.ultimatum.factory
	).

/**
 * The predicates in this module only take the latest version.  There is a
 * function that can be used to upgrade values read from a stream to the
 * latest version.
 */

:- inst latest == bound(batch_1(ground, ground,  ground,  ground, ground, ground, ground, ground, ground)).

/**
 * Contains the values of parameters that are independent of the game used.
  
 */
:- type commonParameterValues --->
	% cpv(
	% 	list(float),
	% 	list(float),
	% 	list(ebea.energy.energyGainProcess),
	% 	list(float),
	% 	list(int),
	% 	list(float),
	% 	list(ebea.population.dynamic),
	% 	list(int),
	% 	int
	% ) ;
	cpv1(
		lMutationProbability :: list(float),
		lEnergyReproduce     :: list(float),
		lEnergyGainProcess   :: list(ebea.energy.energyGainProcess),
		lEnergyBirth         :: list(float),
		lOldAge              :: list(int),
		lDeathSuaveness      :: list(float),
		lCarryingCapacity    :: list(float),
		lPopulationDynamic   :: list(ebea.population.dynamic),
		lRuns                :: list(int),
		rounds               :: int
	).

%:- inst latestCPV == bound(cpv1(ground, ground, ground, ground, ground, ground, ground, ground, ground, ground)).

/**
 * Represents the data in a configuration file needed to perform a run of
  * the Energy Based Evolutionary Algorithm.
  
 */
:- type run(G, CS, P) --->
	run(
		keyCommon         :: string,
		keyGame           :: string,
		game              :: G,
		parameters        :: ebea.population.parameters(P),
		initialPopulation :: list({int, ebea.player.chromosome(CS)})
	).

:- type run --->
	some [G, CS, P, T, A]
	(r(run(G, CS, P))
	=> (
		game(G, CS),
		chromosome(CS, T, P),
		foldable(CS, A),
		printable(CS),
		printable(T),
		printable(A))
	).

:- func defaultBatch = batch.
:- mode defaultBatch = out(latest) is det.

/**
 * Convert a batch (possibly read from a stream) to the latest batch version.
 */
:- func convert(batch) = batch.
:- mode convert(in) =  out(latest) is det.

/**
 * Return a default value of type {@code common Parameter Values} which
 * represents general parameters of EBEA, those that are independent of the
 * game used in EBEA.
  
 */
:- func defaultCommonParameterValues = commonParameterValues.

:- pred scan(string, maybe_error(batch), io.state, io.state).
:- mode scan(in, out(maybe_error(latest)), di, uo) is det.

:- pred run(batch, run).
:- mode run(in(latest), out) is nondet.

% getters and setters for batch

:- func random(batch) = my.random.supplyParameter.
:- mode random(in(latest)) = out is det.

:- func commonParameterValues(batch) = commonParameterValues.
:- mode commonParameterValues(in(latest)) = out is det.

:- func 'random :='(batch, my.random.supplyParameter) = batch.
:- mode 'random :='(in(latest), in) = out(latest) is det.

:- func 'commonParameterValues :='(batch, commonParameterValues) = batch.
:- mode 'commonParameterValues :='(in(latest), in) = out(latest) is det.

:- func gameFactory_2x2(batch) = gl.'2x2'.factory.factory.
:- mode gameFactory_2x2(in(latest)) = out is det.

:- func set_gameFactory_2x2(batch, gl.'2x2'.factory.factory) = batch.
:- mode set_gameFactory_2x2(in(latest), in) = out(latest) is det.

:- func gameFactory_centipede(batch) = gl.centipede.factory.
:- mode gameFactory_centipede(in(latest)) = out is det.

:- func set_gameFactory_centipede(batch, gl.centipede.factory) = batch.
:- mode set_gameFactory_centipede(in(latest), in) = out(latest) is det.

:- func gameFactory_investment(batch) = gl.investment.factory.
:- mode gameFactory_investment(in(latest)) = out is det.

:- func set_gameFactory_investment(batch, gl.investment.factory) = batch.
:- mode set_gameFactory_investment(in(latest), in) = out(latest) is det.

:- func gameFactory_pgp(batch) = gl.pgp.factory.
:- mode gameFactory_pgp(in(latest)) = out is det.

:- func set_gameFactory_pgp(batch, gl.pgp.factory) = batch.
:- mode set_gameFactory_pgp(in(latest), in) = out(latest) is det.

:- func 'gameFactory_pgp+pa'(batch) = gl.'pgp+pa'.factory.
:- mode 'gameFactory_pgp+pa'(in(latest)) = out is det.

:- func 'set_gameFactory_pgp+pa'(batch, gl.'pgp+pa'.factory) = batch.
:- mode 'set_gameFactory_pgp+pa'(in(latest), in) = out(latest) is det.

:- func gameFactory_ultimatum(batch) = gl.ultimatum.factory.
:- mode gameFactory_ultimatum(in(latest)) = out is det.

:- func set_gameFactory_ultimatum(batch, gl.ultimatum.factory) = batch.
:- mode set_gameFactory_ultimatum(in(latest), in) = out(latest) is det.



% getters and setters for commonParameterValues

:- func lMutationProbability(commonParameterValues) = list(float).
%:- mode lMutationProbability(in(latestCPV)) = out is det.

:- func 'lMutationProbability :='(commonParameterValues, list(float)) = commonParameterValues.
%:- mode'lMutationProbability :='(in(latestCPV), in) = out(latestCPV) is det.
	
:- func lEnergyReproduce(commonParameterValues) = list(float).
%:- mode lEnergyReproduce(in(latestCPV)) = out is det.

:- func 'lEnergyReproduce :='(commonParameterValues, list(float)) = commonParameterValues.
%:- mode 'lEnergyReproduce :='(in(latestCPV), in) = out(latestCPV) is det.

:- func lEnergyGainProcess(commonParameterValues) = list(ebea.energy.energyGainProcess).
%:- mode lEnergyGainProcess(in(latestCPV)) = out is det.

:- func 'lEnergyGainProcess :='(commonParameterValues, list(ebea.energy.energyGainProcess)) = commonParameterValues.
%:- mode 'lEnergyGainProcess :='(in(latestCPV), in) = out(latestCPV) is det.

:- func lEnergyBirth(commonParameterValues) = list(float).
%:- mode lEnergyBirth(in(latestCPV)) = out is det.

:- func 'lEnergyBirth :='(commonParameterValues, list(float)) = commonParameterValues.
%:- mode 'lEnergyBirth :='(in(latestCPV), in) = out(latestCPV) is det.

:- func lOldAge(commonParameterValues) = list(int).
%:- mode lOldAge(in(latestCPV)) = out is det.

:- func 'lOldAge :='(commonParameterValues, list(int)) = commonParameterValues.
%:- mode 'lOldAge :='(in(latestCPV), in) = out(latestCPV) is det.

:- func lCarryingCapacity(commonParameterValues) = list(float).
%:- mode lCarryingCapacity(in(latestCPV)) = out is det.

:- func 'lCarryingCapacity :='(commonParameterValues, list(float)) = commonParameterValues.
%:- mode 'lCarryingCapacity :='(in(latestCPV), in) = out(latestCPV) is det.

:- func lDeathSuaveness(commonParameterValues) = list(float).

:- func lRuns(commonParameterValues) = list(int).
%:- mode lRuns(in(latestCPV)) = out is det.

:- func 'lRuns :='(commonParameterValues, list(int)) = commonParameterValues.
%:- mode 'lRuns :='(in(latestCPV), in) = out(latestCPV) is det.

:- func rounds(commonParameterValues) = int.
%:- mode rounds(in(latestCPV)) = out is det.

:- func 'rounds :='(commonParameterValues, int) = commonParameterValues.
%:- mode 'rounds :='(in(latestCPV), in) = out(latestCPV) is det.

:- func lPopulationDynamic(commonParameterValues) = list(ebea.population.dynamic).
%:- mode lPopulationDynamic(in(latestCPV)) = out is det.

:- func 'lPopulationDynamic :='(commonParameterValues, list(ebea.population.dynamic)) = commonParameterValues.
%:- mode 'lPopulationDynamic :='(in(latestCPV), in) = out(latestCPV) is det.

:- implementation.

:- import_module ebea, ebea.selection.
:- import_module gl, gl.'2x2', gl.centipede.
:- import_module tool.
:- import_module rng.
:- import_module my, my.random.
:- import_module scanable.
:- import_module maybe, string.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

defaultBatch = batch_1(
	mt(clock),
	file.batch.defaultCommonParameterValues,
	no,
	gl.'2x2'.defaultFactory,
	gl.centipede.defaultFactory,
	gl.investment.defaultFactory,
	gl.pgp.defaultFactory,
	gl.'pgp+pa'.defaultFactory,
	gl.ultimatum.defaultFactory
	).

convert(Version) = Result :-
	Version = 'batch.0.1'(R, P, SG, C),
	Result = batch_1(R, P, SG, gl.'2x2'.defaultFactory, C, gl.investment.defaultFactory, gl.pgp.defaultFactory, gl.'pgp+pa'.defaultFactory, gl.ultimatum.defaultFactory)
	;
	Version = 'batch.0.2'(R, P, SG, C, Factory_2x2),
	Result = batch_1(R, P, SG, Factory_2x2, C, gl.investment.defaultFactory, gl.pgp.defaultFactory, gl.'pgp+pa'.defaultFactory, gl.ultimatum.defaultFactory)
	;
	Version = 'batch.0.3'(R, P, SG, FactoryCentipede, Factory_2x2, Factory_PGP_PA),
	Result = batch_1(R, P, SG, Factory_2x2, FactoryCentipede, gl.investment.defaultFactory, gl.pgp.defaultFactory, Factory_PGP_PA, gl.ultimatum.defaultFactory)
	;
	Version = batch_1(_, _,   _,   _, _, _, _, _, _),
	Result = Version.

defaultCommonParameterValues = cpv1(
	[0.10],           % lMutationProbability
	[50.0],           % lEnergyReproduce
	[scaled],         % lEnergyGainProcess
	[10.0],           % lEnergyBirth
	[50],             % lOldAge
	[1.0],            % lDeathSuaveness
	[100.0],          % lCarryingCapacity
	[deathThenBirth], % lPopulationDynamic
	1..30,            % lRuns
	10000             % rounds
	).
% defaultCommonParameterValues =  cpv(
% 	[0.10],           % lMutationProbability
% 	[50.0],           % lEnergyReproduce
% 	[scaled],         % lEnergyGainProcess
% 	[10.0],           % lEnergyBirth
% 	[50],             % lOldAge
% 	[100.0],          % lCarryingCapacity
% 	[deathThenBirth], % lPopulationDynamic
% 	1..30,            % lRuns
% 	10000             % rounds
% 	).

scan(Filename, MBatch, !IO) :-
	io.open_input(Filename, IStream, !IO),
	(
		IStream = ok(Stream),
		io.read(Stream, IBatch, !IO),
		(
			IBatch = ok(Batch),
			MBatch = ok(convert(Batch))
			;
			IBatch = eof,
			MBatch = error("end-of-file reached while reading batch file")
			;
			IBatch = error(Message, Line),
			MBatch = error(string.format("%d: %s", [i(Line), s(Message)]))
		),
		io.close_input(Stream, !IO)
		;
		IStream = error(Error),
		MBatch = error(string.format("IO error opening `birth.csv` file: %s", [s(io.error_message(Error))]))
	).

run(Batch, Result) :-
	Batch^selectedGame = yes('2x2'),
	gfactory.value( Batch^gameFactory_2x2,       KeyGame, Game, ListQuantityStrategy, Parameters),
	run(Batch, KeyGame, Game, ListQuantityStrategy, Parameters, Result)
	;
	Batch^selectedGame = yes(centipede),
	gfactory.value( Batch^gameFactory_centipede, KeyGame, Game, ListQuantityStrategy, Parameters),
	run(Batch, KeyGame, Game, ListQuantityStrategy, Parameters, Result)
	;
	Batch^selectedGame = yes(pgp),
	gfactory.value( Batch^gameFactory_pgp,  KeyGame, Game, ListQuantityStrategy, Parameters),
	run(Batch, KeyGame, Game, ListQuantityStrategy, Parameters, Result)
	;
	Batch^selectedGame = yes(investment),
	gfactory.value( Batch^gameFactory_investment,  KeyGame, Game, ListQuantityStrategy, Parameters),
	run(Batch, KeyGame, Game, ListQuantityStrategy, Parameters, Result)
	;
	Batch^selectedGame = yes('pgp+pa'),
	gfactory.value( Batch^'gameFactory_pgp+pa',  KeyGame, Game, ListQuantityStrategy, Parameters),
	run(Batch, KeyGame, Game, ListQuantityStrategy, Parameters, Result)
	;
	Batch^selectedGame = yes(ultimatum),
	gfactory.value( Batch^gameFactory_ultimatum,  KeyGame, Game, ListQuantityStrategy, Parameters),
	run(Batch, KeyGame, Game, ListQuantityStrategy, Parameters, Result)
	.

set_gameFactory_centipede(BP, Factory) = Result :-
	Tmp = 'gameFactory_centipede :='(BP, Factory),
	Result = 'selectedGame :='(Tmp, yes(centipede)).

set_gameFactory_2x2(BP, Factory) = Result :-
	Tmp = 'gameFactory_2x2 :='(BP, Factory),
	Result = 'selectedGame :='(Tmp, yes('2x2')).

set_gameFactory_investment(BP, Factory) = Result :-
	Tmp = 'gameFactory_investment :='(BP, Factory),
	Result = 'selectedGame :='(Tmp, yes(investment)).

set_gameFactory_pgp(BP, Factory) = Result :-
	Tmp = 'gameFactory_pgp :='(BP, Factory),
	Result = 'selectedGame :='(Tmp, yes(pgp)).

'set_gameFactory_pgp+pa'(BP, Factory) = Result :-
	Tmp = 'gameFactory_pgp+pa :='(BP, Factory),
	Result = 'selectedGame :='(Tmp, yes('pgp+pa')).

set_gameFactory_ultimatum(BP, Factory) = Result :-
	Tmp = 'gameFactory_ultimatum :='(BP, Factory),
	Result = 'selectedGame :='(Tmp, yes(ultimatum)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- pred run(batch, string, G, list({int, CS}), P, run)
	<= (
		game(G, CS),
		chromosome(CS, T, P),
		foldable(CS, A),
		printable(CS),
		printable(T),
		printable(A)).
:- mode run(in(latest), in, in, in, in, out) is nondet.

run(Batch, KeyGame, Game, ListQuantityStrategy, Parameters, 'new r'(Run)) :-
	list.member(MutationProbability,  Batch^commonParameterValues^lMutationProbability),
	list.member(EnergyReproduce,      Batch^commonParameterValues^lEnergyReproduce),
	list.member(EnergyGainProcess,    Batch^commonParameterValues^lEnergyGainProcess),
	list.member(EnergyBirth,          Batch^commonParameterValues^lEnergyBirth),
	list.member(CarryingCapacity,     Batch^commonParameterValues^lCarryingCapacity),
	list.member(Dynamic,              Batch^commonParameterValues^lPopulationDynamic),
	list.member(RunNumber,            Batch^commonParameterValues^lRuns),
	%
	GlobalParameters^carryingCapacity = CarryingCapacity,
	GlobalParameters^dynamic          = Dynamic,
	GlobalParameters^pla              = PlayerParameters,
	%
	PlayerParameters^mutationProbability = MutationProbability,
	PlayerParameters^energyPar           = ebea.energy.initParameters(EnergyReproduce, EnergyGainProcess, EnergyBirth),
	PlayerParameters^selectionPar        = ebea.selection.initParameters,
	PlayerParameters^gamePar             = Parameters,
	%
	Func =
	(func({Q, S}) = {Q, C} :-
		C^energyGenes    = plain,
		C^selectionGenes = ebea.selection.initChromosome,
		C^strategyGenes  = S
	),
	list.map(Func, ListQuantityStrategy) = ListQuantityChromosome,
	%
	KeyCommon = string.format("%f %f %s %f %f %s %d",
		[f(MutationProbability), f(EnergyReproduce), s(string(EnergyGainProcess)),
		 f(EnergyBirth), f(CarryingCapacity), s(string(Dynamic)), i(RunNumber)]),
	%
	Run^keyCommon         = KeyCommon,
	Run^keyGame           = KeyGame,
	Run^game              = Game,
	Run^parameters        = GlobalParameters,
	Run^initialPopulation = ListQuantityChromosome.

/*
:- func initBatch(R, commonParameterValues, scanable.result(F)) = scanable.result(batch)
<= (
		ePRNG(R),
		factory(F, G, CS, P),
		game(G, CS),
		chromosome(CS, T, P),
		foldable(CS, A),
		printable(CS),
		printable(T),
		printable(A)
	).

initBatch(Random, CPV, IMFactory) = Result :-
	(if
		IMFactory = ok(ok(Factory))
	then
		Batch = batch(Random, CPV, Factory),
		% Batch^random = Random,
		% Batch^commonParameterValues = CPV,
		% Batch^gameFactory = Factory,
		Result = ok(ok('new bt'(Batch)))
	else
		Result = scanable.noOkToIOresultMaybeError(IMFactory)
	).

:- func initBatch(R, commonParameterValues, io.read_result(F), pred(F)) = scanable.result(batch)
<= (
		ePRNG(R),
		factory(F, G, CS, P),
		game(G, CS),
		chromosome(CS, T, P),
		foldable(CS, A),
		printable(CS),
		printable(T),
		printable(A)
	).
:- mode initBatch(in, in, in, in(pred(in) is semidet)) = out is det.

initBatch(Random, CPV, IFactory, PredCheck) = Result :-
	IFactory = ok(Factory),
	(if
		PredCheck(Factory)
	then
		Batch = batch(Random, CPV, Factory),
		% Batch^random = Random,
		% Batch^commonParameterValues = CPV,
		% Batch^gameFactory = Factory,
		Result = ok(ok('new bt'(Batch)))
	else
		Result = ok(error("factory has invalid values"))
	)
	;
	IFactory = eof,
	Result = ok(error("EOF reached while reading factory"))
	;
	IFactory = error(Msg, Line),
	Result = ok(error(string(Line) ++ ": " ++ Msg)).
*/
:- end_module file.batch.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
