/**
 * 

 * @author Pedro Mariano
 * @version 1.0 2013/09/13
 */
:- module submission_AAMAS2014.

:- interface.

:- import_module io.

:- pred main(io.state, io.state).
:- mode main(di, uo) is det.

:- implementation.

:- import_module int, list, maybe, solutions, string.

:- import_module rng, rng.distribution.
:- import_module my, my.random.

:- import_module tool.
:- import_module chromosome, game, foldable, gfactory, printable, scanable.
:- import_module gl, gl.pgp, gl.centipede, gl.'pgp+pa', gl.'2x2'.
:- import_module ebea, ebea.core, ebea.population, ebea.player, ebea.energy, ebea.selection, ebea.streams.
:- import_module file, file.batch.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

main(!IO) :-
	io.command_line_arguments(Args, !IO),
	(if
		Args = [F]
	then
		F = Filename
	else
		Filename = "batch.txt"
	),
	file.batch.scan(Filename, MBatch, !IO),
	(
		MBatch = ok(Batch),
		my.random.init(Batch^random, MRandom, !IO),
		(
			MRandom = ok(Supply),
			Supply = supply(Random),
			io.open_output("debug.txt", IStreamDebug, !IO),
			ebea.streams.openOutputStreams(summary, IMStreams, !IO),
%			ebea.streams.openOutputStreams(detailed, IMStreams, !IO),
			(if
				IStreamDebug = ok(StreamDebug),
				IMStreams = ok(ok(Streams))
%				Streams = detailed(_, _, _)  % needed because of the mode
%				Streams = summary(_)  % needed because of the mode
%				Batch = 'batch.0.3'(_, cpv1(_, _, _,  _, _, _,   _, _, _, _), _, _, _, _)
			then
  /*
				promise_equivalent_solutions [Count1, !:IO]
				solutions.unsorted_aggregate2(
					submission_AAMAS2014.run(Batch),
					countRuns(StreamDebug),
					0, Count1,
					!IO),
				io.format(StreamDebug, "There are %d runs, computed using unsorted_aggregate/4\n", [i(Count1)], !IO),
				
				solutions.aggregate2(
					submission_AAMAS2014.run(Batch),
					countRuns(StreamDebug),
					0, Count2,
					!IO),
				io.format(StreamDebug, "There are %d runs, computed using aggregate/4\n", [i(Count2)], !IO),
				io.close_output(StreamDebug, !IO)
			*/
				
				promise_equivalent_solutions [!:IO]
				solutions.unsorted_aggregate2(
					submission_AAMAS2014.run(Batch),
					submission_AAMAS2014.runSimMisc(Streams, Batch^commonParameterValues^rounds),
					Random, _,
					!IO),
				ebea.streams.closeOutputStreams(Streams, !IO)

			else
				scanable.printNoOkResult("output streams", IMStreams, !IO)
			)
			;
			MRandom = error(Message),
			io.format("Batch file had an error in the specification of the pseudo-random number generator to be used:\n%s\n", [s(Message)], !IO)
		)
		;
		MBatch = error(Message),
		io.format("An error occurred while reading the batch file:\n%s\n", [s(Message)], !IO)
	).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- pred countRuns(run, int, int).
:- mode countRuns(in, in, out) is det.

countRuns(_, C, C + 1).

:- pred countRuns(io.output_stream, run, int, int, io.state, io.state).
:- mode countRuns(in, in, in, out, di, uo) is det.

countRuns(Stream, Run, C, C + 1, !IO) :-
	io.print(Stream, Run, !IO),
	io.print(Stream, ".\n", !IO).

:- pred run(batch, run).
:- mode run(in(latest), out) is nondet.

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
%	Batch = 'batch.0.3'(_, cpv1(_, _, _,  _, _, _,   _, _, _, _), _, _, _, _),
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
	(
		Batch^commonParameterValues^lOldAge = [],
		PlayerParameters^energyPar           = ebea.energy.initParameters(EnergyReproduce, EnergyGainProcess, EnergyBirth),
		KeyCommon = string.format("%f\t%f\t%s\t%f\t%f\t%s\t%d",
			[f(MutationProbability), f(EnergyReproduce), s(string(EnergyGainProcess)),
			 f(EnergyBirth), f(CarryingCapacity), s(string(Dynamic)), i(RunNumber)])
		;
		Batch^commonParameterValues^lOldAge = [_|_],
		list.member(OldAge,          Batch^commonParameterValues^lOldAge),
		list.member(DeathSuaveness,  Batch^commonParameterValues^lDeathSuaveness),
		PlayerParameters^energyPar           = ebea.energy.initParameters(EnergyReproduce, EnergyGainProcess, EnergyBirth, OldAge, DeathSuaveness),
		KeyCommon = string.format("%f\t%f\t%s\t%f\t%f\t%s\t%d\t%d\t%f",
			[f(MutationProbability), f(EnergyReproduce), s(string(EnergyGainProcess)),
			 f(EnergyBirth), f(CarryingCapacity), s(string(Dynamic)), i(RunNumber),
			 i(OldAge), f(DeathSuaveness)])
	),
	PlayerParameters^selectionPar        = ebea.selection.initParameters(1.0, 1.0, 0.1, 0.1),
	PlayerParameters^gamePar             = Parameters,
	%
	Func =
	(func({Q, S}) = {Q, C} :-
		C^energyGenes    = plain,
%		C^selectionGenes = ebea.selection.partnerSelection(0, 8, 0.5, 0.5),
		C^selectionGenes = ebea.selection.random,
		C^strategyGenes  = S
	),
	list.map(Func, ListQuantityStrategy) = ListQuantityChromosome,
	%
	%
	Run^keyCommon         = KeyCommon,
	Run^keyGame           = KeyGame,
	Run^game              = Game,
	Run^parameters        = GlobalParameters,
	Run^initialPopulation = ListQuantityChromosome.


:- pred runSim(
	ebea.streams.outStreams,
	int,
	file.batch.run,
	R, R,
	io.state, io.state
	) <= ePRNG(R).
:- mode runSim(in(detailed), in, in, in, out, di, uo) is det.

runSim(Streams, NumberRuns, r(Run), !Random, !IO) :-
/*	io.print(Streams^sosummary, Run^keyCommon, !IO),
	io.print(Streams^sosummary, "\t", !IO),
	io.print(Streams^sosummary, Run^keyGame, !IO),
	io.print(Streams^sosummary, "\t", !IO),
*/
	io.print(Streams^sobirth,     "###\n", !IO),
	io.print(Streams^sophenotype, "###\n", !IO),
	io.print(Streams^sodeath,     "###\n", !IO),

	ebea.population.init(Run^parameters, Run^initialPopulation, Population, !Random),
	ebea.core.initData(Run^game, Run^parameters, Streams) = Data,

%	ebea.population.debug(Population, !IO),
	
	ebea.core.run(Data, NumberRuns, Population, rng.distribution.init, _, !Random, !IO),
	
	gl.centipede.statusResetMemoTables(!IO).



:- pred runSimMisc(
	ebea.streams.outStreams,
	int,
	file.batch.run,
	R, R,
	io.state, io.state
	) <= ePRNG(R).
:- mode runSimMisc(in, in, in, in, out, di, uo) is det.

runSimMisc(Streams, NumberRuns, r(Run), !Random, !IO) :-
	(
		Streams = summary(_),
		io.print(Streams^sosummary, Run^keyCommon, !IO),
		io.print(Streams^sosummary, "\t", !IO),
		io.print(Streams^sosummary, Run^keyGame, !IO),
		io.print(Streams^sosummary, "\t", !IO)
		;
		Streams = detailed(_, _, _),
		io.print(Streams^sobirth,     "###\n", !IO),
		io.print(Streams^sophenotype, "###\n", !IO),
		io.print(Streams^sodeath,     "###\n", !IO)
		;
		Streams = dynamics(_),
		io.print(Streams^sopopulation, "###\n", !IO)
	),

	ebea.population.init(Run^parameters, Run^initialPopulation, Population, !Random),
	ebea.core.initData(Run^game, Run^parameters, Streams) = Data,
	ebea.core.run(Data, NumberRuns, Population, rng.distribution.init, _, !Random, !IO),
	
%	gl.centipede.statusResetMemoTables(!IO),
	io.print("-", !IO),
	io.flush_output(!IO),
	true.





:- end_module submission_AAMAS2014.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
