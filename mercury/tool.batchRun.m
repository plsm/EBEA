/**
 * 

 * History

  * This solution does not work because higher order does not support
  * polimorphism.  Prototype of predicate file.batch.run/2:
  
  * <pre>
  * :- pred run(batch, run).
  * :- mode run(in, out) is nondet.
  * </pre>

  * Types {@code batch} and {@code run} are existentially constrained.

  
  * promise_equivalent_solutions [Supply, !:IO]
  * solutions.unsorted_aggregate2(
  *   file.batch.run(BT),
  *   runSim(Streams),
  *   Supply, _,
  *   !IO),


  
 * @author Pedro Mariano
 * @version 1.0 2013/05/28
 */
:- module tool.batchRun.

:- interface.

:- import_module io, maybe.

:- pred go(maybe(string), io.state, io.state).
:- mode go(in, di, uo) is det.

:- implementation.

:- import_module ebea, ebea.core, ebea.energy, ebea.player, ebea.population, ebea.selection, ebea.streams.
:- import_module gl, gl.'2x2'.
:- import_module file, file.batch.
:- import_module gfactory, rng, rng.distribution, probability.
:- import_module my, my.random.
:- import_module list, int, float, random, solutions, string.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

% :- type commonParameterValues --->
% 	cpv(
% 		vMutationProbability :: list(float),
% 		vEnergyReproduce     :: list(float),
% 		vEnergyGainProcess   :: list(ebea.energy.energyGainProcess),
% 		vEnergyBirth         :: list(float),
% 		vCarryingCapacity    :: list(float),
% 		vPopulationDynamic   :: list(ebea.population.dynamic),
% 		vRuns                :: list(int),
% 		rounds               :: int,
% 		random               :: my.random.supply
% 	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

go(MFilename, !IO) :-
	(
		MFilename = yes(Filename)
		;
		MFilename = no,
		Filename = "batch.txt"
	),
	file.batch.scan(Filename, MBatch, !IO),
	(
		MBatch = ok(Batch),
		my.random.init(Batch^random, MRandom, !IO),
		(
			MRandom = ok(Supply),
			Supply = supply(Random),
			ebea.streams.openOutputStreams(summary, IMStreams, !IO),
			(if
				IMStreams = ok(ok(Streams)),
				Streams = summary(_)  % needed because of the mode
			then
				promise_equivalent_solutions [!:IO]
				solutions.unsorted_aggregate2(
					file.batch.run(Batch),
					runSim(Streams, Batch^commonParameterValues^rounds),
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


:- pred runSim(
	ebea.streams.outStreams,
	int,
	file.batch.run,
	R, R,
	io.state, io.state
	) <= ePRNG(R).
:- mode runSim(in(summary), in, in, in, out, di, uo) is det.

runSim(Streams, NumberRuns, r(Run), !Random, !IO) :-
	io.print(Streams^sosummary, Run^keyCommon, !IO),
	io.print(Streams^sosummary, "\t", !IO),
	io.print(Streams^sosummary, Run^keyGame, !IO),
	io.print(Streams^sosummary, "\t", !IO),
	ebea.population.init(Run^parameters, Run^initialPopulation, Population, !Random),
	ebea.core.initData(Run^game, Run^parameters, Streams) = Data,
	ebea.core.run(Data, NumberRuns, Population, rng.distribution.init, _, !Random, !IO).

:- end_module tool.batchRun.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
