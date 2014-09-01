/**
 * Process the players' phenotype.  Currently we only print the opinion and
 * uncertainty of applicable players.

 * @author Pedro Mariano
 * @version 1.0 2014/04/08
 */
:- module tools.processPhenotype.

:- interface.

:- import_module data, data.config.
:- import_module userInterface.
:- import_module io, list.

/**
 * The parameters that control the behaviour of this tool.
 */
:- type parameters.

:- pred run(data.config.config, tools.processPhenotype.parameters, string, string, io.state, io.state).
:- mode run(in, in, in, out, di, uo) is det.

/**
 * Return a default value of {@code parameters}.
 */
:- func default_parameters = tools.processPhenotype.parameters.

:- func dialog_parameters = list(dialogItem(tools.processPhenotype.parameters)).

:- implementation.

:- import_module ebea, ebea.player, ebea.player.chromosome, ebea.player.selection, ebea.player.selection.chromosome,
ebea.population, ebea.population.configuration, ebea.population.site, ebea.population.site.parameters,
ebea.streams, ebea.streams.birth, ebea.streams.death, ebea.streams.phenotype.
:- import_module data.util.
:- import_module tools.utils.
:- import_module game.
:- import_module parseable, parseable.iou, printable.
:- import_module array, bool, exception, float, int, map, math, maybe, string.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

:- type parameters --->
	pr(
		printOpinionDynamics :: bool ,
		fileNamePrefix              :: string
	)
	.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

run(Config, Parameters, Directory, Feedback, !IO) :-
	Level = Config^level,
	(	%
		Level = detailedBin,
		data.util.gameConfig(Config) = gcex(_, _, InitialPopulation),
		int.fold_up2(
			processRun_s1(
				Config,
				'printOpinionDynamics :='(Parameters, bool.and(Parameters^printOpinionDynamics, bool.pred_to_bool(opinionChromosomes(InitialPopulation)))),
				Directory),
			1, Config^numberRuns,
			[], FeedbackAsList,
			!IO),
		(	%
			FeedbackAsList = [],
			Feedback = "ok"
			;
			FeedbackAsList = [_|_],
			Feedback = string(FeedbackAsList)
		)
	;
		Level = detailedTxt,
		Feedback = "Not handled"
	;	
		Level = dynamics,
		Feedback = "The simulation runs did not produce phenotype data"
	;	
		Level = summary,
		Feedback = "The simulation runs did not produce phenotype data"
	).

default_parameters = pr(
	yes,
	"phenotype"
).

dialog_parameters =
	[].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- pred processRun_s1(
	data.config.config                :: in,
	tools.processPhenotype.parameters :: in,
	string                            :: in,
	int                               :: in,
	list(string) :: in, list(string) :: out,
	io.state     :: di, io.state     :: uo
) is det.

processRun_s1(Config, Parameters, Directory, RunIndex, !FeedbackAsList, !IO) :-
	ebea.streams.openInputStreams(Directory, detailedBin, yes(string.format("_R%d", [i(RunIndex)])), IMStreams, !IO),
	(if
		IMStreams = ok(Streams),
		Streams = detailedBin(_, _, _, _, _)
	then
		io.format("Run %d\n", [i(RunIndex)], !IO),
		ebea.streams.birth.read(Streams, IRAllBirths, !IO),
		ebea.streams.death.read(Streams, IRAllDeaths, !IO),
		ebea.streams.phenotype.read(Streams, IRAllPhenotypes, !IO),
		data.util.gameConfig(Config) = gcex(Game, _, _),
		processRun_s2(
			Config,
			Game,
			Parameters,
			Directory,
			RunIndex,
			IRAllBirths,
			IRAllDeaths,
			IRAllPhenotypes,
			!FeedbackAsList,
			!IO)
	else
		IMStreams = ok(_)
		;
		IMStreams = error(ErrorMsg),
		list.cons(ErrorMsg, !FeedbackAsList)
	).

:- pred processRun_s2(
	data.config.config                :: in,
	G                                 :: in,
	tools.processPhenotype.parameters :: in,
	string                            :: in,
	int                               :: in,
	ioResult(list(iterationBirthRecords(C)))   :: in,
	ioResult(list(iterationDeathRecords))      :: in,
	ioResult(list(iterationPhenotypicRecords)) :: in,
	list(string) :: in, list(string) :: out,
	io.state     :: di, io.state     :: uo
) is det
<= (
	asymmetricGame(G, C),
	parseable(C),
	printable(C)
).

processRun_s2(
	_Config,
	_Game,
	Parameters,
	Directory,
	RunIndex,
	IRAllBirths,
	IRAllDeaths,
	IRAllPhenotypes,
	!FeedbackAsList,
	!IO)
:-
	(if
		IRAllBirths = ok(AllBirths),
		IRAllDeaths = ok(AllDeaths),
		IRAllPhenotypes = ok(AllPhenotypes)
	then
		tools.utils.openMaybeStream(
			Parameters^printOpinionDynamics,
			Directory,
			Parameters^fileNamePrefix,
			"opinion-dynamics",
			RunIndex,
			MStreamOpinion,
			!FeedbackAsList,
			!IO
		),
		ArrayPhenotypes = array.from_list(AllPhenotypes),
		format("Number of phenotypes to process: %d\n", [i(list.length(AllPhenotypes))], !IO),
		int.fold_up2(
			processRunIteration(
				Parameters,
				MStreamOpinion,
				Directory,
				RunIndex,
				AllBirths,
				AllDeaths,
				ArrayPhenotypes
			),
			0, array.size(ArrayPhenotypes) - 1,
			!FeedbackAsList,
			!IO
		),
		tools.utils.closeMaybeStream(MStreamOpinion, !IO)
	else
		list.cons("error reading streams birth, death or phenotype", !FeedbackAsList)
	).

:- pred processRunIteration(
	tools.processPhenotype.parameters :: in,
	maybe(io.output_stream)           :: in,
	string                            :: in,
	int                               :: in,
	list(iterationBirthRecords(C))    :: in,
	list(iterationDeathRecords)       :: in,
	array(iterationPhenotypicRecords) :: in,
	int                               :: in,
	list(string) :: in, list(string) :: out,
	io.state     :: di, io.state     :: uo
) is det
<= (
%	asymmetricGame(G, C),
%	parseable(C),
	printable(C)
).


processRunIteration(
	_Parameters,
	MOpinionStream,
	_Directory,
	_RunIndex,
	_AllBirths,
	_AllDeaths,
	ArrayPhenotypes,
	IterationIndex,
	!FeedbackAsList,
	!IO
) :-
		% io.format("\rIteration %d", [i(IterationIndex)], !IO),
		% io.flush_output(io.stdout_stream, !IO),
	(	%
		MOpinionStream = yes(OpinionStream),
		printOpinionDynamics(OpinionStream, IterationIndex, ArrayPhenotypes, !IO)
	;
		MOpinionStream = no
	)
	.

/*
processOpinionDynamics(Parameters, OpinionStream, AllBirths, MapStrategyIndex, ArrayPhenotypes, IterationIndex, !ArrayColumns, !IO) :-
	array.lookup(ArrayPhenotypes, IterationIndex) = IterationPhenotypicRecords,
	ProcessOpinionUncertainty =
	(pred(PPR::in, !.AC::di, !:AC::uo) is det :-
		(if
			PPR^selection = opinion(OpinionValue, Uncertainty),
			ebea.streams.birth.search(PPR^id, AllBirths, PBR),
			map.lookup(MapStrategyIndex, PBR^chromosome^strategyGenes, SI)
		then
			YIndexOV = SI ,
			io.print(OpinionStream, IterationIndex, !IO),
			io.print(OpinionStream, ' ', !IO),
			io.print(OpinionStream, OpinionValue, !IO),
			io.print(OpinionStream, ' ', !IO),
			io.print(OpinionStream, Uncertainty, !IO),
			io.nl(OpinionStream, !IO)
		else
			true
		)
	),
	list.foldl(ProcessOpinionUncertainty, IterationPhenotypicRecords^phenotypes, !ArrayColumns)
*/	
	

:- pred printOpinionDynamics(io.output_stream, int, array.array(ebea.streams.phenotype.iterationPhenotypicRecords), io.state, io.state).
:- mode printOpinionDynamics(in, in, in, di, uo) is det.

printOpinionDynamics(OpinionStream, IterationIndex, ArrayPhenotypes, !IO) :-
	array.lookup(ArrayPhenotypes, IterationIndex) = IterationPhenotypicRecords,
	(if
		IterationPhenotypicRecords^iteration \= IterationIndex
	then
		throw("printOpinionDynamics/4: problem in phenotype stream")
	else
		true
	),
	PrintOpinionUncertainty =
	(pred(PPR::in, !.IO::di, !:IO::uo) is det :-
		(if
			PPR^selection = opinion(OpinionValue, Uncertainty)
		then
			io.print(OpinionStream, IterationIndex, !IO),
			io.print(OpinionStream, ' ', !IO),
			io.print(OpinionStream, OpinionValue, !IO),
			io.print(OpinionStream, ' ', !IO),
			io.print(OpinionStream, Uncertainty, !IO),
			io.nl(OpinionStream, !IO)
		else
			true
		)
	),
	list.foldl(PrintOpinionUncertainty, IterationPhenotypicRecords^phenotypes, !IO)
	.

:- pred opinionChromosomes(ebea.population.configuration.configuration(CS, A)).
:- mode opinionChromosomes(in) is semidet.

opinionChromosomes(L) :-
	list.member(Site, L^sites),
	list.member(InitialPlayer, Site^chromosomes),
	(	%
		InitialPlayer^chromosome^selectionGenes = opinion_old(_, _)
	;
		InitialPlayer^chromosome^selectionGenes = opinion_old(_, _, _, _)
	),
	trace[io(!IO)] (io.format("%s\n", [s(string(InitialPlayer))], !IO)).

:- end_module tools.processPhenotype.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
