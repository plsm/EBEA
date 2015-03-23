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
		fileNamePrefix       :: string
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
		PrintOpinionDynamics = bool.and(
			Parameters^printOpinionDynamics,
			bool.pred_to_bool(opinionChromosomes(InitialPopulation))),
		int.fold_up2(
			processRun_s1(
				Config,
				'printOpinionDynamics :='(Parameters, PrintOpinionDynamics),
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
		data.util.gameConfig(Config) = gcex(Game, _, _),
		processRun_s2(
			Config,
			Game,
			Parameters,
			Directory,
			RunIndex,
			IRAllBirths,
			IRAllDeaths,
			Streams^bisPhenotype,
			!FeedbackAsList,
			!IO),
		ebea.streams.closeInputStreams(Streams, !IO)
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
	ioResult(list(iterationBirthRecords(C))) :: in,
	ioResult(list(iterationDeathRecords))    :: in,
	io.binary_input_stream                   :: in,
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
	PhenotypeStream,
	!FeedbackAsList,
	!IO)
:-
	(if
		IRAllBirths = ok(AllBirths),
		IRAllDeaths = ok(AllDeaths)
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
		loopRunIteration(
			Parameters,
			PhenotypeStream,
			MStreamOpinion,
			AllBirths,
			AllDeaths,
			0,
			parseable.iou.cacheInit, CachePhenotypes,
			!FeedbackAsList,
			!IO
		),
		io.print(CachePhenotypes, !IO),
		io.nl(!IO),
		tools.utils.closeMaybeStream(MStreamOpinion, !IO)
	else
		list.cons("error reading streams birth, death or phenotype", !FeedbackAsList)
	).

:- pred loopRunIteration(
	tools.processPhenotype.parameters :: in,
	io.binary_input_stream            :: in,
	maybe(io.output_stream)           :: in,
	list(iterationBirthRecords(C))    :: in,
	list(iterationDeathRecords)       :: in,
	int                               :: in,
	parseable.iou.cache :: in,  parseable.iou.cache :: out,
	list(string)        :: in,  list(string)        :: out,
	io.state            :: di,  io.state            :: uo
) is det
<= (
	printable(C)
).

loopRunIteration(
	Parameters,
	PhenotypeStream,					  
	MOpinionStream,
	AllBirths,
	AllDeaths,
	IterationIndex,
	!CachePhenotypeStream,
	!FeedbackAsList,
	!IO
) :-
	ebea.streams.phenotype.read(PhenotypeStream, RIResult, !CachePhenotypeStream, !IO),
	(	% switch
		RIResult = parseError,
		list.cons("parse error while reading phenotype file", !FeedbackAsList)
	;
		RIResult = ok(IResult),
		(	% switch
			IResult = eof
		;
			IResult = error(Error),
			list.cons(io.error_message(Error), !FeedbackAsList),
			list.cons("IO error while reading phenotype file:", !FeedbackAsList)
		;
			IResult = ok(Result),
			processRunIteration(
				Parameters,
				MOpinionStream,
				AllBirths,
				AllDeaths,
				Result,
				IterationIndex,
				!FeedbackAsList,
				!IO
			),
			loopRunIteration(
				Parameters,
				PhenotypeStream,					  
				MOpinionStream,
				AllBirths,
				AllDeaths,
				IterationIndex + 1,
				!CachePhenotypeStream,
				!FeedbackAsList,
				!IO
			)
		)
	)
	.


:- pred processRunIteration(
	tools.processPhenotype.parameters :: in,
	maybe(io.output_stream)           :: in,
	list(iterationBirthRecords(C))    :: in,
	list(iterationDeathRecords)       :: in,
	iterationPhenotypicRecords        :: in,
	int                               :: in,
	list(string) :: in, list(string) :: out,
	io.state     :: di, io.state     :: uo
) is det
<= (
	printable(C)
).

processRunIteration(
	_Parameters,
	MOpinionStream,
	_AllBirths,
	_AllDeaths,
	IterationPhenotypicRecords,
	IterationIndex,
	!FeedbackAsList,
	!IO
) :-
	(	% switch
		MOpinionStream = yes(OpinionStream),
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
	;
		MOpinionStream = no
	)
	.

/**
 * opinionChromosomes(Configuration)
 
 * Check if there is any player with opinion selection genes.  If there are any,
 * players' opinion is printed depending on field {@code printOpinionDynamics}.

 * @param Configuration Population configuration
 */
:- pred opinionChromosomes(ebea.population.configuration.configuration(CS, A)).
:- mode opinionChromosomes(in) is semidet.

opinionChromosomes(L) :-
	list.member(Site, L^sites),
	list.member(InitialPlayer, Site^chromosomes),
	(	% switch
		InitialPlayer^chromosome^selectionGenes = opinion(_, _, _, _, _)
	;
		InitialPlayer^chromosome^selectionGenes = opinion_old(_, _)
	;
		InitialPlayer^chromosome^selectionGenes = opinion_old(_, _, _, _)
	).

:- end_module tools.processPhenotype.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
