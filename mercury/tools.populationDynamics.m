/**
 * Create a text file with population dynamics: population size, number of
 * births, number of deaths per cause, strategy parameters
 * characterisation.  This tool only works on a set of simulation runs
 * where the stored data level was {@code dynamicBin}.

 * @author Pedro Mariano
 * @version 1.0 2014/03/19
 */
:- module tools.populationDynamics.

:- interface.

:- import_module data, data.config.
:- import_module userInterface.
:- import_module io, list.

:- type parameters --->
	parameters(
		runs                  :: list(int) ,
		fileNamePrefix        :: string
	).

/**
 * Return a default value of {@code parameters}.
 */
:- func default_parameters = tools.populationDynamics.parameters.

/**
 * dialog_parameters = Parameters
  
 * The logical specification of the user dialog to edit the parameters of
 * the tool that creates gnuplot and text files with population dynamics.
  
 */
:- func dialog_parameters = list(dialogItem(tools.populationDynamics.parameters)).

/**
 * runTool(Config, Parameters, Directory, Feedback, !IO)
  
 * Run the population dynamics tool.
 */
:- pred runTool(data.config.config, tools.populationDynamics.parameters, string, string, io.state, io.state).
:- mode runTool(in, in, in, out, di, uo) is det.

:- func runs(tools.populationDynamics.parameters) = list(int).

:- func fileNamePrefix(tools.populationDynamics.parameters) = string.
:- func 'fileNamePrefix :='(tools.populationDynamics.parameters, string) = tools.populationDynamics.parameters.

:- implementation.

:- import_module parseable, parseable.iou.
:- import_module game, foldable, printable.
:- import_module data.util.
:- import_module ebea, ebea.player, ebea.player.selection, ebea.player.selection.pcv, ebea.population, ebea.population.parameters, ebea.population.players, ebea.streams, ebea.streams.birth, ebea.streams.death, ebea.streams.phenotype.
:- import_module array, bool, exception, int, maybe, set, solutions, string.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

default_parameters = parameters(
	[],
	"population-dynamics"
	).

dialog_parameters =
	[
	di(label("runs to process"),          updateListFieldInt(  runs,                   set_runs)),
	di(label("filename prefix"),          updateFieldString(   fileNamePrefix,         set('fileNamePrefix :=')))
	].

runTool(Data, Parameters, Directory, Feedback, !IO) :-
	data.util.gameConfig(Data) = data.util.gcex(Game, _, InitialPopulation),
	Level = Data^level,
	(	%
		Level = detailedBin,
		Runs = Parameters^runs,
		(
			Runs = [],
			RunIndexes = set.from_list(1..Data^numberRuns)
			;
			Runs = [_ | _],
			RunIndexes = set.intersect(set.from_list(1..Data^numberRuns), set.from_list(Runs))
		),
		set.fold2(
			create_populationDynamics_forRun_s1(Data, Game, ebea.player.initAc, InitialPopulation, Parameters, Directory),
			RunIndexes,
			[], FeedbackAsList,
			!IO),
		(
			FeedbackAsList = [],
			Feedback = "ok"
			;
			FeedbackAsList = [_|_],
			Feedback = string(FeedbackAsList)
		)
		;
		Level = detailedTxt,
		Feedback = "Parsing of detailed text files is not supported"
		;
		Level = dynamics,
		Feedback = "The simulation runs already produced population dynamics"
		;
		Level = summary,
		Feedback = "The simulation runs did not produce data to recreate population dynamics"
	).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

/**
 * opens the streams for reading and if no error occurs, continues to predicate
 * {@code createProbabilityCombinationVectorsNetworksForRun_s2/9}
 
 */
:- pred create_populationDynamics_forRun_s1(
	data.config.config            :: in,
	G                             :: in,
	ebea.player.ac(A)                        :: in,
	ebea.population.parameters.parameters(C) :: in,
	tools.populationDynamics.parameters :: in,
	string :: in,
	int                           :: in,
	list(string) :: in, list(string) :: out,
	io.state     :: di, io.state     :: uo
) is det <= (
	asymmetricGame(G, C),
	parseable(C),
	printable(C),
	foldable(C, A),
	printable(A)
).

create_populationDynamics_forRun_s1(
	Data,
	Game,
	PlayerAc,
	InitialPopulation,
	Parameters,
	Directory,
	RunIndex,
	!FeedbackAsList,
	!IO
) :-
	ebea.streams.openInputStreams(Directory, Data^level, yes(string.format("_R%d", [i(RunIndex)])), IMStreams, !IO),
	(
		IMStreams = ok(S),
		(if
			S = detailedBin(_, _, _, _)
		then
			Streams = S
		else
			throw("Never reached")
		),
		io.format("Run %d\n", [i(RunIndex)], !IO),
		create_populationDynamics_forRun_s2(
			Data,
			Game,
			PlayerAc,
			InitialPopulation,
			Parameters,
			Streams,
			RunIndex,
			!FeedbackAsList,
			!IO),
		ebea.streams.closeInputStreams(Streams, !IO),
		io.print("\r                      \n", !IO)
		;
		IMStreams = error(ErrorMsg),
		list.cons(ErrorMsg, !FeedbackAsList)
	)
	.



/**
 * Reads the birth records corresponding to the initial population.  If
 * these records where not written we do not write the population dynamics.
 * The initial population may be random.  We can only reconstruct the
 * chromosomes if we have the pseudo-random number generator seed.
 */

:- pred create_populationDynamics_forRun_s2(
	data.config.config                       :: in,
	G                                        :: in,
	ebea.player.ac(A)                        :: in,
	ebea.population.parameters.parameters(C) :: in,
	tools.populationDynamics.parameters      :: in,
	ebea.streams.inStreams                   :: in(detailedBin),
	int                                      :: in,
	list(string) :: in, list(string) :: out,
	io.state     :: di, io.state     :: uo
) is det
<= (
	asymmetricGame(G, C),
	parseable(C),
	printable(C),
	foldable(C, A),
	printable(A)
).

create_populationDynamics_forRun_s2(
	Data,
	Game,
	PlayerAc,
	InitialPopulation,
	Parameters,
	BinStreams,
	RunIndex,
	!FeedbackAsList,
	!IO
) :-
	ebea.streams.birth.read(
		BinStreams^bisBirth,
		-1,
		RIIterationBirthRecords,
		no,                       InitialBirthAdvancedResult,
		parseable.iou.cacheInit,  InitialBirthCache,
		!IO
	),
	(	%
		RIIterationBirthRecords = delayed,
		list.cons(
			"Not implemented creating alive players from a simulation without initial population stored in birth files",
			!FeedbackAsList)
		;
		RIIterationBirthRecords = ok(ok(IterationBirthRecords)),
		IterationBirthRecords = ibr(_, ListBirthRecords),
		(	%
			InitialPopulation^geometry = wellmixed,
			MaxSiteIndex = 0
		;
			InitialPopulation^geometry = lattice(XSize, YSize, _, _),
			MaxSiteIndex = XSize * YSize - 1
		),
		create_populationDynamics_forRun_s3(
			Data,
			MaxSiteIndex,
			Game,
			PlayerAc,
			Parameters,
			BinStreams,
			RunIndex,
			ListBirthRecords,
			InitialBirthAdvancedResult,
			InitialBirthCache,
			!FeedbackAsList,
			!IO
		)
		;
		RIIterationBirthRecords = ok(eof),
		list.cons(
			"Not implemented creating alive players from a simulation without initial population stored in birth files",
			!FeedbackAsList)
		;
		RIIterationBirthRecords = ok(error(Error)),
		list.cons(
			string.format("io error while reading birth file: %s", [s(io.error_message(Error))]),
			!FeedbackAsList)
		;
		RIIterationBirthRecords = parseError,
		list.cons(
			"parse error while reading birth file",
			!FeedbackAsList)
	)
	.


/**
 * Opens in write mode the text file that will contain the population
 * dynamics.
 */

:- pred create_populationDynamics_forRun_s3(
	data.config.config                  :: in,
	int                                 :: in,
	G                                   :: in,
	ebea.player.ac(A)                   :: in,
	tools.populationDynamics.parameters :: in,
	ebea.streams.inStreams              :: in(detailedBin),
	int                                 :: in,
	list(playerBirthRecord(C))          :: in,
	parseable.iou.advancedResult(iterationBirthRecords(C))     :: in,
	parseable.iou.cache                 :: in,
	list(string) :: in, list(string) :: out,
	io.state     :: di, io.state     :: uo
) is det
<= (
	asymmetricGame(G, C),
	parseable(C),
	printable(C),
	foldable(C, A),
	printable(A)
).

create_populationDynamics_forRun_s3(
	Data,
	MaxSiteIndex,
	Game,
	_PlayerAc,
	Parameters,
	BinStreams,
	RunIndex,
	ListBirthRecords,
	InitialBirthAdvancedResult,
	InitialBirthCache,
	!FeedbackAsList,
	!IO
) :-

	FileName = string.format("%s_R%d.txt",
		[s(Parameters^fileNamePrefix),
		 i(RunIndex)]),
	io.open_output(FileName, IStream, !IO),
	(	% switch IStream
		IStream = ok(TextDataStream),
		create_populationDynamics_forRunIteration(
			Data,
			game.numberPlayers(Game),
			MaxSiteIndex,
			Parameters,
			BinStreams,
			TextDataStream,
			0,
			ListBirthRecords,            _FinalAllBirthRecords,
			ListBirthRecords,            _FinalCurrentPopulation,
			InitialBirthAdvancedResult,  FinalBirthAdvancedResult,
			InitialBirthCache,           FinalBirthCache,
			no,                          FinalDeathAdvancedResult,
			parseable.iou.cacheInit,     FinalDeathCache,
			!FeedbackAsList,
			!IO
		),
		io.nl(!IO),
		io.print(FinalBirthAdvancedResult, !IO),
		io.nl(!IO),
		io.print(FinalBirthCache, !IO),
		io.nl(!IO),
		io.print(FinalDeathAdvancedResult, !IO),
		io.nl(!IO),
		io.print(FinalDeathCache, !IO),
		io.nl(!IO),
		io.close_output(TextDataStream, !IO)
	;	
		IStream = error(Error),
		list.cons(string.format("IO error opening `%s` file: %s", [s(FileName), s(io.error_message(Error))]), !FeedbackAsList)
	).




/**
 * Read stream birth to obtain the number of births in the current iteration.
 */

:- pred create_populationDynamics_forRunIteration(
	data.config.config                  :: in,
	int                                 :: in,
	int                                 :: in,
	tools.populationDynamics.parameters :: in,
	ebea.streams.inStreams              :: in(detailedBin),
	io.output_stream                    :: in,
	int                                 :: in,
	list(playerBirthRecord(C))                             :: in, list(playerBirthRecord(C))          :: out,
	list(playerBirthRecord(C))                             :: in, list(playerBirthRecord(C))          :: out,
	parseable.iou.advancedResult(iterationBirthRecords(C)) :: in, parseable.iou.advancedResult(iterationBirthRecords(C)) :: out,
	parseable.iou.cache                                    :: in, parseable.iou.cache             :: out,
	parseable.iou.advancedResult(iterationDeathRecords)    :: in, parseable.iou.advancedResult(iterationDeathRecords)    :: out,
	parseable.iou.cache                                    :: in, parseable.iou.cache             :: out,
	list(string)                                           :: in, list(string) :: out,
	io.state                                               :: di, io.state     :: uo
) is det <= (
	parseable(C),
	printable(C),
	foldable(C, A),
	printable(A)
).

create_populationDynamics_forRunIteration(
	Data,
	NumberPlayers,
	MaxSiteIndex,
	Parameters,
	BinStreams,
	TextDataStream,
	IterationIndex,
	!AllBirthRecords,
	!CurrentPopulation,
	!BirthAdvancedResult,
	!BirthCache,
	!DeathAdvancedResult,
	!DeathCache,
	!FeedbackAsList,
	!IO
) :-
	(if
		IterationIndex < Data^numberIterations,
		list.length(!.CurrentPopulation) >= NumberPlayers
	then
		Continue = yes
	else
		Continue = no
	),
	ebea.streams.birth.read(BinStreams^bisBirth, IterationIndex, RIIterationBirthRecords, !BirthAdvancedResult, !BirthCache, !IO),
	(	%/* switch */
		(	%
			RIIterationBirthRecords = delayed,
			ListBirthRecords = []
		;	
			RIIterationBirthRecords = ok(ok(IterationBirthRecords)),
			IterationBirthRecords = ibr(_, ListBirthRecords),
			list.append(ListBirthRecords, !CurrentPopulation),
			list.append(ListBirthRecords, !AllBirthRecords)
		;	
			RIIterationBirthRecords = ok(eof),
			ListBirthRecords = []
		),
		ebea.streams.death.read(BinStreams^bisDeath, IterationIndex, RIIterationDeathRecords, !DeathAdvancedResult, !DeathCache, !IO),
		(	% switch
			(	%
				RIIterationDeathRecords = delayed,
				DeathByCarryingCapacity = [],
				DeathByOldAge = [],
				DeathByStarvation = []
			;	
				RIIterationDeathRecords = ok(ok(IterationDeathRecords)),
				DeathByCarryingCapacity = IterationDeathRecords^carryingCapacity,
				DeathByOldAge = IterationDeathRecords^oldAge,
				DeathByStarvation = IterationDeathRecords^starvation,
				!:CurrentPopulation = removeDeaths(!.CurrentPopulation, IterationDeathRecords)
			;
				RIIterationDeathRecords = ok(eof),
				DeathByCarryingCapacity = [],
				DeathByOldAge = [],
				DeathByStarvation = []
			),
			io.print(TextDataStream, IterationIndex, !IO),
			io.print(TextDataStream, '\t', !IO),
			io.print(TextDataStream, list.length(!.CurrentPopulation) `with_type` int, !IO),
			io.print(TextDataStream, "\t", !IO),
			io.print(TextDataStream, list.length(ListBirthRecords) `with_type` int, !IO),
			io.print(TextDataStream, '\t', !IO),
			io.print(TextDataStream, list.length(DeathByCarryingCapacity) `with_type` int, !IO),
			io.print(TextDataStream, '\t', !IO),
			io.print(TextDataStream, list.length(DeathByOldAge) `with_type` int, !IO),
			io.print(TextDataStream, '\t', !IO),
			io.print(TextDataStream, list.length(DeathByStarvation) `with_type` int, !IO),
			io.print(TextDataStream, '\t', !IO),
			ebea.player.printAc(TextDataStream, ebea.streams.birth.foldlAll(!.CurrentPopulation), !IO),
			(if
				MaxSiteIndex > 0
			then
				tools.populationDynamics.printSite(
					TextDataStream,
					!.AllBirthRecords,
					!.CurrentPopulation,
					ListBirthRecords,
					DeathByCarryingCapacity,
					DeathByOldAge,
					DeathByStarvation,
					0, MaxSiteIndex,
					!IO
				)
			else
				true
			),
			io.nl(TextDataStream, !IO),
			(if
				Continue = yes
			then
				create_populationDynamics_forRunIteration(
					Data,
					NumberPlayers,
					MaxSiteIndex,
					Parameters,
					BinStreams,
					TextDataStream,
					IterationIndex + 1,
					!AllBirthRecords,
					!CurrentPopulation,
					!BirthAdvancedResult,
					!BirthCache,
					!DeathAdvancedResult,
					!DeathCache,
					!FeedbackAsList,
					!IO
				)
			else
				true
			)
		;	
			RIIterationDeathRecords = ok(error(Error)),
			list.cons(
				 string.format("io error while reading death file: %s", [s(io.error_message(Error))]),
				 !FeedbackAsList)
		;	
			RIIterationDeathRecords = parseError,
			list.cons(
				"parse error while reading death file",
				!FeedbackAsList)
		)
	;	
		RIIterationBirthRecords = ok(error(Error)),
		list.cons(string.format("io error while reading birth file: %s", [s(io.error_message(Error))]), !FeedbackAsList)
	;	
		RIIterationBirthRecords = parseError,
		list.cons("parse error while reading birth file", !FeedbackAsList)
	)
	.


:- pred printSite(
	io.output_stream           :: in,
	list(playerBirthRecord(C)) :: in,
	list(playerBirthRecord(C)) :: in,
	list(playerBirthRecord(C)) :: in,
	list(key)                  :: in,
	list(key)                  :: in,
	list(key)                  :: in,
	int                        :: in,
	int                        :: in,
	io.state :: di, io.state :: uo
) is det <= (
	printable(C),
	foldable(C, A),
	printable(A)
).

printSite(
	TextDataStream,
	AllBirthRecords,
	CurrentPopulation,
	ListBirthRecords,
	DeathByCarryingCapacity,
	DeathByOldAge,
	DeathByStarvation,
	ThisSiteIndex,
	MaxSiteIndex,
	!IO
) :-
	io.print(TextDataStream, '\t', !IO),
	io.print(TextDataStream, ebea.streams.birth.birthsAtSite(CurrentPopulation, ThisSiteIndex), !IO),
	io.print(TextDataStream, '\t', !IO),
	io.print(TextDataStream, ebea.streams.birth.birthsAtSite(ListBirthRecords, ThisSiteIndex), !IO),
	io.print(TextDataStream, '\t', !IO),
	io.print(TextDataStream, ebea.streams.death.deathsAtSite(AllBirthRecords, DeathByCarryingCapacity, ThisSiteIndex), !IO),
	io.print(TextDataStream, '\t', !IO),
	io.print(TextDataStream, ebea.streams.death.deathsAtSite(AllBirthRecords, DeathByOldAge, ThisSiteIndex), !IO),
	io.print(TextDataStream, '\t', !IO),
	io.print(TextDataStream, ebea.streams.death.deathsAtSite(AllBirthRecords, DeathByStarvation, ThisSiteIndex), !IO),
	io.print(TextDataStream, '\t', !IO),
	ebea.player.printAc(TextDataStream, ebea.streams.birth.foldlSite(CurrentPopulation, ThisSiteIndex), !IO),
	(if
		ThisSiteIndex < MaxSiteIndex
	then
		printSite(
			TextDataStream,
			AllBirthRecords,
			CurrentPopulation,
			ListBirthRecords,
			DeathByCarryingCapacity,
			DeathByOldAge,
			DeathByStarvation,
			ThisSiteIndex + 1,
			MaxSiteIndex,
			!IO
		)
	else
		true
	)
	.


% /**
%  * Read stream birth to obtain the number of births in the current iteration.
%  */

% :- pred create_populationDynamics_forRunIteration(
% 	data.config.config                  :: in,
% 	tools.populationDynamics.parameters :: in,
% 	ebea.streams.inStreams              :: in(detailedBin),
% 	io.output_stream                    :: in,
% 	int                                 :: in,
% 	list(playerBirthRecord(C))          :: in,
% 	bool                                :: in,
% 	bool                                :: in,
% 	parseable.iou.advancedResult(iterationBirthRecords(C)) :: in, parseable.iou.advancedResult(iterationBirthRecords(C)) :: out,
% 	parseable.iou.cache             :: in, parseable.iou.cache             :: out,
% 	parseable.iou.advancedResult(iterationDeathRecords)    :: in, parseable.iou.advancedResult(iterationDeathRecords)    :: out,
% 	parseable.iou.cache             :: in, parseable.iou.cache             :: out,
% 	list(string) :: in, list(string) :: out,
% 	io.state     :: di, io.state     :: uo
% ) is det <= (
% 	parseable(C),
% 	printable(C),
% 	foldable(C, A),
% 	printable(A)
% ).

% create_populationDynamics_forRunIteration(
% 	Data,
% 	Parameters,
% 	BinStreams,
% 	TextDataStream,
% 	IterationIndex,
% 	ListBirthRecords,
% 	TBStop,
% 	TDStop,
% 	!BirthAdvancedResult,
% 	!BirthCache,
% 	!DeathAdvancedResult,
% 	!DeathCache,
% 	!FeedbackAsList,
% 	!IO
% ) :-
% 	TBStop = yes,
% 	TDStop = yes,
% 	io.nl(!IO)
% 	;
% 	TBStop = yes,
% 	TDStop = no,
% 	% io.format("\rIteration %d", [i(IterationIndex)], !IO),
% 	% io.flush_output(io.stdout_stream, !IO),
% 	ebea.streams.death.read(
% 		BinStreams^bisDeath,
% 		IterationIndex,
% 		RIIterationDeathRecords,
% 		!DeathAdvancedResult,
% 		!DeathCache,
% 		!IO
% 	),
% 	(	% switch RIIterationPhenotypicRecords,
% 		(	%
% 			RIIterationDeathRecords = delayed,
% 			NumberDeaths_carryingCapacity = 0,
% 			NumberDeaths_oldAge = 0,
% 			NumberDeaths_starvation = 0,
% 			NextBirths = ListBirthRecords,
% 			NDStop = no
% 		;	
% 			RIIterationDeathRecords = ok(ok(IterationDeathRecords)),
% 			NumberDeaths_carryingCapacity = list.length(IterationDeathRecords^carryingCapacity),
% 			NumberDeaths_oldAge = list.length(IterationDeathRecords^oldAge),
% 			NumberDeaths_starvation = list.length(IterationDeathRecords^starvation),
% 			NextBirths = removeDeaths(ListBirthRecords, IterationDeathRecords),
% 			NDStop = no
% 		;
% 			RIIterationDeathRecords = ok(eof),
% 			NumberDeaths_carryingCapacity = 0,
% 			NumberDeaths_oldAge = 0,
% 			NumberDeaths_starvation = 0,
% 			NextBirths = ListBirthRecords,
% 			NDStop = yes
% 		),
% 		io.print(TextDataStream, IterationIndex, !IO),
% 		io.print(TextDataStream, '\t', !IO),
% 		io.print(TextDataStream, list.length(ListBirthRecords) `with_type` int, !IO),
% 		io.print(TextDataStream, "\t0\t", !IO),
% 		io.print(TextDataStream, NumberDeaths_carryingCapacity, !IO),
% 		io.print(TextDataStream, '\t', !IO),
% 		io.print(TextDataStream, NumberDeaths_oldAge, !IO),
% 		io.print(TextDataStream, '\t', !IO),
% 		io.print(TextDataStream, NumberDeaths_starvation, !IO),
% 		io.print(TextDataStream, '\t', !IO),
% 		ebea.player.printAc(TextDataStream, ebea.streams.birth.foldlAll(ListBirthRecords), !IO),
% 		io.nl(TextDataStream, !IO),
% 		create_populationDynamics_forRunIteration(
% 			Data,
% 			Parameters,
% 			BinStreams,
% 			TextDataStream,
% 			IterationIndex + 1,
% 			NextBirths,
% 			yes,
% 			NDStop,
% 			!BirthAdvancedResult,
% 			!BirthCache,
% 			!DeathAdvancedResult,
% 			!DeathCache,
% 			!FeedbackAsList,
% 			!IO
% 		)
% 	;
% 		RIIterationDeathRecords = ok(error(Error)),
% 		list.cons(
% 			string.format("io error while reading death file: %s", [s(io.error_message(Error))]),
% 			!FeedbackAsList)
% 		;
% 		RIIterationDeathRecords = parseError,
% 		list.cons(
% 			"parse error while reading death file",
% 			!FeedbackAsList)
% 	)
% 	;
% 	TBStop = no,
% 	TDStop = yes,
% 	% io.format("\rIteration %d", [i(IterationIndex)], !IO),
% 	% io.flush_output(io.stdout_stream, !IO),
% 	ebea.streams.birth.read(
% 		BinStreams^bisBirth,
% 		IterationIndex,
% 		RIIterationBirthRecords,
% 		!BirthAdvancedResult,
% 		!BirthCache,
% 		!IO
% 	),
% 	(	% switch RIIterationPhenotypicRecords,
% 		(
% 			RIIterationBirthRecords = delayed,
% 			NewBirths = [],
% 			NBStop = no
% 		;	
% 			RIIterationBirthRecords = ok(ok(IterationBirthRecords)),
% 			IterationBirthRecords = ibr(_, NewBirths),
% 			NBStop = no
% 		;
% 			RIIterationBirthRecords = ok(eof),
% 			NewBirths = [],
% 			NBStop = yes
% 		),
% 		io.print(TextDataStream, IterationIndex, !IO),
% 		io.print(TextDataStream, '\t', !IO),
% 		io.print(TextDataStream, list.length(ListBirthRecords) `with_type` int, !IO),
% 		io.print(TextDataStream, '\t', !IO),
% 		io.print(TextDataStream, list.length(NewBirths) `with_type` int, !IO),
% 		io.print(TextDataStream, "\t0\t0\t0\t", !IO),
% 		ebea.player.printAc(TextDataStream, ebea.streams.birth.foldlAll(ListBirthRecords), !IO),
% 		io.nl(TextDataStream, !IO),
% 		create_populationDynamics_forRunIteration(
% 			Data,
% 			Parameters,
% 			BinStreams,
% 			TextDataStream,
% 			IterationIndex + 1,
% 			list.append(ListBirthRecords, NewBirths),
% 			NBStop,
% 			yes,
% 			!BirthAdvancedResult,
% 			!BirthCache,
% 			!DeathAdvancedResult,
% 			!DeathCache,
% 			!FeedbackAsList,
% 			!IO
% 		)
% 	;	
% 		RIIterationBirthRecords = ok(error(Error)),
% 		list.cons(
% 			string.format("io error while reading birth file: %s", [s(io.error_message(Error))]),
% 			!FeedbackAsList)
% 		;
% 		RIIterationBirthRecords = parseError,
% 		list.cons(
% 			"parse error while reading birth file",
% 			!FeedbackAsList)
% 	)
% 	;
% 	TBStop = no,
% 	TDStop = no,
% 	% io.format("\rIteration %d", [i(IterationIndex)], !IO),
% 	% io.flush_output(io.stdout_stream, !IO),
% 	ebea.streams.birth.read(
% 		BinStreams^bisBirth,
% 		IterationIndex,
% 		RIIterationBirthRecords,
% 		!BirthAdvancedResult,
% 		!BirthCache,
% 		!IO
% 	),
% 	(	% switch RIIterationPhenotypicRecords,
% 		(
% 			RIIterationBirthRecords = delayed,
% 			NewBirths = [],
% 			NBStop = no
% 		;	
% 			RIIterationBirthRecords = ok(ok(IterationBirthRecords)),
% 			IterationBirthRecords = ibr(_, NewBirths),
% 			NBStop = no
% 		;
% 			RIIterationBirthRecords = ok(eof),
% 			NewBirths = [],
% 			NBStop = yes
% 		),
% 		ebea.streams.death.read(
% 			BinStreams^bisDeath,
% 			IterationIndex,
% 			RIIterationDeathRecords,
% 			!DeathAdvancedResult,
% 			!DeathCache,
% 			!IO
% 		),
% 		(	% switch RIIterationPhenotypicRecords,
% 			(
% 				RIIterationDeathRecords = delayed,
% 				NumberDeaths_carryingCapacity = 0,
% 				NumberDeaths_oldAge = 0,
% 				NumberDeaths_starvation = 0,
% 				NextBirths = list.append(NewBirths, ListBirthRecords),
% 				NDStop = no
% 			;	
% 				RIIterationDeathRecords = ok(ok(IterationDeathRecords)),
% 				NumberDeaths_carryingCapacity = list.length(IterationDeathRecords^carryingCapacity),
% 				NumberDeaths_oldAge = list.length(IterationDeathRecords^oldAge),
% 				NumberDeaths_starvation = list.length(IterationDeathRecords^starvation),
% 				NextBirths = removeDeaths(list.append(NewBirths, ListBirthRecords), IterationDeathRecords),
% 				NDStop = no
% 			;
% 				RIIterationDeathRecords = ok(eof),
% 				NumberDeaths_carryingCapacity = 0,
% 				NumberDeaths_oldAge = 0,
% 				NumberDeaths_starvation = 0,
% 				NextBirths = list.append(NewBirths, ListBirthRecords),
% 				NDStop = yes
% 			),
% 			io.print(TextDataStream, IterationIndex, !IO),
% 			io.print(TextDataStream, '\t', !IO),
% 			io.print(TextDataStream, list.length(ListBirthRecords) `with_type` int, !IO),
% 			io.print(TextDataStream, '\t', !IO),
% 			io.print(TextDataStream, list.length(NewBirths) `with_type` int, !IO),
% 			io.print(TextDataStream, '\t', !IO),
% 			io.print(TextDataStream, NumberDeaths_carryingCapacity, !IO),
% 			io.print(TextDataStream, '\t', !IO),
% 			io.print(TextDataStream, NumberDeaths_oldAge, !IO),
% 			io.print(TextDataStream, '\t', !IO),
% 			io.print(TextDataStream, NumberDeaths_starvation, !IO),
% 			io.print(TextDataStream, '\t', !IO),
% 			ebea.player.printAc(TextDataStream, ebea.streams.birth.foldlAll(ListBirthRecords), !IO),
% 			io.nl(TextDataStream, !IO),
% 			create_populationDynamics_forRunIteration(
% 				Data,
% 				Parameters,
% 				BinStreams,
% 				TextDataStream,
% 				IterationIndex + 1,
% 				NextBirths,
% 				NBStop,
% 				NDStop,
% 				!BirthAdvancedResult,
% 				!BirthCache,
% 				!DeathAdvancedResult,
% 				!DeathCache,
% 				!FeedbackAsList,
% 				!IO
% 			)
% 		;
% 			RIIterationDeathRecords = ok(error(Error)),
% 			list.cons(
% 				string.format("io error while reading death file: %s", [s(io.error_message(Error))]),
% 				!FeedbackAsList)
% 			;
% 			RIIterationDeathRecords = parseError,
% 			list.cons(
% 				"parse error while reading death file",
% 				!FeedbackAsList)
% 		)
% 	;	
% 		RIIterationBirthRecords = ok(error(Error)),
% 		list.cons(
% 			string.format("io error while reading birth file: %s", [s(io.error_message(Error))]),
% 			!FeedbackAsList)
% 		;
% 		RIIterationBirthRecords = parseError,
% 		list.cons(
% 			"parse error while reading birth file",
% 			!FeedbackAsList)
% 	)
% 	.




% /**
%  * Read stream birth to obtain the number of births in the current iteration.
%  */

% :- pred create_populationDynamics_forRunIteration_s1(
% 	data.config.config                  :: in,
% 	G                                   :: in,
% 	tools.populationDynamics.parameters :: in,
% 	ebea.streams.inStreams              :: in(detailedBin),
% 	io.output_stream                    :: in,
% 	int                                 :: in,
% 	list(playerBirthRecord(C))          :: in,
% 	bool                                :: in,
% 	bool                                :: in,
% 	maybe(iterationBirthRecords(C)) :: in, maybe(iterationBirthRecords(C)) :: out,
% 	parseable.iou.cache             :: in, parseable.iou.cache             :: out,
% 	maybe(iterationDeathRecords)    :: in, maybe(iterationDeathRecords)    :: out,
% 	parseable.iou.cache             :: in, parseable.iou.cache             :: out,
% 	list(string) :: in, list(string) :: out,
% 	io.state     :: di, io.state     :: uo
% ) is det <= (
% 	asymmetricGame(G, C),
% 	parseable(C)
% ).

% create_populationDynamics_forRunIteration_s1(
% 	Data,
% 	Game,
% 	Parameters,
% 	BinStreams,
% 	TextDataStream,
% 	IterationIndex,
% 	ListBirthRecords,
% 	TBStop,
% 	TDStop,
% 	!BirthAdvancedResult,
% 	!BirthCache,
% 	!DeathAdvancedResult,
% 	!DeathCache,
% 	!FeedbackAsList,
% 	!IO
% ) :-
% 	TBStop = yes,
% 	io.format("\rIteration %d", [i(IterationIndex)], !IO),
% 	io.flush_output(io.stdout_stream, !IO),
% 	create_populationDynamics_forRunIteration_s2(
% 		Data,
% 		Game,
% 		Parameters,
% 		BinStreams,
% 		TextDataStream,
% 		IterationIndex,
% 		ListBirthRecords,
% 		[],
% 		yes,
% 		TDStop,
% 		!BirthAdvancedResult,
% 		!BirthCache,
% 		!DeathAdvancedResult,
% 		!DeathCache,
% 		!FeedbackAsList,
% 		!IO
% 	)
% 	;
% 	TBStop = no,
% 	io.format("\rIteration %d", [i(IterationIndex)], !IO),
% 	io.flush_output(io.stdout_stream, !IO),
% 	ebea.streams.birth.read(
% 		BinStreams^bisBirth,
% 		IterationIndex,
% 		RIIterationBirthRecords,
% 		!BirthAdvancedResult,
% 		!BirthCache,
% 		!IO
% 	),
% 	(	% switch RIIterationPhenotypicRecords,
% 		(	%
% 			RIIterationBirthRecords = delayed,
% 			NewBirths = [],
% 			NBStop = no
% 		;	
% 			RIIterationBirthRecords = ok(ok(IterationBirthRecords)),
% 			IterationBirthRecords = ibr(_, NewBirths),
% 			NBStop = no
% 		;
% 			RIIterationBirthRecords = ok(eof),
% 			NewBirths = [],
% 			NBStop = yes
% 		),
% 		create_populationDynamics_forRunIteration_s2(
% 			Data,
% 			Game,
% 			Parameters,
% 			BinStreams,
% 			TextDataStream,
% 			IterationIndex,
% 			ListBirthRecords,
% 			NewBirths,
% 			NBStop,
% 			TDStop,
% 			!BirthAdvancedResult,
% 			!BirthCache,
% 			!DeathAdvancedResult,
% 			!DeathCache,
% 			!FeedbackAsList,
% 			!IO
% 		)
% 	;	
% 		RIIterationBirthRecords = ok(error(Error)),
% 		list.cons(
% 			string.format("io error while reading birth file: %s", [s(io.error_message(Error))]),
% 			!FeedbackAsList)
% 		;
% 		RIIterationBirthRecords = parseError,
% 		list.cons(
% 			"parse error while reading birth file",
% 			!FeedbackAsList)
% 	)
% 	.



% /**
%  * Read stream death to obtain the number of births in the current iteration.
%  */

% :- pred create_populationDynamics_forRunIteration_s2(
% 	data.config.config                  :: in,
% 	G                                   :: in,
% 	tools.populationDynamics.parameters :: in,
% 	ebea.streams.inStreams              :: in(detailedBin),
% 	io.output_stream                    :: in,
% 	int                                 :: in,
% 	list(playerBirthRecord(C))          :: in,
% 	list(playerBirthRecord(C))          :: in,
% 	bool                                :: in,
% 	bool                                :: in,
% 	maybe(iterationBirthRecords(C)) :: in, maybe(iterationBirthRecords(C)) :: out,
% 	parseable.iou.cache             :: in, parseable.iou.cache             :: out,
% 	maybe(iterationDeathRecords)    :: in, maybe(iterationDeathRecords)    :: out,
% 	parseable.iou.cache             :: in, parseable.iou.cache             :: out,
% 	list(string) :: in, list(string) :: out,
% 	io.state     :: di, io.state     :: uo
% ) is det <= (
% 	asymmetricGame(G, C),
% 	parseable(C)
% ).

% create_populationDynamics_forRunIteration_s2(
% 	Data,
% 	Game,
% 	Parameters,
% 	BinStreams,
% 	TextDataStream,
% 	IterationIndex,
% 	ListBirthRecords,
% 	NewBirths,
% 	NBStop,
% 	TDStop,
% 	!BirthAdvancedResult,
% 	!BirthCache,
% 	!DeathAdvancedResult,
% 	!DeathCache,
% 	!FeedbackAsList,
% 	!IO
% ) :-
% 	TDStop = yes,
% 	io.print(TextDataStream, IterationIndex, !IO),
% 	io.print(TextDataStream, '\t', !IO),
% 	io.print(TextDataStream, list.length(ListBirthRecords) `with_type` int, !IO),
% 	io.print(TextDataStream, '\t', !IO),
% 	io.print(TextDataStream, list.length(NewBirths)`with_type` int, !IO),
% 	io.print(TextDataStream, "\t0\t0\t0\n", !IO),
% 	(if
% 		NBStop = yes
% 	then
% 		true
% 	else
% 		create_populationDynamics_forRunIteration_s1(
% 			Data,
% 			Game,
% 			Parameters,
% 			BinStreams,
% 			TextDataStream,
% 			IterationIndex,
% 			list.append(ListBirthRecords, NewBirths),
% 			no,
% 			yes,
% 			!BirthAdvancedResult,
% 			!BirthCache,
% 			!DeathAdvancedResult,
% 			!DeathCache,
% 			!FeedbackAsList,
% 			!IO
% 		)
% 	)
% 	;
% 	TDStop = no,
% 	ebea.streams.death.read(
% 		BinStreams^bisDeath,
% 		IterationIndex,
% 		RIIterationDeathRecords,
% 		!DeathAdvancedResult,
% 		!DeathCache,
% 		!IO
% 	),
% 	(	% switch RIIterationPhenotypicRecords,
% 		(	%
% 			RIIterationDeathRecords = delayed,
% 			NumberDeaths_carryingCapacity = 0,
% 			NumberDeaths_oldAge = 0,
% 			NumberDeaths_starvation = 0,
% 			NDStop = no,
% 			NextBirths = list.append(ListBirthRecords, NewBirths)
% 		;	
% 			RIIterationDeathRecords = ok(ok(IterationDeathRecords)),
% 			NumberDeaths_carryingCapacity = list.length(IterationDeathRecords^carryingCapacity),
% 			NumberDeaths_oldAge = list.length(IterationDeathRecords^oldAge),
% 			NumberDeaths_starvation = list.length(IterationDeathRecords^starvation),
% 			NextBirths = removeDeaths(list.append(ListBirthRecords, NewBirths), IterationDeathRecords),
% 			NDStop = no
% 		;
% 			RIIterationDeathRecords = ok(eof),
% 			NumberDeaths_carryingCapacity = 0,
% 			NumberDeaths_oldAge = 0,
% 			NumberDeaths_starvation = 0,
% 			NDStop = yes,
% 			NextBirths = list.append(ListBirthRecords, NewBirths)
% 		),
% 		io.print(TextDataStream, IterationIndex, !IO),
% 		io.print(TextDataStream, '\t', !IO),
% 		io.print(TextDataStream, list.length(ListBirthRecords) `with_type` int, !IO),
% 		io.print(TextDataStream, '\t', !IO),
% 		io.print(TextDataStream, list.length(NewBirths) `with_type` int, !IO),
% 		io.print(TextDataStream, '\t', !IO),
% 		io.print(TextDataStream, NumberDeaths_carryingCapacity, !IO),
% 		io.print(TextDataStream, '\t', !IO),
% 		io.print(TextDataStream, NumberDeaths_oldAge, !IO),
% 		io.print(TextDataStream, '\t', !IO),
% 		io.print(TextDataStream, NumberDeaths_starvation, !IO),
% 		io.nl(TextDataStream, !IO),
% 		(if
% 			NDStop = yes,
% 			NBStop = yes
% 		then
% 			true
% 		else
% 			create_populationDynamics_forRunIteration_s1(
% 				Data,
% 				Game,
% 				Parameters,
% 				BinStreams,
% 				TextDataStream,
% 				IterationIndex + 1,
% 				NextBirths,
% 				NBStop,
% 				NDStop,
% 				!BirthAdvancedResult,
% 				!BirthCache,
% 				!DeathAdvancedResult,
% 				!DeathCache,
% 				!FeedbackAsList,
% 				!IO
% 			)
% 		)
% 	;	
% 		RIIterationDeathRecords = ok(error(Error)),
% 		list.cons(
% 			string.format("io error while reading death file: %s", [s(io.error_message(Error))]),
% 			!FeedbackAsList)
% 		;
% 		RIIterationDeathRecords = parseError,
% 		list.cons(
% 			"parse error while reading death file",
% 			!FeedbackAsList)
% 	)
% 	.





:- func removeDeaths(list(playerBirthRecord(C)), iterationDeathRecords) = list(playerBirthRecord(C)).

removeDeaths([], _) = [].
removeDeaths([H | T], IDR) = Result :-
	(if
		list.member(H^id, IDR^carryingCapacity) ;
		list.member(H^id, IDR^oldAge) ;
		list.member(H^id, IDR^starvation)
	then
		Result = removeDeaths(T, IDR)
	else
		Result = [H | removeDeaths(T, IDR)]
	).


:- func set_runs(tools.populationDynamics.parameters, list(int)) = userInterface.setResult(tools.populationDynamics.parameters).

set_runs(P, V) = Result :-
	(if
		list.append(_, [H | T], V),
		list.append(_, [H | _], T)
	then
		Result = error("Duplicate run number")
	else if
		list.member(Run, V),
		Run =< 0
	then
		Result = error("Runs indexes must be positive")
	else
		Result = ok('runs :='(P, V))
	).

:- end_module tools.populationDynamics.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
