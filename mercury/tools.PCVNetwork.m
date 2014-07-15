/**
 * This tool creates movies with the evolution of a player's probability
 * and combination vectors.  The user can select which for which players a
 * movie is created.  This movie file name has a suffix stating the run
 * number and the player's ID.

 * @author Pedro Mariano
 * @version 1.0 2014/03/14
 */
:- module tools.'PCVNetwork'.

:- interface.

:- import_module data, data.config.
:- import_module userInterface.
:- import_module bool, io, list.

/**
 * Parameters that control the tool that creates movies with the evolution
 * of a player's probability and combination vectors.
  
 */
:- type parameters --->
	parameters(
		printSelectionGenes   :: bool ,
		printStrategyGenes    :: bool ,
		printProbability      :: bool ,
		printCombinationIndex :: bool ,
		players               :: list(int) ,
		runs                  :: list(int) ,
		fileNamePrefix        :: string
	) .


/**
 * Return a default value of {@code parameters}.
 */
:- func default_parameters = tools.'PCVNetwork'.parameters.

/**
 * dialog_parameters = Parameters
  
 * The logical specification of the user dialog to edit the parameters of
 * the tool that creates movies with the evolution of a player's
 * probability and combination vectors.
  
 */
:- func dialog_parameters = list(dialogItem(tools.'PCVNetwork'.parameters)).

/**
 * runTool(Config, Parameters, Feedback, !IO)
  
 * Run the tool on the files produced by the given configuration.
 * Parameter {@code Parameters} controls the data that is shown in the
 * graphs that represent the probability and combination vectors.  Feedback
 * on possible errors is returned in parameter {@code Feedback}.

 * <p> This tool can only run if the data produced by a series of a
 * simulation runs was written in the set of detailed binary streams.  This
 * means field {@code level} must be {@code detailedBin}.
  
 */
:- pred runTool(data.config.config, tools.'PCVNetwork'.parameters, string, io.state, io.state).
:- mode runTool(in, in, out, di, uo) is det.

:- implementation.

:- import_module data.util.
:- import_module game, printable.
:- import_module ebea, ebea.player, ebea.player.selection, ebea.player.selection.pcv, ebea.population, ebea.population.parameters, ebea.streams, ebea.streams.birth, ebea.streams.phenotype.
:- import_module parseable, parseable.iou.
:- import_module util.
:- import_module array, exception, int, maybe, set, solutions, string.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

default_parameters = Result :-
	Result^printSelectionGenes = default_printSelectionGenes,
	Result^printStrategyGenes = default_printStrategyGenes,
	Result^printProbability = default_printProbability,
	Result^printCombinationIndex = default_printCombinationIndex,
	Result^players = default_players,
	Result^runs = default_runs,
	Result^fileNamePrefix = default_fileNamePrefix.

dialog_parameters =
	[
	di(label("print selection genes"),    updateFieldBool(     get_printSelectionGenes,    set(set_printSelectionGenes))),
	di(label("print strategy genes"),     updateFieldBool(     get_printStrategyGenes,     set(set_printStrategyGenes))),
	di(label("print probability"),        updateFieldBool(     get_printProbability,       set(set_printProbability))),
	di(label("print combination index"),  updateFieldBool(     get_printCombinationIndex,  set(set_printCombinationIndex))),
	di(label("players to process"),       updateListFieldInt(  get_players,                set_players)),
	di(label("runs to process"),          updateListFieldInt(  get_runs,                   set_runs)),
	di(label("filename prefix"),          updateFieldString(   get_fileNamePrefix,         set(set_fileNamePrefix)))
	].

runTool(Data, Parameters, Feedback, !IO) :-
	data.util.gameConfig(Data) = gcex(Game, _, InitialPopulation),
	Level = Data^level,
	(
		Level = detailedBin,
		(
			Parameters^runs = [],
			RunIndexes = set.from_list(1..Data^numberRuns)
			;
			Parameters^runs = [_ | _],
			RunIndexes = set.intersect(set.from_list(1..Data^numberRuns), set.from_list(Parameters^runs))
		),
		set.fold2(
			createProbabilityCombinationVectorsNetworksForRun_s1(Data, Game, InitialPopulation, Parameters),
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
		Feedback = "The simulation runs did not produce player profile data"
		;
		Level = summary,
		Feedback = "The simulation runs did not produce player profile data"
	).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

/**
 * opens the streams for reading and if no error occurs, continues to predicate
 * {@code createProbabilityCombinationVectorsNetworksForRun_s2/9}
 
 */
:- pred createProbabilityCombinationVectorsNetworksForRun_s1(
	data.config.config            :: in,
	G                             :: in,
	ebea.population.parameters.parameters(C) :: in,
	tools.'PCVNetwork'.parameters :: in,
	int                           :: in,
	list(string) :: in, list(string) :: out,
	io.state     :: di, io.state     :: uo
) is det <= (
	asymmetricGame(G, C),
	parseable(C),
	printable(C)
).

createProbabilityCombinationVectorsNetworksForRun_s1(
	Data,
	Game,
	InitialPopulation,
	Parameters,
	RunIndex,
	!FeedbackAsList,
	!IO
) :-
	ebea.streams.openInputStreams(Data^level, yes(string.format("_R%d", [i(RunIndex)])), IMStreams, !IO),
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
		createProbabilityCombinationVectorsNetworksForRun_s2(
			Data,
			Game,
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

:- pred createProbabilityCombinationVectorsNetworksForRun_s2(
	data.config.config                       :: in,
	G                                        :: in,
	ebea.population.parameters.parameters(C) :: in,
	tools.'PCVNetwork'.parameters            :: in,
	ebea.streams.inStreams                   :: in(detailedBin),
	int                                      :: in,
	list(string)           :: in, list(string) :: out,
	io.state               :: di, io.state     :: uo
) is det <= (
	asymmetricGame(G, C),
	parseable(C),
	printable(C)
).

createProbabilityCombinationVectorsNetworksForRun_s2(
	Data,
	Game,
	_InitialPopulation,
	Parameters,
	Streams,
	RunIndex,
	!FeedbackAsList,
	!IO
) :-
	ebea.streams.birth.read(Streams^bisBirth, -1, RIIterationBirthRecords, no, InitialBirthAdvancedResult, parseable.iou.cacheInit, InitialBirthCache, !IO),
	(	%
		RIIterationBirthRecords = delayed,
		list.cons(
			"Not implemented creating alive players from a simulation without initial population stored in birth files",
			!FeedbackAsList)
		;
		RIIterationBirthRecords = ok(ok(IterationBirthRecords)),
		IterationBirthRecords = ibr(_, ListBirthRecords),
		createProbabilityCombinationVectorsNetworksForRunIteration(
			Data,
			Game,
%			InitialPopulation,
			Parameters,
			Streams,
			RunIndex,
			0,
			ListBirthRecords, _,
			InitialBirthAdvancedResult, FinalBirthAdvancedResult,
			InitialBirthCache,          FinalBirthCache,
			parseable.iou.cacheInit,    FinalPhenotypeCache,
			!FeedbackAsList,
			!IO
		),
		io.print(FinalBirthAdvancedResult, !IO),
		io.nl(!IO),
		io.print(FinalBirthCache, !IO),
		io.nl(!IO),
		io.print(FinalPhenotypeCache, !IO),
		io.nl(!IO)
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


% /**
%  * startProbabilityCombinationVectorsNetworksForRun(Data, Parameters, Streams, RunIndex, ListBirthRecords, !BirthAdvancedResult, !BirthCache, !FeedbackAsList, !IO)

%  * Start creating the probability and combination vectors networks for the given run.
  
%  */
% :- pred startProbabilityCombinationVectorsNetworksForRun(
% 	data.config.config              :: in,
% 	parameters                      :: in,
% 	ebea.streams.inStreams          :: in,
% 	int                             :: in,
% 	list(playerBirthRecord(C))      :: in,
% 	maybe(iterationBirthRecords(C)) :: in, maybe(iterationBirthRecords(C)) :: out,
% 	parseable.iou.cache             :: in, parseable.iou.cache             :: out,
% 	list(string)                    :: in, list(string)                    :: out,
% 	io.state                        :: di, io.state                        :: uo
% ) is det.

% startProbabilityCombinationVectorsNetworksForRun(
% 	Data,
% 	Parameters,
% 	Streams,
% 	RunIndex,
% 	ListBirthRecords,
% 	!BirthAdvancedResult,
% 	!BirthCache,
% 	!FeedbackAsList,
% 	!IO
% ) :-
% 	(if
% 		ListBirthRecords = []
% 	then
% 		% MapID =
% 		% (func(BR) = Re :-
% 		% 	Re = BR^id
% 		% ),
% 		% InitialListAlivePlayers = list.map(MapID, ListBirthRecords),
% 		createProbabilityCombinationVectorsNetworksForRunIteration(
% 			Data,
% 			Parameters,
% 			Streams,
% 			RunIndex,
% 			0,
% 			ListBirthRecords, _,
% 			!BirthAdvancedResult,
% 			!BirthCache,
% 			parseable.iou.cacheInit, FinalPhenotypeCache,
% 			!FeedbackAsList,
% 			!IO
% 		),
% 		io.print(FinalPhenotypeCache, !IO),
% 		io.nl(!IO)
% 	else
% 		list.cons(
% 			"Not implemented creating alive players from a simulation without initial population stored in birth files",
% 			!FeedbackAsList)
% 	).


/**
 *  In each iteration we create a dot file for each player specified in
 *  {@code Parameters^players}.  We read the phenotype file to extract the
 *  probability and combination vectors.  The file is only created if the
 *  player has a <i>pcv</i> selection genes.
  
 */
:- pred createProbabilityCombinationVectorsNetworksForRunIteration(
	data.config.config              :: in,
	G                               :: in,
	tools.'PCVNetwork'.parameters   :: in,
	ebea.streams.inStreams          :: in(detailedBin),
	int                             :: in,
	int                             :: in,
	list(playerBirthRecord(C))      :: in, list(playerBirthRecord(C))      :: out,
	maybe(iterationBirthRecords(C)) :: in, maybe(iterationBirthRecords(C)) :: out,
	parseable.iou.cache             :: in, parseable.iou.cache             :: out,
	parseable.iou.cache             :: in, parseable.iou.cache             :: out,
	list(string)                    :: in, list(string)                    :: out,
	io.state                        :: di, io.state                        :: uo
) is det <= (
	asymmetricGame(G, C),
	parseable(C),
	printable(C)
).

createProbabilityCombinationVectorsNetworksForRunIteration(
	Data,
	Game,
	Parameters,
	Streams,
	RunIndex,
	IterationIndex,
	!ListBirthRecords,
	!BirthAdvancedResult,
	!BirthCache,
	!PhenotypeCache,
	!FeedbackAsList,
	!IO
) :-
	
	io.format("\rIteration %d", [i(IterationIndex)], !IO),
	io.flush_output(io.stdout_stream, !IO),
	ebea.streams.phenotype.read(Streams^bisPhenotype, RIIterationPhenotypicRecords, !PhenotypeCache, !IO),
	(	% switch RIIterationPhenotypicRecords,
		RIIterationPhenotypicRecords = ok(ok(IterationPhenotypicRecords)),
		ebea.streams.birth.read(Streams^bisBirth, IterationIndex, RIIterationBirthRecords, !BirthAdvancedResult, !BirthCache, !IO),
		(	% switch RIIterationBirthRecords
			(
				RIIterationBirthRecords = delayed
				;
				RIIterationBirthRecords = ok(ok(IterationBirthRecords)),
				IterationBirthRecords = ibr(_, NewBirthRecords),
				list.append(NewBirthRecords, !ListBirthRecords)
				;
				RIIterationBirthRecords = ok(eof)
			),
			IterationPhenotypicRecords = ipr(_, ListPlayerPhenotypicRecord),
			list.foldl2(
				createProbabilityCombinationVectorsNetworkForRunIterationPlayer(
					Parameters,
					RunIndex,
					IterationIndex,
					!.ListBirthRecords,
					ListPlayerPhenotypicRecord
				),
				Parameters^players,
				!FeedbackAsList,
				!IO
			),
			createProbabilityCombinationVectorsNetworksForRunIteration(
				Data,
				Game,
				Parameters,
				Streams,
				RunIndex,
				IterationIndex + 1,
				!ListBirthRecords,
				!BirthAdvancedResult,
				!BirthCache,
				!PhenotypeCache,
				!FeedbackAsList,
				!IO)
		;	
			RIIterationBirthRecords = ok(error(Error)),
			list.cons(string.format("io error while reading birth file: %s", [s(io.error_message(Error))]), !FeedbackAsList)
			;
			RIIterationBirthRecords = parseError,
			list.cons("parse error while reading birth file", !FeedbackAsList)
		)
	;
		RIIterationPhenotypicRecords = ok(eof),
		% create movies
		Pred =
		(	pred(PlayerIndex::in, IOdi::di, IOuo::uo) is det :-
			CmdAvi = string.format("avconv -y -i \"%s_R%d_P%d_T%%d.png\" -r 10 \"%s_R%d_P%d.avi\"",
				[s(Parameters^fileNamePrefix), i(RunIndex), i(PlayerIndex),
				 s(Parameters^fileNamePrefix), i(RunIndex), i(PlayerIndex)]),
			util.callSystem(CmdAvi, IOdi, IOuo)
		),
		list.foldl(Pred, Parameters^players, !IO)
	;
		RIIterationPhenotypicRecords = ok(error(Error)),
		list.cons(io.error_message(Error), !FeedbackAsList)
	;
		RIIterationPhenotypicRecords = parseError,
		list.cons("parse error while reading phenotype file", !FeedbackAsList)
	)
	.


/**
 * Write a graphviz file with the probability and combination vectors for
 * the given player.  We only create a file, if the player is present in
 * the phenotypic records.
  
 */

:- pred createProbabilityCombinationVectorsNetworkForRunIterationPlayer(
	tools.'PCVNetwork'.parameters :: in,
	int                           :: in,
	int                           :: in,
	list(playerBirthRecord(C))    :: in,
	list(playerPhenotypicRecord)  :: in,
	int                           :: in,
	list(string)                  :: in, list(string) :: out,
	io.state                      :: di, io.state     :: uo
) is det <= (
	printable(C)
).

createProbabilityCombinationVectorsNetworkForRunIterationPlayer(
	Parameters,
	RunIndex,
	IterationIndex,
	ListBirthRecords,
	ListPlayerPhenotypicRecord,
	PlayerID,
	!FeedbackAsList,
	!IO
) :-
	(if
		ebea.streams.phenotype.search(PlayerID, ListPlayerPhenotypicRecord, ThePlayerPhenotypicRecord),
		ThePlayerPhenotypicRecord^selection = partnerSelection(PCV)
	then
		FileName = string.format("%s_R%d_P%d_T%d.dot",
			[s(Parameters^fileNamePrefix),
			 i(RunIndex),
			 i(PlayerID),
			 i(IterationIndex)]),
		io.open_output(FileName, IStream, !IO),
		(	% switch IStream
			IStream = ok(DotStream),
			PartnersID = solutions.solutions(playerIDNet(PCV)),
			/* write dot file */
			tools.printGraphVizHeader(DotStream, !IO),
			list.foldl(
				printGraphVizNode(DotStream, Parameters^printSelectionGenes, Parameters^printStrategyGenes, ListBirthRecords),
				[PlayerID | PartnersID],
				!IO),
			int.fold_up(printEdges(DotStream, Parameters, PlayerID, PCV), 0, array.size(PCV) - 1, !IO),
			printEnd(DotStream, !IO),
			io.close_output(DotStream, !IO),
			/* create PNG graph */
			CmdPng = string.format("dot -Tpng -o\"%s_R%d_P%d_T%d.png\" \"%s\"",
				[s(Parameters^fileNamePrefix),
				 i(RunIndex),
				 i(PlayerID),
				 i(IterationIndex),
				 s(FileName)]),
			util.callSystem(CmdPng, !IO)
		;
			IStream = error(Error),
			list.cons(string.format("IO error opening `%s` file: %s", [s(FileName), s(io.error_message(Error))]), !FeedbackAsList)
		)
	else
		true
	).




/**
 * playerIDNet(PlayerProfileNetwork, ID)

 * Unify {@code ID} with a player ID taken from a combination vector.  It
 *  is a vector that is part of a probability and combination vectors
 *  stored in {@code phenotype} stream.
 */

:- pred playerIDNet(probabilityCombinationVector, int).
:- mode playerIDNet(in, out) is nondet.

playerIDNet(PCV, ID) :-
	array.member(PCV, Slot),
	list.member(ID, Slot^combination).



/**
 * Print an edge of the probability and combination vector graph.  The edge
 * has a label representing the probability and the combination index.
 */

:- pred printEdges(
	io.output_stream              :: in,
	tools.'PCVNetwork'.parameters :: in,
	int                           :: in,
	probabilityCombinationVector  :: in,
	int                           :: in,
	io.state                      :: di, io.state     :: uo
) is det.

printEdges(DotStream, Parameters, PlayerID, PCV, SlotIndex, !IO) :-
	probability(PCV, SlotIndex, Probability),
	list.foldl(
		printAnEdge(DotStream, Parameters, PlayerID, Probability, SlotIndex),
		array.lookup(PCV, SlotIndex)^combination,
		!IO
	).

:- pred printAnEdge(
	io.output_stream              :: in,
	tools.'PCVNetwork'.parameters :: in,
	int                           :: in,
	int                           :: in,
	int                           :: in,
	int                           :: in,
	io.state                      :: di, io.state     :: uo
) is det.

printAnEdge(Stream, _Parameters, PlayerID, Probability, SlotIndex, PartnerID, !IO) :-
	io.print(Stream, "P", !IO),
	io.print(Stream, PlayerID, !IO),
	io.print(Stream, "--P", !IO),
	io.print(Stream, PartnerID, !IO),
	io.print(Stream, "[label=\"", !IO),
	io.print(Stream, SlotIndex, !IO),
	io.print(Stream, ":", !IO),
	io.print(Stream, Probability, !IO),
	io.print(Stream, "\"]\n", !IO)
	.

:- pred printEnd(io.output_stream, io.state, io.state).
:- mode printEnd(in, di, uo) is det.

printEnd(Stream, !IO) :-
	io.print(Stream, "}\n", !IO).




:- func default_printSelectionGenes = bool.

default_printSelectionGenes = no.

:- func default_printStrategyGenes = bool.

default_printStrategyGenes = yes.

:- func default_printProbability = bool.

default_printProbability = yes.

:- func default_printCombinationIndex = bool.

default_printCombinationIndex = yes.

:- func default_players = list(int).

default_players = [0].

:- func default_runs = list(int).

default_runs = [1,2,3,4,5,6,8,8,10].

:- func default_fileNamePrefix = string.

default_fileNamePrefix = "probability-combination-vectors".

:- func get_printSelectionGenes(tools.'PCVNetwork'.parameters) = bool.

get_printSelectionGenes(P) = P^printSelectionGenes.


:- func set_printSelectionGenes(tools.'PCVNetwork'.parameters, bool) = tools.'PCVNetwork'.parameters.

set_printSelectionGenes(P, V) = 'printSelectionGenes :='(P, V).



:- func get_printStrategyGenes(tools.'PCVNetwork'.parameters) = bool.

get_printStrategyGenes(P) = P^printStrategyGenes.


:- func set_printStrategyGenes(tools.'PCVNetwork'.parameters, bool) = tools.'PCVNetwork'.parameters.

set_printStrategyGenes(P, V) = 'printStrategyGenes :='(P, V).



:- func get_printProbability(tools.'PCVNetwork'.parameters) = bool.

get_printProbability(P) = P^printProbability.


:- func set_printProbability(tools.'PCVNetwork'.parameters, bool) = tools.'PCVNetwork'.parameters.

set_printProbability(P, V) = 'printProbability :='(P, V).



:- func get_printCombinationIndex(tools.'PCVNetwork'.parameters) = bool.

get_printCombinationIndex(P) = P^printCombinationIndex.


:- func set_printCombinationIndex(tools.'PCVNetwork'.parameters, bool) = tools.'PCVNetwork'.parameters.

set_printCombinationIndex(P, V) = 'printCombinationIndex :='(P, V).



:- func get_players(tools.'PCVNetwork'.parameters) = list(int).

get_players(P) = P^players.


:- func set_players(tools.'PCVNetwork'.parameters, list(int)) = userInterface.setResult(tools.'PCVNetwork'.parameters).

set_players(P, V) = Result :-
	(if
		list.append(_, [H | T], V),
		list.append(_, [H | _], T)
	then
		Result = error("Duplicate player identification")
	else if
		list.member(ID, V),
		ID < 0
	then
		Result = error("Player's identification are not negative")
	else
		Result = ok('players :='(P, V))
	).

:- func get_runs(tools.'PCVNetwork'.parameters) = list(int).

get_runs(P) = P^runs.


:- func set_runs(tools.'PCVNetwork'.parameters, list(int)) = userInterface.setResult(tools.'PCVNetwork'.parameters).

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



:- func get_fileNamePrefix(tools.'PCVNetwork'.parameters) = string.

get_fileNamePrefix(P) = P^fileNamePrefix.


:- func set_fileNamePrefix(tools.'PCVNetwork'.parameters, string) = tools.'PCVNetwork'.parameters.

set_fileNamePrefix(P, V) = 'fileNamePrefix :='(P, V).


:- end_module tools.'PCVNetwork'.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
