/**
 * 

 * @author Pedro Mariano
 * @version 1.0 2014/04/ 6
 */
:- module tools.processPlayerProfile.

:- interface.

:- import_module data, data.config.
:- import_module userInterface.
:- import_module io, list.

/**
 * The parameters that control the behaviour of this tool.
 */
:- type parameters.

:- pred run(data.config.config, tools.processPlayerProfile.parameters, string, string, io.state, io.state).
:- mode run(in, in, in, out, di, uo) is det.

/**
 * Return a default value of {@code parameters}.
 */
:- func default_parameters = tools.processPlayerProfile.parameters.

:- func dialog_parameters = list(dialogItem(tools.processPlayerProfile.parameters)).

:- implementation.

:- import_module ebea, ebea.player, ebea.player.chromosome, ebea.population, ebea.population.players, ebea.streams, ebea.streams.birth, ebea.streams.death, ebea.streams.playerProfile.
:- import_module data.util.
:- import_module game.
:- import_module parseable, parseable.iou, printable.
:- import_module array, bool, exception, float, int, map, math, maybe, set_bbbtree, string.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

:- type parameters --->
	parameters(
		printPlayerID               :: bool ,
		printSiteIndex              :: bool ,
		printSelectionGenes         :: bool ,
		printStrategyGenes          :: bool ,
		onlyPrintSelectingEdge      :: bool ,
		slidingWindowSize           :: int ,
		createPng                   :: bool ,
		createMovie                 :: bool ,
		createAverageNumberPartners :: bool ,
		maxIteration                :: int ,
		printRawValues              :: bool ,
		minPercentageValue          :: int ,
		fileNamePrefix              :: string
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

/**
 * Player's partners is represented as map that associates player's id to
 * partners' id.
  
 */
:- type playerPartners == map(key, set_bbbtree(key)).

/**
 * This map associates strategy profiles to their number of occurrences.

*/
:- type strategyPlayMatrix(S) == map(list(S), int).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

run(Config, Parameters, Directory, Feedback, !IO) :-
	Level = Config^level,
	(	%
		Level = detailedBin,
		int.fold_up2(processRun_s1(Config, Parameters, Directory), 1, Config^numberRuns, [], FeedbackAsList, !IO),
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
		Feedback = "The simulation runs did not produce player profile data"
	;	
		Level = summary,
		Feedback = "The simulation runs did not produce player profile data"
	).

default_parameters = parameters(yes, yes, no, yes, yes, 10, no, no, yes, 2000000,
		yes,
0,
										  "player-profiles").

% default_parameters = parameters(yes, yes, no, yes, yes, 10, no, yes, yes, 200,
% 		yes,
% 0,
% 										  "player-profiles").


dialog_parameters =
	[
	di(label("print player id"),            updateFieldBool( printPlayerID,           set('printPlayerID :='))),
	di(label("print site index"),           updateFieldBool( printSiteIndex,          set('printSiteIndex :='))),
	di(label("print selection genes"),      updateFieldBool( printSelectionGenes,     set('printSelectionGenes :='))),
	di(label("print strategy genes"),       updateFieldBool( printStrategyGenes,      set('printStrategyGenes :='))),
	di(label("only print selecting edge"),  updateFieldBool( onlyPrintSelectingEdge,  set('onlyPrintSelectingEdge :='))),
	di(label("sliding window size"),        updateFieldInt(  slidingWindowSize,       checkInt( "sliding window size",  bounded(0, yes), unbound, 'slidingWindowSize :='))),
	di(label("create png"),                 updateFieldBool( createPng,               set('createPng :='))),
	di(label("create movie"),               updateFieldBool( createMovie,             set('createMovie :='))),
	di(label("create average number partners"), updateFieldBool( createAverageNumberPartners, set('createAverageNumberPartners :='))),
	di(label("maximum iteration"),          updateFieldInt(  maxIteration,            checkInt( "maximum iteration",    bounded(10, yes), unbound, 'maxIteration :='))),
	di(label("print raw values"),           updateFieldBool( printRawValues,          set('printRawValues :='))),
	di(label("minimum value (% window)"),   updateFieldInt(  minPercentageValue,      checkInt( "percentage",    bounded(0, yes), bounded(100, yes), 'minPercentageValue :='))),
	di(label("filename prefix"),            updateFieldString(   fileNamePrefix,          set('fileNamePrefix :=')))
	].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- pred processRun_s1(
	data.config.config                    :: in,
	tools.processPlayerProfile.parameters :: in,
	string                                :: in,
	int                                   :: in,
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
		ebea.streams.playerProfile.read(Streams, Game, IRAllPlayerProfiles, !IO),
		data.util.gameConfig(Config) = gcex(Game, _, _),
		processRun_s2(Config, Game, Parameters, Directory, RunIndex, IRAllBirths, IRAllDeaths, IRAllPlayerProfiles, !FeedbackAsList, !IO)
	else
		IMStreams = ok(_)
		;
		IMStreams = error(ErrorMsg),
		list.cons(ErrorMsg, !FeedbackAsList)
	).

:- pred processRun_s2(
	data.config.config                    :: in,
	G                                     :: in,
	tools.processPlayerProfile.parameters :: in,
	string                                :: in,
	int                                   :: in,
	ioResult(list(iterationBirthRecords(C)))      :: in,
	ioResult(list(iterationDeathRecords))         :: in,
	ioResult(list(iterationPlayerProfileRecords)) :: in,
	list(string) :: in, list(string) :: out,
	io.state     :: di, io.state     :: uo
) is det
	<= (asymmetricGame(G, C), parseable(C), printable(C))
	.

processRun_s2(
	Config,
	_Game,
	Parameters,
	Directory,
	RunIndex,
	IRAllBirths,
	IRAllDeaths,
	IRAllPlayerProfiles,
	!FeedbackAsList,
	!IO)
:-
	(if
		IRAllBirths = ok(AllBirths),
		IRAllDeaths = ok(AllDeaths),
		IRAllPlayerProfiles = ok(AllPlayerProfiles)
	then
		IterationWidth = float.ceiling_to_int(math.unchecked_log10(float(Config^numberIterations + 1))),
		openAverageNumberPartnersStream(
			Parameters,
			Directory,
			RunIndex,
			MAverageNumberPartnersStream,
			!FeedbackAsList,
			!IO
		),
		ArrayPlayerProfiles = array.from_list(AllPlayerProfiles),
		intFoldUp3(
			processRunIteration(
				IterationWidth,
				Parameters,
				MAverageNumberPartnersStream,
				Directory,
				RunIndex,
				AllBirths,
				AllDeaths,
				ArrayPlayerProfiles
			),
			0,
			array.size(ArrayPlayerProfiles) - 1,
			map.init, StrategyPlayMatrix,
			!FeedbackAsList,
			!IO
		),
		writeStrategyPlayMatrix(Parameters, Directory, RunIndex, StrategyPlayMatrix, !FeedbackAsList, !IO),
		% processRunIteration(
		% 	IterationWidth,
		% 	Parameters,
		% 	MAverageNumberPartnersStream,
		% 	Directory,
		% 	RunIndex,
		% 	0,
		% 	AllBirths,
		% 	AllDeaths,
		% 	AllPlayerProfiles,
		%	map.init, StrategyPlayMatrix,
		% 	!FeedbackAsList,
		% 	!IO),
		(	%
			MAverageNumberPartnersStream = yes(ANPS),
			io.close_output(ANPS, !IO)
		;	
			MAverageNumberPartnersStream = no
		),
		io.print("\r                      \n", !IO),
		table_reset_for_memo_mapPlayerIDStrategy_2(!IO),
		ebea.streams.birth.table_reset_for_search_3(!IO)
	else
		list.cons("error reading streams birth, death or playerProfile", !FeedbackAsList)
	)
	.
:- pred processRunIteration(
	int                                   :: in,
	tools.processPlayerProfile.parameters :: in,
	maybe(io.output_stream)               :: in,
	string                                :: in,
	int                                   :: in,
	list(iterationBirthRecords(C))        :: in,
	list(iterationDeathRecords)           :: in,
	array(iterationPlayerProfileRecords)  :: in,
	int                                   :: in,
	strategyPlayMatrix(C) :: in, strategyPlayMatrix(C) :: out,
	list(string)          :: in, list(string) :: out,
	io.state              :: di, io.state     :: uo
) is det
	<= (printable(C))
.

processRunIteration(
	_IterationWidth,
	Parameters,
	MAverageNumberPartnersStream,
	_Directory,
	_RunIndex,
	AllBirths,
	_AllDeaths,
	AllPlayerProfiles,
	IterationIndex,
	!StrategyPlayMatrix,
	!FeedbackAsList,
	!IO
) :-
	(if
		Parameters^createMovie = yes ;
		Parameters^createPng = yes
	then
		io.format("\rIteration %d", [i(IterationIndex)], !IO),
		io.flush_output(io.stdout_stream, !IO)
	else
		true
	),
	array.lookup(AllPlayerProfiles, IterationIndex) = PlayerProfileRecords,
	list.foldl(incrementCount(AllBirths), PlayerProfileRecords^profiles, !StrategyPlayMatrix),
	(if
		MAverageNumberPartnersStream = yes(AverageNumberPartnersStream)
	then
		int.fold_up(
			updatePartners(
				AllPlayerProfiles
			),
			int.max(0, IterationIndex - Parameters^slidingWindowSize),
			IterationIndex,
			map.init, PlayerPartners
		),
		map.foldl2(updateAverageNumberPartners, PlayerPartners, 0, X, 0, Y),
		io.print(AverageNumberPartnersStream, IterationIndex, !IO),
		io.print(AverageNumberPartnersStream, '\t', !IO),
		(if
			Y = 0
		then
			io.print(AverageNumberPartnersStream, "0/0", !IO)
		else
			io.print(AverageNumberPartnersStream, float(X) / float(Y), !IO)
		),
		io.nl(AverageNumberPartnersStream, !IO)
	else
		true
	)
	% (if
	% 	IterationIndex < array.size(AllPlayerProfiles)
	% then
	% 	processRunIteration(
	% 		IterationWidth,
	% 		Parameters,
	% 		MAverageNumberPartnersStream,
	% 		Directory,
	% 		RunIndex,
	% 		IterationIndex,
	% 		AllBirths,
	% 		AllDeaths,
	% 		AllPlayerProfiles,
	% 		!FeedbackAsList,
	% 		!IO)
	% else
	% 	true
	% )
	.



/**
 * writeStrategyPlayMatrix(Parameters, Directory, RunIndex, StrategyPlayMatrix, !FeedbackAsList, !IO)

 * Write the strategy play matrix to a text file.
  
 */
:- pred writeStrategyPlayMatrix(
	 tools.processPlayerProfile.parameters :: in,
	 string                                :: in,
	 int                                   :: in,
	 strategyPlayMatrix(S)                 :: in,
	 list(string) :: in, list(string) :: out,
	 io.state     :: di, io.state     :: uo
	 )
is det
 <= printable(S).

writeStrategyPlayMatrix(Parameters, Directory, RunIndex, StrategyPlayMatrix, !FeedbackAsList, !IO) :-
	FileName = string.format(
		"%s%s_strategy-play-matrix_R%d.txt",
		[s(Directory), s(Parameters^fileNamePrefix), i(RunIndex)]),
	io.open_output(FileName, IStream, !IO),
	(	% switch IStream
		IStream = ok(Stream),
		PredPrint =
		(	pred(Key::in, Value::in, IOdi::di, IOuo::uo) is det :-
			io.print(Stream, Value, IOdi, IO1),
			list.foldl(printable.spacePrint(Stream), Key, IO1, IO2),
			io.nl(Stream, IO2, IOuo)
		),
		map.foldl(PredPrint, StrategyPlayMatrix, !IO),
		io.close_output(Stream, !IO)
	;
		IStream = error(Error),
		list.cons(
			string.format("IO error opening `%s` file: %s", [s(FileName), s(io.error_message(Error))]),
			!FeedbackAsList)
	).




/**
 * incrementCount(ListBirthRecords, PlayerProfile, !StrategyPlayMatrix)
  
 * Increments by one the number of times the given player profile has
 * occurred.

 * @param ListBirthRecords The birth records used to map the player
 * profile, which contains player's ids to player's strategies.
  
 * @param PlayerProfile The player profile to be processed.

 * @param !StrategyPlayMatrix 
  
  * 
 */
:- pred incrementCount(
	list(iterationBirthRecords(C)) :: in,
	list(key)                      :: in,
	strategyPlayMatrix(C) :: in,  strategyPlayMatrix(C) :: out
) is det.

incrementCount(AllBirths, PlayerProfile, !StrategyPlayMatrix) :-
	list.map(memo_mapPlayerIDStrategy(AllBirths), PlayerProfile) = ListStrategies,
	(if
		map.search(!.StrategyPlayMatrix, ListStrategies, Count)
	then
		map.det_update(ListStrategies, Count + 1, !StrategyPlayMatrix)
	else
		map.det_insert(ListStrategies, 1, !StrategyPlayMatrix)
	)
	.

%:- pragma minimal_model(memo_mapPlayerIDStrategy/2, [fast_loose, allow_reset]).
:- pragma memo(memo_mapPlayerIDStrategy/2, [fast_loose, allow_reset]).

:- func memo_mapPlayerIDStrategy(list(iterationBirthRecords(C)), key) = C.

memo_mapPlayerIDStrategy(AllBirths, ID) = work_mapPlayerIDStrategy(AllBirths, ID).

:- func work_mapPlayerIDStrategy(list(iterationBirthRecords(C)), key) = C.

work_mapPlayerIDStrategy([], _) = throw("mapPlayerIDStrategy/2: Never reached").

work_mapPlayerIDStrategy([ibr(_, ListBirthRecords) | Rest], ID) =
	(if
		ebea.streams.birth.work_search(ID, ListBirthRecords, S)
	then
		S^chromosome^strategyGenes
	else
		work_mapPlayerIDStrategy(Rest, ID)
	).




/**
 * Given a key value pair from the player profile network update the
 * selected partners of a player.
  
 */
:- pred updatePartners(array(iterationPlayerProfileRecords), int, playerPartners, playerPartners).
:- mode updatePartners(in, in, in, out) is det.

updatePartners(AllPlayerProfiles, IterationIndex, !PlayerPartners) :-
	array.lookup(AllPlayerProfiles, IterationIndex) = PlayerProfileRecords,
	(if
		PlayerProfileRecords^iteration \= IterationIndex
	then
		throw("updatePartners/4: problem in player profile stream")
	else
		true
	),
	PredUpdate =
	(pred(StrategyProfile::in, !.PlayerPartners::in, !:PlayerPartners::out) is det :- 
		StrategyProfile = [Player | Partners],
		(if
			map.search(!.PlayerPartners, Player, OldPartners)
		then
			map.det_update(Player, set_bbbtree.union(OldPartners, set_bbbtree.from_list(Partners)), !PlayerPartners)
		else
			map.det_insert(Player, set_bbbtree.from_list(Partners), !PlayerPartners)
		)
	;	
		StrategyProfile = [],
		throw("updatePartners/4: never reached?")
	),
	list.foldl(PredUpdate, PlayerProfileRecords^profiles, !PlayerPartners)
	.

/**
 * Update the average number of partners of players using a key value pair
 * from the selected partners of a player.
 */

:- pred updateAverageNumberPartners(key, set_bbbtree.set_bbbtree(key), int, int, int, int).
:- mode updateAverageNumberPartners(in, in, in, out, in, out) is det.

updateAverageNumberPartners(
	_Player,
	Partners,
	TotalPartners, TotalPartners + set_bbbtree.count(Partners),
	TotalPlayers, TotalPlayers + 1
).


/**
 * Opens a stream to write the average number of partners of a player if
 * the corresponding parameter is true.
 */

:- pred openAverageNumberPartnersStream(
	tools.processPlayerProfile.parameters :: in,
	string                                :: in,
	int                                   :: in,
	maybe(io.output_stream) :: out,
	list(string) :: in, list(string) :: out,
	io.state     :: di, io.state     :: uo
) is det.

openAverageNumberPartnersStream(
	Parameters,
	Directory,
	RunIndex,
	MAverageNumberPartnersStream,
	!FeedbackAsList,
	!IO
) :-
	(if
		Parameters^createAverageNumberPartners = yes
	then
		FileName = string.format("%s%s_average-number-partners_R%d.txt", [s(Directory), s(Parameters^fileNamePrefix), i(RunIndex)]),
		io.open_output(FileName, IAverageNumberPartnersStream, !IO),
		(	%
			IAverageNumberPartnersStream = ok(AverageNumberPartnersStream),
			MAverageNumberPartnersStream = yes(AverageNumberPartnersStream)
		;	
			IAverageNumberPartnersStream = error(Error2),
			list.cons(string.format("IO error opening `%s` file: %s", [s(FileName), s(io.error_message(Error2))]), !FeedbackAsList),
			MAverageNumberPartnersStream = no
		)
	else
		MAverageNumberPartnersStream = no
	).


:- func printPlayerID(tools.processPlayerProfile.parameters) = bool.
:- func 'printPlayerID :='(tools.processPlayerProfile.parameters, bool) = tools.processPlayerProfile.parameters.

:- func printSiteIndex(tools.processPlayerProfile.parameters) = bool.
:- func 'printSiteIndex :='(tools.processPlayerProfile.parameters, bool) = tools.processPlayerProfile.parameters.

:- func printSelectionGenes(tools.processPlayerProfile.parameters) = bool.
:- func 'printSelectionGenes :='(tools.processPlayerProfile.parameters, bool) = tools.processPlayerProfile.parameters.

:- func printStrategyGenes(tools.processPlayerProfile.parameters) = bool.
:- func 'printStrategyGenes :='(tools.processPlayerProfile.parameters, bool) = tools.processPlayerProfile.parameters.

:- func onlyPrintSelectingEdge(tools.processPlayerProfile.parameters) = bool.
:- func 'onlyPrintSelectingEdge :='(tools.processPlayerProfile.parameters, bool) = tools.processPlayerProfile.parameters.

:- func slidingWindowSize(tools.processPlayerProfile.parameters) = int.
:- func 'slidingWindowSize :='(tools.processPlayerProfile.parameters, int) = tools.processPlayerProfile.parameters.

:- func createPng(tools.processPlayerProfile.parameters) = bool.
:- func 'createPng :='(tools.processPlayerProfile.parameters, bool) = tools.processPlayerProfile.parameters.

:- func createMovie(tools.processPlayerProfile.parameters) = bool.
:- func 'createMovie :='(tools.processPlayerProfile.parameters, bool) = tools.processPlayerProfile.parameters.

:- func createAverageNumberPartners(tools.processPlayerProfile.parameters) = bool.
:- func 'createAverageNumberPartners :='(tools.processPlayerProfile.parameters, bool) = tools.processPlayerProfile.parameters.

:- func maxIteration(tools.processPlayerProfile.parameters) = int.
:- func 'maxIteration :='(tools.processPlayerProfile.parameters, int) = tools.processPlayerProfile.parameters.

:- func fileNamePrefix(tools.processPlayerProfile.parameters) = string.
:- func 'fileNamePrefix :='(tools.processPlayerProfile.parameters, string) = tools.processPlayerProfile.parameters.

:- func printRawValues(tools.processPlayerProfile.parameters) = bool.
:- func 'printRawValues :='(tools.processPlayerProfile.parameters, bool) = tools.processPlayerProfile.parameters.

:- func minPercentageValue(tools.processPlayerProfile.parameters) = int.
:- func 'minPercentageValue :='(tools.processPlayerProfile.parameters, int) = tools.processPlayerProfile.parameters.



:- pred intFoldUp3(pred(int, A, A, B, B, C, C), int, int, A, A, B, B, C, C).
:- mode intFoldUp3(in(pred(in, in, out, in, out, di, uo) is det), in, in, in, out, in, out, di, uo) is det.

intFoldUp3(Pred, Min, Max, !A, !B, !C) :-
	(if
		Min =< Max
	then
		Pred(Min, !A, !B, !C),
		intFoldUp3(Pred, Min + 1, Max, !A, !B, !C)
	else
		true
	).



:- end_module tools.processPlayerProfile.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
