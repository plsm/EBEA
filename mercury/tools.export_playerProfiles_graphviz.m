/**

 * This tool produces for each simulation run a movie representing the
 * evolution of the player profile graph.  A node represents a player and a
 * directed edge represents a player selecting a partner.  Edges can be
 * labelled with a number representing the number of times a player has
 * selected another.  The number may be absolute or a percentage.  Each
 * movie frame represents a sequence of EBEA iterations.  The maximum
 * number of movie frames is the simulation run.  To produce a movie frame
 * we process its corresponding iteration and up to a certain number of
 * past iterations.  For each player we calculate how many times it has
 * selected some partner.  This value is used to draw an edge between two
 * players.


 * <ul>

 * <li>Average number of partners per iteration step computed from a
 * sliding window iteration data.

 * </li>
 * </ul>

  janela mesmo peso todas as interacções

   janela com peso atenuado com o passado.

  * só fazer a janela para o passado.

  * calcular o grau de conectividade de um nó

  * calcular a frequEncia de ligações entre pares de estratégias.


 * @author Pedro Mariano
 * @version 1.0 2014/03/ 7
 */
:- module tools.export_playerProfiles_graphviz.

:- interface.

:- import_module data, data.config.
:- import_module userInterface.
:- import_module io, list.

/**
 * The parameters that control the behaviour of this tool.
 */
:- type parameters.

:- pred createPlayerProfilesNetworks(data.config.config, parameters, string, string, io.state, io.state).
:- mode createPlayerProfilesNetworks(in, in, in, out, di, uo) is det.

/**
 * Return a default value of {@code parameters}.
 */
:- func default_parameters = parameters.

:- func dialog_parameters = list(dialogItem(parameters)).

:- implementation.

:- import_module ebea.
:- import_module ebea.population, ebea.population.players.
:- import_module ebea.streams, ebea.streams.birth, ebea.streams.playerProfile.
:- import_module ebea.player, ebea.player.chromosome, ebea.player.selection, ebea.player.selection.chromosome.
:- import_module game.
:- import_module parseable, parseable.iou, printable.
:- import_module util.
:- import_module data.util.
:- import_module bool, exception, float, int, map, math, maybe, set_bbbtree, solutions, string.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types


/**
 *
 * TODO: implement createPng
 */
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
	) .

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

:- type playerProfileRecord --->
	ppr(
		iteration :: int,
		profile   :: playerProfile
	).

:- type playerProfile --->
	pp(list(int)).

:- inst detailedLevel == bound(detailedTxt ; detailedBin).

/**
 * The player profile network is represented by a map where the keys
 * represent a player profile and the value represents the iteration where
 * that player profile occurred.
 */

:- type playerProfileNetwork == map(list(key), list(int)).

/**
 * This map associates strategy profiles to their number of occurrences.

*/
:- type strategyPlayMatrix(S) == map(list(S), int).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

createPlayerProfilesNetworks(Data, Parameters, Directory, Feedback, !IO) :-
	Level = Data^level,
	(
		(
			Level = detailedTxt
			;
			Level = detailedBin
		),
		int.fold_up2(createPlayerProfilesNetworkForRun(Data, Level, Parameters, Directory), 1, Data^numberRuns, [], FeedbackAsList, !IO),
		(
			FeedbackAsList = [],
			Feedback = "ok"
			;
			FeedbackAsList = [_|_],
			Feedback = string(FeedbackAsList)
		)
		;
		Level = dynamics,
		Feedback = "The simulation runs did not produce player profile data"
		;
		Level = summary,
		Feedback = "The simulation runs did not produce player profile data"
	)
/*	(
		Data^level = detailedTxt
		;
		Data^level = detailedBin
	),
	int.fold_up2(createPlayerProfilesNetworkForRun(Data, detailedTxt, Parameters), 1, Data^numberRuns, [], FeedbackAsList, !IO),
	(
		FeedbackAsList = [],
		Feedback = "ok"
		;
		FeedbackAsList = [_|_],
		Feedback = string(FeedbackAsList)
	)
	;
	Data^level = dynamics,
	Feedback = "The simulation runs did not produce player profile data"
	;
	Data^level = summary,
	Feedback = "The simulation runs did not produce player profile data"
*/	.

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

:- pred createPlayerProfilesNetworkForRun(
	data.config.config,
	ebea.streams.level,
	tools.export_playerProfiles_graphviz.parameters,
	string,
	int,
	list(string), list(string),
	io.state, io.state).
:- mode createPlayerProfilesNetworkForRun(in, in(detailedLevel), in, in, in, in, out, di, uo) is det.

createPlayerProfilesNetworkForRun(Data, Level, Parameters, Directory, RunIndex, !FeedbackAsList, !IO) :-
	ebea.streams.openInputStreams(Directory, Level, yes(string.format("_R%d", [i(RunIndex)])), IMStreams, !IO),
	(	%
		IMStreams = ok(Streams),
		(	%
			Streams = detailedTxt(_, _, _, _),
			io.format("Run %d\n", [i(RunIndex)], !IO),
			createPlayerProfilesNetworkForIterationTxt(Data, Parameters, RunIndex, 0, Streams, no, _, !FeedbackAsList, !IO),
			ebea.streams.closeInputStreams(Streams, !IO),
			io.print("\r                      \n", !IO)
		;
			Streams = detailedBin(_, _, _, _),
			io.format("Run %d\n", [i(RunIndex)], !IO),
			ebea.streams.birth.read(Streams^bisBirth, -1, RIIterationBirthRecords, no, BirthAdvancedResult, parseable.iou.cacheInit, BirthCache, !IO),
			(	%
				RIIterationBirthRecords = delayed,
				ListBirthRecords = []
			;	
				RIIterationBirthRecords = ok(ok(IterationBirthRecords)),
				IterationBirthRecords = ibr(_, ListBirthRecords)
			;	
				RIIterationBirthRecords = ok(eof),
				ListBirthRecords = []
			;	
				RIIterationBirthRecords = ok(error(Error)),
				list.cons(string.format("io error while reading birth file: %s", [s(io.error_message(Error))]), !FeedbackAsList),
				ListBirthRecords = []
			;	
				RIIterationBirthRecords = parseError,
				list.cons("parse error while reading birth file", !FeedbackAsList),
				ListBirthRecords = []
			),
			data.util.gameConfig(Data) = gcex(Game, _, _),
			IterationWidth = float.ceiling_to_int(math.unchecked_log10(float(Data^numberIterations + 1))),
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
			),
			createPlayerProfilesNetworkForIterationBin(
				IterationWidth, Parameters, Directory, RunIndex, 0, Streams,
				MAverageNumberPartnersStream,
				Game, ListBirthRecords, _,
				map.init, _,
				map.init, _StrategyPlayMatrix,
				BirthAdvancedResult, Debug0,
				BirthCache, Debug1,
				parseable.iou.cacheInit, Debug2,
				!FeedbackAsList, !IO),
			io.print(Debug0, !IO), io.nl(!IO),
			io.print(Debug1, !IO), io.nl(!IO),
			io.print(Debug2, !IO), io.nl(!IO),
			(	%
				MAverageNumberPartnersStream = yes(ANPS),
				io.close_output(ANPS, !IO)
			;
				MAverageNumberPartnersStream = no
			),
			ebea.streams.closeInputStreams(Streams, !IO),
			io.print("\r                      \r", !IO)
		;
			(
				Streams = dynamics(_) ;
				Streams = summary(_)
			),
			throw("createPlayerProfilesNetworkForRun/8: never reached")
		)
	;	
		IMStreams = error(ErrorMsg),
		list.cons(ErrorMsg, !FeedbackAsList)
	)
	.

:- pred createPlayerProfilesNetworkForIterationTxt(data.config.config, tools.export_playerProfiles_graphviz.parameters, int, int, ebea.streams.inStreams, maybe(playerProfileRecord), maybe(playerProfileRecord), list(string), list(string), io.state, io.state).
:- mode createPlayerProfilesNetworkForIterationTxt(in, in, in, in, in(detailedTxt), in, out, in, out, di, uo) is det.

createPlayerProfilesNetworkForIterationTxt(Data, Parameters, RunIndex, IterationIndex, Streams, !MSavePlayerProfile, !FeedbackAsList, !IO) :-
	io.format("\rIteration %d", [i(IterationIndex)], !IO),
	io.flush_output(io.stdout_stream, !IO),
	readPlayerProfiles(Streams, IterationIndex, !MSavePlayerProfile, no, MPlayerProfiles, !FeedbackAsList, !IO),
	(
		MPlayerProfiles = yes(_PlayerProfiles),
		FileName = string.format("%s_R%d_T%d.dot", [s(Parameters^fileNamePrefix), i(RunIndex), i(IterationIndex)]),
		io.open_output(FileName, IStream, !IO),
		(
			IStream = ok(DotStream),
			printHeader(DotStream, !IO),
%			list.foldl(printEdge(DotStream), list.map(playerProfile, PlayerProfiles), !IO),
			printEnd(DotStream, !IO),
			io.close_output(DotStream, !IO),
			(if
				IterationIndex < Parameters^maxIteration
			then
				createPlayerProfilesNetworkForIterationTxt(Data, Parameters, RunIndex, IterationIndex + 1, Streams, !MSavePlayerProfile, !FeedbackAsList, !IO)
			else
				true
			)
			;
			IStream = error(Error),
			list.cons(string.format("IO error opening `%s` file: %s", [s(FileName), s(io.error_message(Error))]), !FeedbackAsList)
		)
		;
		MPlayerProfiles = no
	).


:- pred createPlayerProfilesNetworkForIterationBin(
	int, tools.export_playerProfiles_graphviz.parameters, string, int, int, ebea.streams.inStreams,
	maybe(io.output_stream),
	G, list(playerBirthRecord(C)), list(playerBirthRecord(C)),
	playerProfileNetwork, playerProfileNetwork,
	strategyPlayMatrix(C), strategyPlayMatrix(C),
		parseable.iou.advancedResult(iterationBirthRecords(C)), 	parseable.iou.advancedResult(iterationBirthRecords(C)),
	parseable.iou.cache, parseable.iou.cache,
	parseable.iou.cache, parseable.iou.cache,
	list(string), list(string), io.state, io.state)
	<= (asymmetricGame(G, C), parseable(C), printable(C)).
:- mode createPlayerProfilesNetworkForIterationBin(
	in, in, in, in, in, in(detailedBin),
	in,
	in, in, out,
	in, out,
	in, out,
	in, out, in, out, in, out,
	in, out, di, uo) is det.

createPlayerProfilesNetworkForIterationBin(
	IterationWidth, Parameters, Directory, RunIndex, IterationIndex, Streams,
	MAverageNumberPartnersStream,
	Game, !ListBirthRecords,
	!PlayerProfileNetwork,
	!StrategyPlayMatrix,
	!BirthAdvancedResult, !BirthCache, !PlayerProfileCache,
	!FeedbackAsList, !IO
) :-
	(if
		Parameters^createMovie = yes ;
		Parameters^createPng = yes ;
		Parameters^createAverageNumberPartners = yes
	then
		io.format("\rIteration %d", [i(IterationIndex)], !IO),
		io.flush_output(io.stdout_stream, !IO)
	else
		true
	),
	ebea.streams.playerProfile.read(Streams^bisPlayerProfile, Game, !PlayerProfileCache, MIIterationPlayerProfileRecords, !IO),
	(	% switch
		MIIterationPlayerProfileRecords = ok(ok(IterationPlayerProfileRecords)),
		IterationPlayerProfileRecords = ippr(_, PlayerProfiles),
		ebea.streams.birth.read(Streams^bisBirth, IterationIndex, RIIterationBirthRecords, !BirthAdvancedResult, !BirthCache, !IO),
		(	% switch
			(
				RIIterationBirthRecords = delayed
				;
				RIIterationBirthRecords = ok(ok(IterationBirthRecords)),
				IterationBirthRecords = ibr(_, NewBirthRecords),
				list.append(NewBirthRecords, !ListBirthRecords)
				;
				RIIterationBirthRecords = ok(eof)
			),
			% update data depending on what we have to do
			(if
				Parameters^createMovie = yes ;
				Parameters^createPng = yes ;
				Parameters^createAverageNumberPartners = yes
			then
				% update player profile network AND strategy play matrix
				map.map_values_only(removeIteration(IterationIndex - Parameters^slidingWindowSize - 1), !PlayerProfileNetwork),
				list.foldl2(processPlayerProfile(IterationIndex, !.ListBirthRecords), PlayerProfiles, !PlayerProfileNetwork, !StrategyPlayMatrix)
			else
				list.foldl(incrementCount(!.ListBirthRecords), PlayerProfiles, !StrategyPlayMatrix)
			),
			(if
				Parameters^createMovie = yes ;
				Parameters^createPng = yes
			then
				% FileName = string.format("%s%s_R%d_T%d.dot",
				% 	[s(Directory), s(Parameters^fileNamePrefix), i(RunIndex), i(IterationIndex)]),
				SimpleFileName = string.format("%s%s_R%d_T%0*d",
					[s(Directory), s(Parameters^fileNamePrefix), i(RunIndex), i(IterationWidth), i(IterationIndex)]),
				io.open_output(SimpleFileName ++ ".dot", IStream, !IO),
				(	% switch
					IStream = ok(DotStream),
					% write dot file
					printHeader(DotStream, !IO),
					PlayersID = solutions.solutions(playerIDNet(!.PlayerProfileNetwork)),
					list.foldl(printNode(DotStream, Parameters, !.ListBirthRecords), PlayersID, !IO),
					map.foldl(printEdges(DotStream, Parameters, yes), !.PlayerProfileNetwork, !IO),
					printEnd(DotStream, !IO),
					io.close_output(DotStream, !IO),
					% create png graph
					CmdPng = string.format(
						"twopi -Tpng -o\"%s.png\" \"%s.dot\"",
						[s(SimpleFileName), s(SimpleFileName)]),
					util.callSystem(CmdPng, !IO)
				;	
					IStream = error(Error),
					list.cons(string.format("IO error opening `%s.dot` file: %s", [s(SimpleFileName), s(io.error_message(Error))]), !FeedbackAsList)
				)
			else
				true
			),
			(if
				MAverageNumberPartnersStream = yes(AverageNumberPartnersStream)
			then
				map.foldl(updatePartners, !.PlayerProfileNetwork, map.init, PlayerPartners),
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
			),

		   % loop
			(if
				IterationIndex < Parameters^maxIteration
			then
				createPlayerProfilesNetworkForIterationBin(
					IterationWidth, Parameters, Directory, RunIndex, IterationIndex + 1, Streams,
					MAverageNumberPartnersStream,
					Game, !ListBirthRecords,
					!PlayerProfileNetwork,
					!StrategyPlayMatrix,
					!BirthAdvancedResult, !BirthCache, !PlayerProfileCache,
					!FeedbackAsList, !IO)
			else
				finishPlayerProfilesNetworkForRunBin(
					IterationWidth,
					Parameters,
					Directory,
					RunIndex,
					!.StrategyPlayMatrix,
					!FeedbackAsList,
					!IO
				)
			)
			;
			RIIterationBirthRecords = ok(error(Error)),
			list.cons(string.format("io error while reading birth file: %s", [s(io.error_message(Error))]), !FeedbackAsList)
			;
			RIIterationBirthRecords = parseError,
			list.cons("parse error while reading birth file", !FeedbackAsList)
		)
		;
		MIIterationPlayerProfileRecords = ok(eof),
		finishPlayerProfilesNetworkForRunBin(
			IterationWidth,
			Parameters,
			Directory,
			RunIndex,
			!.StrategyPlayMatrix,
			!FeedbackAsList,
			!IO
		)
																 
		% (	% switch CreateMovie
		% 	CreateMovie = yes,
		% 								  % create movie
		% 	CmdAvi = string.format("avconv -y -i \"%s_R%d_T%%d.png\" -r 1 %s_R%d.avi", %
		% 		[s(Parameters^fileNamePrefix), i(RunIndex),
		% 		 s(Parameters^fileNamePrefix), i(RunIndex)]),
		% 	util.callSystem(CmdAvi, !IO)
		% ;
		% 	CreateMovie = no
		% ),
		% % create strategy play matrix
		% writeStrategyPlayMatrix(Parameters, RunIndex, !.StrategyPlayMatrix, !FeedbackAsList, !IO)
		;
		MIIterationPlayerProfileRecords = ok(error(Error)),
		list.cons(io.error_message(Error), !FeedbackAsList)
		;
		MIIterationPlayerProfileRecords = parseError,
		list.cons("parse error while reading player profile file", !FeedbackAsList)
	)
	.

/**
 * After all specified iterations have been processed by predicate {@code
 * createPlayerProfilesNetworkForIterationBin/22}, create the movie and write
 * a text file with the strategy play matrix.

*/

:- pred finishPlayerProfilesNetworkForRunBin(
	int                                             :: in,
	tools.export_playerProfiles_graphviz.parameters :: in,
	string                                          :: in,
	int                                             :: in,
	strategyPlayMatrix(C)                           :: in,
	list(string) :: in, list(string) :: out,
	io.state     :: di, io.state     :: uo
) is det
	<= printable(C)
.

finishPlayerProfilesNetworkForRunBin(
	IterationWidth,
	Parameters,
	Directory,
	RunIndex,
	StrategyPlayMatrix,
	!FeedbackAsList,
	!IO
) :-
	CreateMovie = Parameters^createMovie,
	(	% switch
		CreateMovie = yes,
		CmdAvi = string.format("avconv -y -i \"%s%s_R%d_T%%0%dd.png\" -r 25 %s%s_R%d.avi", %
			[s(Directory), s(Parameters^fileNamePrefix), i(RunIndex), i(IterationWidth),
			 s(Directory), s(Parameters^fileNamePrefix), i(RunIndex)]),
		util.callSystem(CmdAvi, !IO)
	;	
		CreateMovie = no
	),
	% create strategy play matrix
	writeStrategyPlayMatrix(Parameters, Directory, RunIndex, StrategyPlayMatrix, !FeedbackAsList, !IO)
	.


:- pred createPngsMovie( tools.export_playerProfiles_graphviz.parameters, int, int,  io.state, io.state).
:- mode createPngsMovie( in, in, in, di, uo) is det.

createPngsMovie( Parameters, RunIndex, NumberIterations, !IO) :-
	PredCreatePng =
	(pred(I::in, IOdi::di, IOuo::uo) is det :-
		CmdPng = string.format("dot -Tpng -o%s_R%d_T%d.png %s_R%d_T%d.dot",
			[s(Parameters^fileNamePrefix), i(RunIndex), i(I),
			 s(Parameters^fileNamePrefix), i(RunIndex), i(I)]),
		util.callSystem(CmdPng, IOdi, IOuo)
	),
	int.fold_up(PredCreatePng, 0, NumberIterations, !IO),
	CmdAvi = string.format("avconv -y -i \"%s_R%d_T%%d.png\" -r 20 %s_R%d.avi",
			[s(Parameters^fileNamePrefix), i(RunIndex),
			 s(Parameters^fileNamePrefix), i(RunIndex)]),
	util.callSystem(CmdAvi, !IO)
	.

/**
 * writeStrategyPlayMatrix(Parameters, RunIndex, StrategyPlayMatrix, !FeedbackAsList, !IO)

 * Write the strategy play matrix to a text file.
  
 */
:- pred writeStrategyPlayMatrix(
	 tools.export_playerProfiles_graphviz.parameters :: in,
	 string                                          :: in,
	 int                                             :: in,
	 strategyPlayMatrix(S)                           :: in,
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
 * Remove an iteration from the player profile network.  The network is
 * represented by a map with a list of iterations (where a player profile)
 * occurred stored as values.
 */

:- pred removeIteration(int, list(int), list(int)).
:- mode removeIteration(in, in, out) is det.

removeIteration(_, [], []).
removeIteration(E, [H | T], R) :-
	(if
		E = H
	then
		R = T
	else
		removeIteration(E, T, RR),
		R = [H | RR]
	).

/**
 * addIteration(Iteration, PlayerProfile, !PlayerProfileNetwork)
  
 * Player profile network update operation that adds the current iteration
 * to the given player profile list of iterations.

 * @param Iteration the current iteration.
 * @param PlayerProfile the player profile.
 * @param !PlayerProfileNetwork the player profile network to update.
 */
:- pred addIteration(int, list(key), playerProfileNetwork, playerProfileNetwork).
:- mode addIteration(in, in, in, out) is det.

addIteration(Iteration, PlayerProfile, !PlayerProfileNetwork) :-
	(if
		map.search(!.PlayerProfileNetwork, PlayerProfile, ListIterations)
	then
		map.det_update(PlayerProfile, [Iteration | ListIterations], !PlayerProfileNetwork)
	else
		map.det_insert(PlayerProfile, [Iteration], !PlayerProfileNetwork)
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
	list(playerBirthRecord(C)) :: in,
	list(key)                  :: in,
	strategyPlayMatrix(C)      :: in,  strategyPlayMatrix(C) :: out
) is det.

incrementCount(ListBirthRecords, PlayerProfile, !StrategyPlayMatrix) :-
	MapPlayerIDStrategy =
	(func(ID) = R :-
		(if
			ebea.streams.birth.search(ID, ListBirthRecords, S)
		then
			R = S^chromosome^strategyGenes
		else
			throw("processPlayerProfile/7: not reachable")
		)
	),
	list.map(MapPlayerIDStrategy, PlayerProfile) = ListStrategies,
	(if
		map.search(!.StrategyPlayMatrix, ListStrategies, Count)
	then
		map.det_update(ListStrategies, Count + 1, !StrategyPlayMatrix)
	else
		map.det_insert(ListStrategies, 1, !StrategyPlayMatrix)
	)
	.
	
:- pred processPlayerProfile(
	int                        :: in,
	list(playerBirthRecord(C)) :: in,
	list(key)                  :: in,
	playerProfileNetwork       :: in,  playerProfileNetwork  :: out,
	strategyPlayMatrix(C)      :: in,  strategyPlayMatrix(C) :: out
	) is det.

processPlayerProfile(
	Iteration,
	ListBirthRecords,
	PlayerProfile,
	!PlayerProfileNetwork,
	!StrategyPlayMatrix
) :-
	addIteration(Iteration, PlayerProfile, !PlayerProfileNetwork),
	MapPlayerIDStrategy =
	(func(ID) = R :-
		(if
			ebea.streams.birth.search(ID, ListBirthRecords, S)
		then
			R = S^chromosome^strategyGenes
		else
			throw("processPlayerProfile/7: not reachable")
		)
	),
	list.map(MapPlayerIDStrategy, PlayerProfile) = ListStrategies,
	(if
		map.search(!.StrategyPlayMatrix, ListStrategies, Count)
	then
		map.det_update(ListStrategies, Count + 1, !StrategyPlayMatrix)
	else
		map.det_insert(ListStrategies, 1, !StrategyPlayMatrix)
	)
	.
	% (	%
	% 	PlayerProfile = []
	% ;
	% 	PlayerProfile = [SelectingPlayer | RestPlayerProfile],
	% 	UpdateFunc =
	% 	(pred(ID::in, AC::in, R::out) is det :-
	% 		(if
	% 			ebea.streams.birth.search(SelectingPlayer, ListBirthRecords, BR1),
	% 			ebea.streams.birth.search(ID, ListBirthRecords, BR2)
	% 		then
	% 			Set = set.from_list([BR1^chromosome^strategyGenes, BR2^chromosome^strategyGenes]),
	% 			(if
	% 				map.search(AC, Set, Count)
	% 			then
	% 				map.det_update(Set, Count + 1, AC, R)
	% 			else
	% 				map.det_insert(Set, 1, AC, R)
	% 			)
	% 		else
	% 			throw("cannot build strategy play matrix")
	% 		)
	% 	),
	% 	list.foldl(UpdateFunc, RestPlayerProfile, !StrategyPlayMatrix)
	% ).

% /**
%  * playerID(PlayerProfiles, ID)

%  * Unify {@code ID} with a player ID taken from the given list of player
%  * profiles.  This list is stored in the {@code birth} stream.
%  */

% :- pred playerID(list(list(int)), int).
% :- mode playerID(in, out) is nondet.

% playerID(PlayerProfiles, ID) :-
% 	list.member(PlayerProfile, PlayerProfiles),
% 	list.member(ID, PlayerProfile).

/**
 * playerIDNet(PlayerProfileNetwork, ID)

 * Unify {@code ID} with a player ID taken from the given list of player
 * profiles.  This list is stored in the {@code birth} stream.
 */

:- pred playerIDNet(playerProfileNetwork, key).
:- mode playerIDNet(in, out) is nondet.

playerIDNet(PlayerProfileNetwork, ID) :-
	map.member(PlayerProfileNetwork, PlayerProfile, [_ | _]),
	list.member(ID, PlayerProfile).


:- func playerProfile(playerProfileRecord) = list(int).

playerProfile(ppr(_, pp(Result))) = Result.



/**
 * Given a key value pair from the player profile network update the
 * selected partners of a player.
  
 */
:- pred updatePartners(list(key), list(int), map(key, set_bbbtree.set_bbbtree(key)), map(key, set_bbbtree.set_bbbtree(key))).
:- mode updatePartners(in, in, in, out) is det.

updatePartners(StrategyProfile, ListIterations, !PlayerPartners) :-
	(if
		ListIterations = [_|_]
	then
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
	else
		true
	)
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

:- pred printHeader(io.output_stream, io.state, io.state).
:- mode printHeader(in, di, uo) is det.

printHeader(Stream, !IO) :-
	io.print(Stream, "graph {
size=\"10,10\"
page=\"10,10\"
center=1
", !IO).

:- pred printNode(io.output_stream, tools.export_playerProfiles_graphviz.parameters, list(playerBirthRecord(C)), key, io.state, io.state)
	<= printable(C).
:- mode printNode(in, in, in, in, di, uo) is det.

printNode(Stream, Parameters, ListBirthRecords, ID, !IO) :-
	Parameters^printPlayerID = PrintPlayerID,
	Parameters^printSiteIndex = PrintSiteIndex,
	Parameters^printSelectionGenes = PrintSelectionGenes,
	Parameters^printStrategyGenes = PrintStrategyGenes,
	io.print(Stream, "P", !IO),
	io.print(Stream, ID, !IO),
	io.print(Stream, " [label=\"", !IO),
	PredPrint =
	(pred(!.First::in, !:First::out, !.IO::di, !:IO::uo) is det :-
		(
			PrintPlayerID = yes,
			printFirst(Stream, "\\n", !First, !IO),
			io.print(Stream, ID, !IO)
		;
			PrintPlayerID = no
		),
		(if
			(	
				PrintSelectionGenes = yes ;
				PrintStrategyGenes = yes ;
				PrintSiteIndex = yes
			),
			ebea.streams.birth.search(ID, ListBirthRecords, BR)
		then
			(
				PrintSiteIndex = yes,
				printFirst(Stream, "\\n", !First, !IO),
%				io.print(Stream, "\\n", !IO),
				printable.print(Stream, BR^siteIndex, !IO)
			;	
				PrintSiteIndex = no
			),
			(
				PrintSelectionGenes = yes,
				printFirst(Stream, "\\n", !First, !IO),
%				io.print(Stream, "\\n", !IO),
				printable.print(Stream, BR^chromosome^selectionGenes, !IO)
			;	
				PrintSelectionGenes = no
			),
			(
				PrintStrategyGenes = yes,
				printFirst(Stream, "\\n", !First, !IO),
%				io.print(Stream, "\\n", !IO),
				printable.print(Stream, BR^chromosome^strategyGenes, !IO)
			;
				PrintStrategyGenes = no
			)
		else
			true
		)
	),
	PredPrint(no, _, !IO),
	io.print(Stream, "\"]\n", !IO)
	.
/*
:- pred search(int, list(playerBirthRecord(C)), playerBirthRecord(C)).
:- mode search(in, in, out) is semidet.

search(ID, [H | T], R) :-
	(if
		H^id = ID
	then
		R = H
	else
		search(ID, T, R)
	).
*/
:- pred printEdges(io.output_stream, tools.export_playerProfiles_graphviz.parameters, bool, list(key), list(int), io.state, io.state).
:- mode printEdges(in, in, in, in, in, di, uo) is det.

printEdges(Stream, Parameters, IsSelectingPlayer, PlayerProfile, ListIterations, !IO) :-
	RawValue = list.length(ListIterations),
	(if
		RawValue * 100 / Parameters^slidingWindowSize >= Parameters^minPercentageValue
	then
		PrintRawValues = Parameters^printRawValues,
		(	/* switch */
			PrintRawValues = yes,
			EdgeLabel = string.format("%d", [i(RawValue)])
			;
			PrintRawValues = no,
			EdgeLabel = string.format("%d", [i(RawValue * 100 / Parameters^slidingWindowSize)])
		),
		EdgeWeight = float(RawValue),
		(	/* switch */
			ListIterations = []
		;	
			ListIterations = [_|_],
			PlayerProfile = []
		;	
			ListIterations = [_|_],
			PlayerProfile = [H | T],
			printAnEdge(Stream, IsSelectingPlayer, H, T, EdgeLabel, EdgeWeight, !IO),
			(if
				Parameters^onlyPrintSelectingEdge = no
			then
				printEdges(Stream, Parameters, no, T, ListIterations, !IO)
			else
				true
			)
		)
	else
		true
	)
	.

% printEdges(_Stream, _IsSelectingPlayer, [], _ListIterations, !IO).
% printEdges(Stream, IsSelectingPlayer, [H | T], ListIterations, !IO) :-
% 	EdgeWeight = float(list.length(ListIterations)),
% 	printAnEdge(Stream, IsSelectingPlayer, H, T, EdgeWeight, !IO),
% 	printEdges(Stream, no, T, ListIterations, !IO)
% 	.

:- pred printAnEdge(io.output_stream, bool, key, list(key), string, float, io.state, io.state).
:- mode printAnEdge(in, in, in, in, in, in, di, uo) is det.

printAnEdge(_Stream, _IsSelectingPlayer, _, [], _EdgeLabel, _EdgeWeight, !IO).
printAnEdge(Stream, IsSelectingPlayer, ID1, [ID2 | T], EdgeLabel, EdgeWeight, !IO) :-
	io.print(Stream, "P", !IO),
	io.print(Stream, ID1, !IO),
	% (
	% 	IsSelectingPlayer = no,
	% 	io.print(Stream, "--", !IO)
	% 	;
	% 	IsSelectingPlayer = yes,
	% 	io.print(Stream, "->", !IO)
	% ),
	io.print(Stream, "--P", !IO),
	io.print(Stream, ID2, !IO),
	io.print(Stream, "[dir=", !IO),
	(
		IsSelectingPlayer = no,
		io.print(Stream, "none", !IO)
		;
		IsSelectingPlayer = yes,
		io.print(Stream, "forward", !IO)
	),
	io.print(Stream, " label=\"", !IO),
	io.print(Stream, EdgeLabel, !IO),
	io.print(Stream, "\" weight=\"", !IO),
	io.print(Stream, EdgeWeight, !IO),
	io.print(Stream, "\"]\n", !IO),
	printAnEdge(Stream, IsSelectingPlayer, ID1, T, EdgeLabel, EdgeWeight, !IO)
	.

:- pred printEnd(io.output_stream, io.state, io.state).
:- mode printEnd(in, di, uo) is det.

printEnd(Stream, !IO) :-
	io.print(Stream, "}\n", !IO).


:- pred printPlayerProfile(io.output_stream, playerProfile, io.state, io.state).
:- mode printPlayerProfile(in, in, di, uo) is det.

printPlayerProfile(Stream, pp(PlayerProfile), !IO) :-
	PlayerProfile = []
	;
	PlayerProfile = [ID | RestIDs],
	io.print(Stream, "P", !IO),
	io.print(Stream, ID, !IO),
	PredPrint =
	(pred(I::in, IOdi::di, IOuo::uo) is det :-
		io.print(Stream, "--P", IOdi, IO1),
		io.print(Stream, I, IO1, IOuo)
	),
	list.foldl(PredPrint, RestIDs, !IO)
	.



/**
 * printFirst(Stream, String, !First, !IO)
  
 * Prints string to the given stream if {@code !First} state is {@code
 * yes}.  The next state of {@code First} is set to {@code yes}.
  
 */
:- pred printFirst(io.output_stream, string, bool, bool, io.state, io.state).
:- mode printFirst(in, in, in, out, di, uo) is det.

printFirst(_Stream, _String, no, yes, !IO).

printFirst(Stream, String, yes, yes, !IO) :-
	io.print(Stream, String, !IO).










:- pred readPlayerProfiles(
	ebea.streams.inStreams, int,
	maybe(playerProfileRecord), maybe(playerProfileRecord),
	maybe(list(playerProfileRecord)), maybe(list(playerProfileRecord)),
	list(string), list(string),
	io.state, io.state).
:- mode readPlayerProfiles(
	in(detailedTxt), in,
	in, out,
	in, out,
	in, out,
	di, uo) is det.

readPlayerProfiles(Streams, IterationIndex, !MSavePlayerProfile, !MPlayerProfiles, !FeedbackAsList, !IO) :-
	!.MSavePlayerProfile = yes(SavePlayerProfile),
	(if
		SavePlayerProfile^iteration = IterationIndex
	then
		addMaybeList(SavePlayerProfile, !MPlayerProfiles),
		readPlayerProfiles(Streams, IterationIndex, no, !:MSavePlayerProfile, !MPlayerProfiles, !FeedbackAsList, !IO)
	else if
		SavePlayerProfile^iteration > IterationIndex
	then
		okifyMaybeList(!MPlayerProfiles)
	else
		throw("Invalid combination of saved player profile and iteration index")
	)
	;
	!.MSavePlayerProfile = no,
	readPlayerProfile(Streams, MPlayerProfile, !FeedbackAsList, !IO),
	(
		MPlayerProfile = yes(PlayerProfile),
		(if
			PlayerProfile^iteration = IterationIndex
		then
			addMaybeList(PlayerProfile, !MPlayerProfiles),
			readPlayerProfiles(Streams, IterationIndex, !MSavePlayerProfile, !MPlayerProfiles, !FeedbackAsList, !IO)
		else if
			PlayerProfile^iteration > IterationIndex
		then
			!:MSavePlayerProfile = yes(PlayerProfile),
			okifyMaybeList(!MPlayerProfiles)
		else
			throw("Invalid combination of saved player profile and iteration index")
		)
		;
		MPlayerProfile = no
	)
	.

:- pred addMaybeList(T, maybe(list(T)), maybe(list(T))).
:- mode addMaybeList(in, in, out) is det.

addMaybeList(X, no, yes([X])).
addMaybeList(X, yes(L), yes([X | L])).

:- pred okifyMaybeList(maybe(list(T)), maybe(list(T))).
:- mode okifyMaybeList(in, out) is det.

okifyMaybeList(no, yes([])).
okifyMaybeList(yes(L), yes(L)).

:- pred readPlayerProfile(ebea.streams.inStreams, maybe(playerProfileRecord), list(string), list(string), io.state, io.state).
:- mode readPlayerProfile(in(detailedTxt), out, in, out, di, uo) is det.

readPlayerProfile(Streams, MPlayerProfileRecord, !FeedbackAsList, !IO) :-
	io.read_line_as_string(Streams^tisPlayerProfile, ILine, !IO),
	(
		ILine = ok(Line),
		(if
			string.words(Line) = [SIteration | SPlayerIDS],
			string.to_int(SIteration, Iteration),
			list.map(string.to_int, SPlayerIDS, PlayerIDS)
		then
			PlayerProfileRecord = ppr(Iteration, pp(PlayerIDS)),
			MPlayerProfileRecord = yes(PlayerProfileRecord)
		else
			MPlayerProfileRecord = no,
			list.cons("Parse error while processing a line", !FeedbackAsList)
		)
		;
		ILine = eof,
		MPlayerProfileRecord = no
		;
		ILine = error(Error),
		MPlayerProfileRecord = no,
		list.cons(string.format("IO error reading player profiles file: %s", [s(io.error_message(Error))]), !FeedbackAsList)
	).




:- func printPlayerID(tools.export_playerProfiles_graphviz.parameters) = bool.
:- func 'printPlayerID :='(tools.export_playerProfiles_graphviz.parameters, bool) = tools.export_playerProfiles_graphviz.parameters.

:- func printSiteIndex(tools.export_playerProfiles_graphviz.parameters) = bool.
:- func 'printSiteIndex :='(tools.export_playerProfiles_graphviz.parameters, bool) = tools.export_playerProfiles_graphviz.parameters.

:- func printSelectionGenes(tools.export_playerProfiles_graphviz.parameters) = bool.
:- func 'printSelectionGenes :='(tools.export_playerProfiles_graphviz.parameters, bool) = tools.export_playerProfiles_graphviz.parameters.

:- func printStrategyGenes(tools.export_playerProfiles_graphviz.parameters) = bool.
:- func 'printStrategyGenes :='(tools.export_playerProfiles_graphviz.parameters, bool) = tools.export_playerProfiles_graphviz.parameters.

:- func onlyPrintSelectingEdge(tools.export_playerProfiles_graphviz.parameters) = bool.
:- func 'onlyPrintSelectingEdge :='(tools.export_playerProfiles_graphviz.parameters, bool) = tools.export_playerProfiles_graphviz.parameters.

:- func slidingWindowSize(tools.export_playerProfiles_graphviz.parameters) = int.
:- func 'slidingWindowSize :='(tools.export_playerProfiles_graphviz.parameters, int) = tools.export_playerProfiles_graphviz.parameters.

:- func createPng(tools.export_playerProfiles_graphviz.parameters) = bool.
:- func 'createPng :='(tools.export_playerProfiles_graphviz.parameters, bool) = tools.export_playerProfiles_graphviz.parameters.

:- func createMovie(tools.export_playerProfiles_graphviz.parameters) = bool.
:- func 'createMovie :='(tools.export_playerProfiles_graphviz.parameters, bool) = tools.export_playerProfiles_graphviz.parameters.

:- func createAverageNumberPartners(tools.export_playerProfiles_graphviz.parameters) = bool.
:- func 'createAverageNumberPartners :='(tools.export_playerProfiles_graphviz.parameters, bool) = tools.export_playerProfiles_graphviz.parameters.

:- func maxIteration(tools.export_playerProfiles_graphviz.parameters) = int.
:- func 'maxIteration :='(tools.export_playerProfiles_graphviz.parameters, int) = tools.export_playerProfiles_graphviz.parameters.

:- func fileNamePrefix(tools.export_playerProfiles_graphviz.parameters) = string.
:- func 'fileNamePrefix :='(tools.export_playerProfiles_graphviz.parameters, string) = tools.export_playerProfiles_graphviz.parameters.

:- func printRawValues(tools.export_playerProfiles_graphviz.parameters) = bool.
:- func 'printRawValues :='(tools.export_playerProfiles_graphviz.parameters, bool) = tools.export_playerProfiles_graphviz.parameters.

:- func minPercentageValue(tools.export_playerProfiles_graphviz.parameters) = int.
:- func 'minPercentageValue :='(tools.export_playerProfiles_graphviz.parameters, int) = tools.export_playerProfiles_graphviz.parameters.

:- end_module tools.export_playerProfiles_graphviz.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
