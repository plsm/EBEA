/**
 *

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
:- import_module bool, io, list, string.

:- type parameters --->
	parameters(
		printSelectionGenes    :: bool ,
		printStrategyGenes     :: bool ,
		onlyPrintSelectingEdge :: bool ,
		slidingWindowSize      :: int ,
		createPng              :: bool ,
		createMovie            :: bool ,
		maxIteration           :: int ,
		fileNamePrefix         :: string
	) .

:- pred createPlayerProfilesNetworks(data.config.config, tools.export_playerProfiles_graphviz.parameters, string, io.state, io.state).
:- mode createPlayerProfilesNetworks(in, in, out, di, uo) is det.

/**
 * Return a default value of {@code parameters}.
 */
:- func default_parameters = tools.export_playerProfiles_graphviz.parameters.

:- func dialog_parameters = list(dialogItem(tools.export_playerProfiles_graphviz.parameters)).

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

:- func fileNamePrefix(tools.export_playerProfiles_graphviz.parameters) = string.
:- func 'fileNamePrefix :='(tools.export_playerProfiles_graphviz.parameters, string) = tools.export_playerProfiles_graphviz.parameters.

:- implementation.

:- import_module ebea, ebea.player, ebea.streams, ebea.streams.birth, ebea.streams.playerProfile.
:- import_module game.
:- import_module parseable, parseable.iou, printable.
:- import_module util.
:- import_module data.util.
:- import_module exception, float, int, map, maybe, solutions.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

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

:- type playerProfileNetwork == map(list(int), list(int)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

createPlayerProfilesNetworks(Data, Parameters, Feedback, !IO) :-
	Level = Data^level,
	(
		(
			Level = detailedTxt
			;
			Level = detailedBin
		),
		int.fold_up2(createPlayerProfilesNetworkForRun(Data, Level, Parameters), 1, Data^numberRuns, [], FeedbackAsList, !IO),
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

default_parameters = parameters(no, yes, yes, 10, no, yes, 200, "player-profiles").

dialog_parameters =
	[
	di(label("print selection genes"),      updateFieldBool(     printSelectionGenes,     set('printSelectionGenes :='))),
	di(label("print strategy genes"),       updateFieldBool(     printStrategyGenes,      set('printStrategyGenes :='))),
	di(label("only print selecting edge"),  updateFieldBool(     onlyPrintSelectingEdge,  set('onlyPrintSelectingEdge :='))),
	di(label("sliding window size"),        updateFieldInt(      slidingWindowSize,       checkInt(   "sliding window size",       bounded(0, yes), unbound, 'slidingWindowSize :='))),
	di(label("create png"),                 updateFieldBool(     createPng,               set('createPng :='))),
	di(label("create movie"),               updateFieldBool(     createMovie,             set('createMovie :='))),
	di(label("filename prefix"),            updateFieldString(   fileNamePrefix,          set('fileNamePrefix :=')))
	].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- pred createPlayerProfilesNetworkForRun(data.config.config, ebea.streams.level, tools.export_playerProfiles_graphviz.parameters, int, list(string), list(string), io.state, io.state).
:- mode createPlayerProfilesNetworkForRun(in, in(detailedLevel), in, in, in, out, di, uo) is det.

createPlayerProfilesNetworkForRun(Data, Level, Parameters, RunIndex, !FeedbackAsList, !IO) :-
%	Level = detailedTxt,
	ebea.streams.openInputStreams(Level, yes(string.format("_R%d", [i(RunIndex)])), IMStreams, !IO),
	(
		IMStreams = ok(Streams),
		(
			Streams = detailedTxt(_, _, _, _),
			io.format("Run %d\n", [i(RunIndex)], !IO),
			createPlayerProfilesNetworkForIterationTxt(Data, Parameters, RunIndex, 0, Streams, no, _, !FeedbackAsList, !IO),
			ebea.streams.closeInputStreams(Streams, !IO),
			io.print("\r                      \n", !IO)
			;
			Streams = detailedBin(_, _, _, _),
			io.format("Run %d\n", [i(RunIndex)], !IO),
			ebea.streams.birth.read(Streams^bisBirth, -1, RIIterationBirthRecords, no, BirthAdvancedResult, parseable.iou.cacheInit, BirthCache, !IO),

			(
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
			createPlayerProfilesNetworkForIterationBin(
				Data, Parameters, RunIndex, 0, Streams,
				Game, ListBirthRecords, _,
				map.init, _,
				BirthAdvancedResult, Debug0, BirthCache, Debug1, parseable.iou.cacheInit, Debug2,
				!FeedbackAsList, !IO),
			io.print(Debug0, !IO), io.nl(!IO),
			io.print(Debug1, !IO), io.nl(!IO),
			io.print(Debug2, !IO), io.nl(!IO),
		  /*
			(
				(
					RIIterationBirthRecords = delayed,
					ListBirthRecords = []
					;
					RIIterationBirthRecords = ok(ok(IterationBirthRecords)),
					IterationBirthRecords = ibr(_, ListBirthRecords)
				),
				data.util.gameConfig(Data) = gcex(Game, _, _),
				createPlayerProfilesNetworkForIterationBin(
					Data, Parameters, RunIndex, 0, Streams,
					Game, ListBirthRecords, _,
					BirthAdvancedResult, _, BirthCache, _, parseable.iou.cacheInit, _,
					!FeedbackAsList, !IO)
				;
				RIIterationBirthRecords = ok(eof)
				;
				RIIterationBirthRecords = ok(error(Error)),
				list.cons(string.format("io error while reading birth file: %s", [s(io.error_message(Error))]), !FeedbackAsList)
				;
				RIIterationBirthRecords = parseError,
				list.cons("parse error while reading birth file", !FeedbackAsList)
			),

*/

		  
/*			(
				Data^selectedGame = '2x2',
				createPlayerProfilesNetworkForIterationBin(
					Data, Parameters, RunIndex, 0, Streams,
					Data^cfg_2x2^game, [], _,
					no, _, parseable.iou.cacheInit, _, parseable.iou.cacheInit, _,
					!FeedbackAsList, !IO)
				;
				Data^selectedGame = battlesexes,
				createPlayerProfilesNetworkForIterationBin(
					Data, Parameters, RunIndex, 0, Streams,
					Data^battlesexes^game, [], _,
					no, _, parseable.iou.cacheInit, _, parseable.iou.cacheInit, _,
					!FeedbackAsList, !IO)
				;
				Data^selectedGame = centipede,
				createPlayerProfilesNetworkForIterationBin(
					Data, Parameters, RunIndex, 0, Streams,
					Data^centipede^game, [], _,
					no, _, parseable.iou.cacheInit, _, parseable.iou.cacheInit, _,
					!FeedbackAsList, !IO)
				;
				Data^selectedGame = givetake,
				createPlayerProfilesNetworkForIterationBin(
					Data, Parameters, RunIndex, 0, Streams,
					Data^givetake^game, [], _,
					no, _, parseable.iou.cacheInit, _, parseable.iou.cacheInit, _,
					!FeedbackAsList, !IO)
				;
				Data^selectedGame = investment,
				createPlayerProfilesNetworkForIterationBin(
					Data, Parameters, RunIndex, 0, Streams,
					Data^investment^game, [], _,
					no, _, parseable.iou.cacheInit, _, parseable.iou.cacheInit, _,
					!FeedbackAsList, !IO)
				;
				Data^selectedGame = pgp,
				createPlayerProfilesNetworkForIterationBin(
					Data, Parameters, RunIndex, 0, Streams,
					Data^pgp^game, [], _,
					no, _, parseable.iou.cacheInit, _, parseable.iou.cacheInit, _,
					!FeedbackAsList, !IO)
				;
				Data^selectedGame = 'pgp+pa',
				createPlayerProfilesNetworkForIterationBin(
					Data, Parameters, RunIndex, 0, Streams,
					Data^'pgp+pa'^game, [], _,
					no, _, parseable.iou.cacheInit, _, parseable.iou.cacheInit, _,
					!FeedbackAsList, !IO)
				;
				Data^selectedGame = ultimatum,
				createPlayerProfilesNetworkForIterationBin(
					Data, Parameters, RunIndex, 0, Streams,
					Data^ultimatum^game, [], _,
					no, _, parseable.iou.cacheInit, _, parseable.iou.cacheInit, _,
					!FeedbackAsList, !IO)
			),*/
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
		MPlayerProfiles = yes(PlayerProfiles),
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
	data.config.config, tools.export_playerProfiles_graphviz.parameters, int, int, ebea.streams.inStreams,
	G, list(playerBirthRecord(C)), list(playerBirthRecord(C)),
	playerProfileNetwork, playerProfileNetwork,
	maybe(iterationBirthRecords(C)), maybe(iterationBirthRecords(C)), parseable.iou.cache, parseable.iou.cache, parseable.iou.cache, parseable.iou.cache,
	list(string), list(string), io.state, io.state)
	<= (asymmetricGame(G, C), parseable(C), printable(C)).
:- mode createPlayerProfilesNetworkForIterationBin(
	in, in, in, in, in(detailedBin),
	in, in, out,
	in, out,
	in, out, in, out, in, out,
	in, out, di, uo) is det.

createPlayerProfilesNetworkForIterationBin(
	Data, Parameters, RunIndex, IterationIndex, Streams,
	Game, !ListBirthRecords,
	!PlayerProfileNetwork,
	!BirthAdvancedResult, !BirthCache, !PlayerProfileCache,
	!FeedbackAsList, !IO
	) :-

	io.format("\rIteration %d", [i(IterationIndex)], !IO),
	io.flush_output(io.stdout_stream, !IO),
	ebea.streams.playerProfile.read(Streams^bisPlayerProfile, Game, !PlayerProfileCache, MIIterationPlayerProfileRecords, !IO),
	(
		MIIterationPlayerProfileRecords = ok(ok(IterationPlayerProfileRecords)),
		ebea.streams.birth.read(Streams^bisBirth, IterationIndex, RIIterationBirthRecords, !BirthAdvancedResult, !BirthCache, !IO),
		(
			(
				RIIterationBirthRecords = delayed
				;
				RIIterationBirthRecords = ok(ok(IterationBirthRecords)),
				IterationBirthRecords = ibr(_, NewBirthRecords),
				list.append(NewBirthRecords, !ListBirthRecords)
				;
				RIIterationBirthRecords = ok(eof)
			),
			FileName = string.format("%s_R%d_T%d.dot", [s(Parameters^fileNamePrefix), i(RunIndex), i(IterationIndex)]),
			io.open_output(FileName, IStream, !IO),
			(
				IStream = ok(DotStream),
				% update player profile network
				IterationPlayerProfileRecords = ippr(_, PlayerProfiles),
				map.map_values_only(removeIteration(IterationIndex - Parameters^slidingWindowSize - 1), !PlayerProfileNetwork),
				list.foldl(addIteration(IterationIndex), PlayerProfiles, !PlayerProfileNetwork),
				% write dot file
				printHeader(DotStream, !IO),
				PlayersID = solutions.solutions(playerIDNet(!.PlayerProfileNetwork)),
				list.foldl(printNode(DotStream, Parameters, !.ListBirthRecords), PlayersID, !IO),
				map.foldl(printEdges(DotStream, yes), !.PlayerProfileNetwork, !IO),
				printEnd(DotStream, !IO),
				io.close_output(DotStream, !IO),
				% create png graph
				CmdPng = string.format("dot -Tpng -o\"%s_R%d_T%d.png\" \"%s_R%d_T%d.dot\"",
					[s(Parameters^fileNamePrefix), i(RunIndex), i(IterationIndex),
					 s(Parameters^fileNamePrefix), i(RunIndex), i(IterationIndex)]),
				util.callSystem(CmdPng, !IO),
			   % loop
				(if
					IterationIndex < Parameters^maxIteration
				then
					createPlayerProfilesNetworkForIterationBin(
						Data, Parameters, RunIndex, IterationIndex + 1, Streams,
						Game, !ListBirthRecords,
						!PlayerProfileNetwork,
						!BirthAdvancedResult, !BirthCache, !PlayerProfileCache,
						!FeedbackAsList, !IO)
				else
					true
				)
				;
				IStream = error(Error),
				list.cons(string.format("IO error opening `%s` file: %s", [s(FileName), s(io.error_message(Error))]), !FeedbackAsList)
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
		% create movie
		CmdAvi = string.format("avconv -y -i \"%s_R%d_T%%d.png\" -r 10 %s_R%d.avi",
			[s(Parameters^fileNamePrefix), i(RunIndex),
			 s(Parameters^fileNamePrefix), i(RunIndex)]),
		util.callSystem(CmdAvi, !IO)
		;
		MIIterationPlayerProfileRecords = ok(error(Error)),
		list.cons(io.error_message(Error), !FeedbackAsList)
		;
		MIIterationPlayerProfileRecords = parseError,
		list.cons("parse error while reading player profile file", !FeedbackAsList)
	)
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
:- pred addIteration(int, list(int), playerProfileNetwork, playerProfileNetwork).
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
 * playerID(PlayerProfiles, ID)

 * Unify {@code ID} with a player ID taken from the given list of player
 * profiles.  This list is stored in the {@code birth} stream.
 */

:- pred playerID(list(list(int)), int).
:- mode playerID(in, out) is nondet.

playerID(PlayerProfiles, ID) :-
	list.member(PlayerProfile, PlayerProfiles),
	list.member(ID, PlayerProfile).

/**
 * playerIDNet(PlayerProfileNetwork, ID)

 * Unify {@code ID} with a player ID taken from the given list of player
 * profiles.  This list is stored in the {@code birth} stream.
 */

:- pred playerIDNet(playerProfileNetwork, int).
:- mode playerIDNet(in, out) is nondet.

playerIDNet(PlayerProfileNetwork, ID) :-
	map.member(PlayerProfileNetwork, PlayerProfile, [_ | _]),
	list.member(ID, PlayerProfile).


:- func playerProfile(playerProfileRecord) = list(int).

playerProfile(ppr(_, pp(Result))) = Result.

:- pred printHeader(io.output_stream, io.state, io.state).
:- mode printHeader(in, di, uo) is det.

printHeader(Stream, !IO) :-
	io.print(Stream, "graph {
size=\"10,10\"
page=\"10,10\"
center=1
", !IO).

:- pred printNode(io.output_stream, tools.export_playerProfiles_graphviz.parameters, list(playerBirthRecord(C)), int, io.state, io.state)
	<= printable(C).
:- mode printNode(in, in, in, in, di, uo) is det.

printNode(Stream, Parameters, ListBirthRecords, ID, !IO) :-
	Parameters^printSelectionGenes = PrintSelectionGenes,
	Parameters^printStrategyGenes = PrintStrategyGenes,
	io.print(Stream, "P", !IO),
	io.print(Stream, ID, !IO),
	io.print(Stream, " [label=\"", !IO),
	io.print(Stream, ID, !IO),
	(if
		(
			PrintSelectionGenes = yes ;
			PrintStrategyGenes = yes
		),
		ebea.streams.birth.search(ID, ListBirthRecords, BR)
	then
		(
			PrintSelectionGenes = yes,
			io.print(Stream, "\\n", !IO),
			printable.print(Stream, BR^chromosome^selectionGenes, !IO)
			;
			PrintSelectionGenes = no
		),
		(
			PrintStrategyGenes = yes,
			io.print(Stream, "\\n", !IO),
			printable.print(Stream, BR^chromosome^strategyGenes, !IO)
			;
			PrintStrategyGenes = no
		)
	else
		true
	),
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
:- pred printEdges(io.output_stream, bool, list(int), list(int), io.state, io.state).
:- mode printEdges(in, in, in, in, di, uo) is det.

printEdges(Stream, IsSelectingPlayer, PlayerProfile, ListIterations, !IO) :-
	ListIterations = []
	;
	ListIterations = [_|_],
	PlayerProfile = []
	;
	ListIterations = [_|_],
	PlayerProfile = [H | T],
	EdgeWeight = float(list.length(ListIterations)),
	printAnEdge(Stream, IsSelectingPlayer, H, T, EdgeWeight, !IO),
	printEdges(Stream, no, T, ListIterations, !IO)
	.

% printEdges(_Stream, _IsSelectingPlayer, [], _ListIterations, !IO).
% printEdges(Stream, IsSelectingPlayer, [H | T], ListIterations, !IO) :-
% 	EdgeWeight = float(list.length(ListIterations)),
% 	printAnEdge(Stream, IsSelectingPlayer, H, T, EdgeWeight, !IO),
% 	printEdges(Stream, no, T, ListIterations, !IO)
% 	.

:- pred printAnEdge(io.output_stream, bool, int, list(int), float, io.state, io.state).
:- mode printAnEdge(in, in, in, in, in, di, uo) is det.

printAnEdge(_Stream, _IsSelectingPlayer, _, [], _EdgeWeight, !IO).
printAnEdge(Stream, IsSelectingPlayer, ID, [H | T], EdgeWeight, !IO) :-
	io.print(Stream, "P", !IO),
	io.print(Stream, ID, !IO),
	% (
	% 	IsSelectingPlayer = no,
	% 	io.print(Stream, "--", !IO)
	% 	;
	% 	IsSelectingPlayer = yes,
	% 	io.print(Stream, "->", !IO)
	% ),
	io.print(Stream, "--P", !IO),
	io.print(Stream, H, !IO),
	io.print(Stream, "[dir=", !IO),
	(
		IsSelectingPlayer = no,
		io.print(Stream, "none", !IO)
		;
		IsSelectingPlayer = yes,
		io.print(Stream, "forward", !IO)
	),
	io.print(Stream, " label=\"", !IO),
	io.print(Stream, EdgeWeight, !IO),
	io.print(Stream, "\" weight=\"", !IO),
	io.print(Stream, EdgeWeight, !IO),
	io.print(Stream, "\"]\n", !IO),
	printAnEdge(Stream, IsSelectingPlayer, ID, T, EdgeWeight, !IO)
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




:- end_module tools.export_playerProfiles_graphviz.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
