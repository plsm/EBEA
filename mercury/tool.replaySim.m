/**
 * Read the files produced by an EBEA run and show the data collected.

 * @author Pedro Mariano
 * @version 1.0 2013/05/22
 */
:- module tool.replaySim.

:- interface.

:- import_module io, maybe.

:- pred go(maybe(string), io.state, io.state).
:- mode go(in, di, uo) is det.

:- implementation.

:- import_module ebea, ebea.energy, ebea.player, ebea.population, ebea.streams.
:- import_module file, file.config.
:- import_module float, int, list, string.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

:- type player --->
	empty ;
	player(
		id     :: int,
		energy :: float,
		age    :: int
	).

:- type linePhenotype --->
	linePhenotype(
		round        :: int,
		playerID     :: int,
		playerEnergy :: float,
		playerAge    :: int
	).

:- type lineDeath --->
	lineDeath(
		dround        :: int,
		dplayerID     :: int
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

go(MFilename, !IO) :-
	file.config.scan(MFilename, IMConfig, !IO),
	(if
		IMConfig = ok(ok(cfg(Config)))
	then
		ebea.streams.openInputStreams(detailed, IMStreams, !IO),
		(if
			IMStreams = ok(ok(Streams)),
			Streams = detailed(_, _, _)
		then
			replay(
				Config^parameters^pla^energyPar^energyReproduce,
				Config^parameters^pla^energyPar^oldAge,
				Streams,
				no, no, 0, [],
				!IO),
			ebea.streams.closeInputStreams(Streams, !IO)
		else
			scanable.printNoOkResult("EBEA detailed streams", IMStreams, !IO)
		)
	else
		scanable.printNoOkResult("EBEA configuration file", IMConfig, !IO)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- pred replay(float, int, ebea.streams.inStreams, maybe(tool.replaySim.linePhenotype), maybe(tool.replaySim.lineDeath), int, list(player), io.state, io.state).
:- mode replay(in, in, in(detailed), in, in, in, in, di, uo) is det.

replay(ReproductionThreshold, OldAge, Streams, PreviousLinePhenotype, PreviousLineDeath, Round, PreviousPopulation, !IO) :-
	scanPhenotype(Streams^siphenotype, PreviousLinePhenotype, Round, PreviousPopulation, PopulationTmp, NextPreviousLinePhenotype, !IO),
	scanDeath(Streams^sideath, PreviousLineDeath, Round, PopulationTmp, Population, NextPreviousLineDeath, !IO),
	printPopulation(ReproductionThreshold, OldAge, Round, Population, !IO),
	(
		NextPreviousLinePhenotype = yes(_),
		replay(ReproductionThreshold, OldAge, Streams, NextPreviousLinePhenotype, NextPreviousLineDeath, Round + 1, Population, !IO)
		;
		NextPreviousLinePhenotype = no
	).

:- pred printPopulation(float, int, int, list(player), io.state, io.state).
:- mode printPopulation(in, in, in, in, di, uo) is det.

printPopulation(ReproductionThreshold, OldAge, Round, Population, !IO) :-
	io.print("\033[2J", !IO),  % clear screen
	io.print("\033[;H", !IO),  % move to top of screen
	io.format("Round: %8d\n", [i(Round)], !IO),
	list.foldl2(printPlayer(ReproductionThreshold, OldAge), Population, 0, _, !IO),
	io.print("\n\nPress ENTER to continue", !IO),
	io.read_line_as_string(_, !IO).

:- pred printPlayer(float, int, player, int, int, io.state, io.state).
:- mode printPlayer(in, in, in, in, out, di, uo) is det.

printPlayer(ReproductionThreshold, OldAge, Player, Index, Index + 1, !IO) :-
	Player = player(_, _, _),
	(if
		Index mod 10 = 0
	then
		io.nl(!IO),
		io.nl(!IO)
	else
		true
	),
	io.print("\033[33;44m", !IO),
	io.print(bar(Player^age * maxBarSize * numberSegments / OldAge), !IO),
	io.print("\033[0m", !IO),
	io.print(" ", !IO),
	io.print("\033[37;45m", !IO),
	io.print(bar(float.round_to_int(Player^energy * float(maxBarSize * numberSegments)  / ReproductionThreshold )), !IO),
	io.print("\033[0m", !IO),
	io.print("  ", !IO)
	;
	Player = empty,
	io.print("     ", !IO).

/**
 * Scan a line from the <pre>phenotype.csv</pre> file.  Each line contains
 * four values.  If a line does not contain such data or an IO error
 * occurs, we return a {@code yes(error(_))} instance, which the caller
 * uses to stop reading the file.
  
 */
:- pred scanLinePhenotype(io.input_stream, maybe(maybe_error(tool.replaySim.linePhenotype)), io.state, io.state).
:- mode scanLinePhenotype(in, out, di, uo) is det.

scanLinePhenotype(Stream, MLine, !IO) :-
	io.read_line_as_string(Stream, RLine, !IO),
	(
		RLine = ok(SLine),
		(if
			string.words(SLine) = [SThisRound, SPlayerID, SPlayerEnergy, SPlayerAge],
			string.to_int(SThisRound, Line^round),
			string.to_int(SPlayerID, Line^playerID),
			string.to_float(SPlayerEnergy, Line^playerEnergy),
			string.to_int(SPlayerAge, Line^playerAge)
		then
			MLine = yes(ok(Line))
		else
			MLine = yes(error(string.format("The phenotype.csv file may be corrupted.  The line does not contain three integers and a floating point value representing the round, the player's id, energy and age:\n%s\n", [s(SLine)])))
		)
		;
		RLine = eof,
		MLine = no
		;
		RLine = error(Error),
		MLine = yes(error(string.format("IO error occurred while reading phenotype.csv file:\n%s\n", [s(io.error_message(Error))])))
	).
	

/**
 * Scan the phenotype file to create the current population.  Each line
 * contains the round number, the player ID, its energy and age.  We stop
 * reading when the round number is different from parameter {@code Round}
  
 */
:- pred scanPhenotype(io.input_stream, maybe(tool.replaySim.linePhenotype), int, list(player), list(player), maybe(tool.replaySim.linePhenotype), io.state, io.state).
:- mode scanPhenotype(in, in, in, in, out, out, di, uo) is det.

scanPhenotype(Stream, PreviousLine, Round, !Population, NextPreviousLine, !IO) :-
	(
		PreviousLine = no,
		scanLinePhenotype(Stream, MThisLine, !IO)
		;
		PreviousLine = yes(Line),
		MThisLine = yes(ok(Line))
	),
	(
		MThisLine = yes(ok(ThisLine)),
		(if
			ThisLine^round = Round
		then
			Player = player(ThisLine^playerID, ThisLine^playerEnergy, ThisLine^playerAge),
			findSpot(ThisLine^playerID, !.Population, PartA, PartB),
			list.append(PartA, [Player | PartB]) = Pop,
			scanPhenotype(Stream, no, Round, Pop, !:Population, NextPreviousLine, !IO)
		else
			NextPreviousLine = yes(ThisLine)
		)
		;
		MThisLine = yes(error(Message)),
		io.print(io.stderr_stream, Message, !IO),
		NextPreviousLine = no
		;
		MThisLine = no,
		NextPreviousLine = no
	).

/**
 * Scan a line from the <pre>death.csv</pre> file.  Each line contains
 * four values.  If a line does not contain such data or an IO error
 * occurs, we return a {@code yes(error(_))} instance, which the caller
 * uses to stop reading the file.
  
 */
:- pred scanLineDeath(io.input_stream, maybe(maybe_error(tool.replaySim.lineDeath)), io.state, io.state).
:- mode scanLineDeath(in, out, di, uo) is det.

scanLineDeath(Stream, MLine, !IO) :-
	io.read_line_as_string(Stream, RLine, !IO),
	(
		RLine = ok(SLine),
		(if
			string.words(SLine) = [SThisRound, SPlayerID],
			string.to_int(SThisRound, Line^dround),
			string.to_int(SPlayerID, Line^dplayerID)
		then
			MLine = yes(ok(Line))
		else
			MLine = yes(error(string.format("The death.csv file may be corrupted.  The line does not contain three integers and a floating point value representing the round, the player's id, energy and age:\n%s\n", [s(SLine)])))
		)
		;
		RLine = eof,
		MLine = no
		;
		RLine = error(Error),
		MLine = yes(error(string.format("IO error occurred while reading death.csv file:\n%s\n", [s(io.error_message(Error))])))
	).
	

/**
 * Scan the death file to create the current population.  Each line
 * contains the round number and the player ID.  We stop
 * reading when the round number is different from parameter {@code Round}
  
 */
:- pred scanDeath(io.input_stream, maybe(tool.replaySim.lineDeath), int, list(player), list(player), maybe(tool.replaySim.lineDeath), io.state, io.state).
:- mode scanDeath(in, in, in, in, out, out, di, uo) is det.

scanDeath(Stream, PreviousLine, Round, !Population, NextPreviousLine, !IO) :-
	(
		PreviousLine = no,
		scanLineDeath(Stream, MThisLine, !IO)
		;
		PreviousLine = yes(Line),
		MThisLine = yes(ok(Line))
	),
	(
		MThisLine = yes(ok(ThisLine)),
		(if
			ThisLine^dround = Round
		then
			clearSpot(ThisLine^dplayerID, !Population),
			scanDeath(Stream, no, Round, !Population, NextPreviousLine, !IO)
		else
			NextPreviousLine = yes(ThisLine)
		)
		;
		MThisLine = yes(error(Message)),
		io.print(io.stderr_stream, Message, !IO),
		NextPreviousLine = no
		;
		MThisLine = no,
		NextPreviousLine = no
	).

:- pred clearSpot(int, list(player), list(player)).
:- mode clearSpot(in, in, out) is det.

clearSpot(_, [], []).

clearSpot(ID, [Player | RestPartA], Result) :-
	(if
		Player^id = ID
	then
		Result = [empty | RestPartA]
	else
		Result = [Player | RestResult],
		clearSpot(ID, RestPartA, RestResult)
	).

/**
 * Find a spot in list to place the information about the player with the
 * given identification.  Either we replace the previous information or we
 * replace an empty value or we insert at the end.
  
 */
:- pred findSpot(int, list(player), list(player), list(player)).
:- mode findSpot(in, in, out, out) is det.

findSpot(ID, List, PartA, PartB) :-
	(if
		findSpotID(ID, List, A, B)
	then
		A = PartA,
		B = PartB
	else
		findSpotEmpty(List, PartA, PartB)
	).

:- pred findSpotID(int, list(player), list(player), list(player)).
:- mode findSpotID(in, in, out, out) is semidet.

findSpotID(ID, List, PartA, PartB) :-
	List = [Player | RestList],
	(if
		Player^id = ID
	then
		PartA = [],
		PartB = RestList
	else
		PartA = [Player | RestPartA],
		findSpotID(ID, RestList, RestPartA, PartB)
	).

:- pred findSpotEmpty(list(player), list(player), list(player)).
:- mode findSpotEmpty(in, out, out) is det.

findSpotEmpty(List, PartA, PartB) :-
	List = [],
	PartA = [],
	PartB = []
	;
	List = [Player | RestList],
	(if
		Player = empty
	then
		PartA = [],
		PartB = RestList
	else
		PartA = [Player | RestPartA],
		findSpotEmpty(RestList, RestPartA, PartB)
	).

:- func bar(int) = string.

bar(V) = Result :-
	R = V rem numberSegments,
	Q = V  /  numberSegments,
	(if
		Q >= maxBarSize
	then
		Result = duplicate(seg(numberSegments), maxBarSize)
	else
		Result = duplicate(seg(numberSegments), Q) ++ seg(R) ++ string.duplicate_char(' ', maxBarSize - Q - 1)
		% Middle = string.char_to_string(det_index(bars, R)),
		% Result = string.pad_left(string.pad_right(Middle, ' ', maxBarSize - Q), string.det_index(bars, 8), Q)
	).

:- func duplicate(string, int) = string.

duplicate(String, Qty) = Result :-
	(if
		Qty =< 0
	then
		Result = ""
	else
		Result = String ++ duplicate(String, Qty - 1)
	).

:- func maxBarSize = int.

maxBarSize = 2.

:- func numberSegments = int.

numberSegments = 8.

:- func seg(int) = string.

seg(Num) = Result :-
	(if
		seg(Num, R)
	then
		Result = R
	else
		throw("seg/1: invalid parameter")
	).

:- pred seg(int, string).
:- mode seg(in, out) is semidet.

seg(0, " ").
seg(1, "▏").
seg(2, "▎").
seg(3, "▍").
seg(4, "▌").
seg(5, "▋").
seg(6, "▊").
seg(7, "▉").
seg(8, "█").

:- end_module tool.replaySim.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:



