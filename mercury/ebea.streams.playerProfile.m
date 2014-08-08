/**
 * Provides a type that represents the information stored in {@code
 * player-profile.dat} file.  This file contains information about the
 * player profiles in all iterations.

 * @author Pedro Mariano
 * @version 1.0 2014/03/ 9
 */
:- module ebea.streams.playerProfile.

:- interface.

:- import_module ebea.population, ebea.population.players.
:- import_module game.
:- import_module parseable.iou.

:- type iterationPlayerProfileRecords --->
	ippr(
		iteration :: int,
		profiles  :: list(list(key))
	).

:- pred read(io.binary_input_stream, G, parseable.iou.cache, parseable.iou.cache, parseable.iou.result(io.result(iterationPlayerProfileRecords)), io.state, io.state)
	<= asymmetricGame(G, C).
:- mode read(in, in, in, out, out, di, uo) is det.

/**
 * Reads the entire stream into a list.
 */
:- pred read(ebea.streams.inStreams, G, parseable.iou.ioResult(list(iterationPlayerProfileRecords)), io.state, io.state)
	<= asymmetricGame(G, C).
:- mode read(in(detailedBin), in, out, di, uo) is det.

:- pred write(io.binary_output_stream, G, int, list(list(key)), io.state, io.state)
	<= asymmetricGame(G, C).
:- mode write(in, in, in, in, di, uo) is det.

:- pred parse(int, iterationPlayerProfileRecords, parseable.state, parseable.state).
:- mode parse(in, in, out, in) is det.
:- mode parse(in, out, in, out) is semidet.


:- implementation.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

read(Stream, Game, !Cache, RIResult, !IO) :-
%	io.format("\nNumber of players %d for game %s\n", [i(numberPlayers(Game)), s(string(Game))], !IO),
	PredParser =
	(pred(I::out, LB1::in, LB2::out) is semidet :-
		ebea.streams.playerProfile.parse(numberPlayers(Game), I, LB1, LB2)
	),
	parseable.iou.read(Stream, 4096, no, PredParser, !Cache, RIResult, !IO).

read(BinStream, Game, Result, !IO) :-
	PredParser =
	(pred(I::out, LB1::in, LB2::out) is semidet :-
		ebea.streams.playerProfile.parse(numberPlayers(Game), I, LB1, LB2)
	),
	parseable.iou.readAll(BinStream^bisPlayerProfile, 4096, no, PredParser, Result, !IO).

write(Stream, Game, Iteration, PlayerProfiles, !IO) :-
	Data = ippr(Iteration, PlayerProfiles),
	parse(numberPlayers(Game), Data, Bytes, []),
	list.foldl(io.write_byte(Stream), Bytes, !IO).

parse(NumberPlayers, ippr(Iteration, PlayerProfiles)) -->
	parseable.int32(Iteration),
	parsePlayerProfiles(NumberPlayers, PlayerProfiles)
%	parseable.parseList(withLength, parseable.parseList(onlyElements(NumberPlayers)), PlayerProfiles)
	.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- pred parsePlayerProfiles(int, list(list(key)), parseable.state, parseable.state).
:- mode parsePlayerProfiles(in, in, out, in) is det.
:- mode parsePlayerProfiles(in, out, in, out) is semidet.

:- pragma promise_pure(parsePlayerProfiles/4).

parsePlayerProfiles(NumberPlayers::in, PlayerProfiles::in, L1::out, L2::in) :-
	parseable.parseList(withLength, parsePlayerProfileD(NumberPlayers), PlayerProfiles, L1, L2)
	.

parsePlayerProfiles(NumberPlayers::in, PlayerProfiles::out, L1::in, L2::out) :-
	parseable.parseList(withLength, parsePlayerProfileS(NumberPlayers), PlayerProfiles, L1, L2)
	.


:- pred parsePlayerProfileD(int, list(key), parseable.state, parseable.state).
:- mode parsePlayerProfileD(in, in, out, in) is det.

parsePlayerProfileD(NumberPlayers, PlayerProfile) -->
	parseable.parseList(onlyElements(NumberPlayers), PlayerProfile)
	.

:- pred parsePlayerProfileS(int, list(key), parseable.state, parseable.state).
:- mode parsePlayerProfileS(in, out, in, out) is semidet.

parsePlayerProfileS(NumberPlayers, PlayerProfile) -->
	parseable.parseList(onlyElements(NumberPlayers), PlayerProfile)
	.


:- end_module ebea.streams.playerProfile.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
