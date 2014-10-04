/**
 * Provides types that represent birth data stored in binary format.  The
 * stream with birth data is composed of records.  Each record represents
 * the births occurred during an iteration.  If there was no births in an
 * iteration, nothing is written.  The record contains the iteration number
 * and a list with data of where the birth occurred (site index), player
 * key and chromosome.

 * @author Pedro Mariano
 * @version 1.0 2014/03/08
 */
:- module ebea.streams.birth.

:- interface.

:- import_module ebea.player, ebea.player.chromosome, ebea.population, ebea.population.players.
:- import_module parseable.iou.
:- import_module foldable.

:- type iterationBirthRecords(C) --->
	ibr(
		iteration :: int,
		list(playerBirthRecord(C))
	).

:- type playerBirthRecord(C) --->
	pbr(
		id         :: key,
		siteIndex  :: int,
		chromosome :: chromosome(C)
	).

:- instance parseable(iterationBirthRecords(C)) <= parseable(C).

/**
 * read(Stream, Iteration, DIResult, !AdvancedResult, !Cache, !IO)
  
 */
:- pred read(
	io.binary_input_stream, int,
	parseable.iou.delayedResult(io.result(iterationBirthRecords(C))),
	parseable.iou.advancedResult(iterationBirthRecords(C)), parseable.iou.advancedResult(iterationBirthRecords(C)),
	parseable.iou.cache, parseable.iou.cache,
	io.state, io.state
	)
	<= parseable(C).
:- mode read(in, in, out,  in, out,  in, out,  di, uo) is det.

/**
 * Write the information about the players that born in the given
 * iteration.  If there are no births, nothing is written.
  
 */
:- pred write(io.binary_output_stream, int, list(ebea.player.player(C, T)), io.state, io.state)
	<= parseable(C).
:- mode write(in, in, in, di, uo) is det.

/**
 * Write the information about the players that born in the given
 * iteration.  If there are no births, nothing is written.
  
 */
:- func foldInit(ebea.player.player(C, T), list(playerBirthRecord(C))) = list(playerBirthRecord(C)).

:- pred writeInit(io.binary_output_stream, list(playerBirthRecord(C)), io.state, io.state)
	<= parseable(C).
:- mode writeInit(in, in, di, uo) is det.


/**
 * Reads the entire stream into a list.
 */
:- pred read(ebea.streams.inStreams, parseable.iou.ioResult(list(iterationBirthRecords(C))), io.state, io.state)
	<= parseable(C).
:- mode read(in(detailedBin), out, di, uo) is det.

/**
 * Fold all players' chromosome.
 */
:- func foldlAll(list(playerBirthRecord(C))) = ebea.player.ac(A)
	<= foldable(C, A).

/**
 * Fold all players' chromosome that were born at the given site.
 */
:- func foldlSite(list(playerBirthRecord(C)), int) = ebea.player.ac(A)
	<= foldable(C, A).

:- pred parse(iterationBirthRecords(C), list(int), list(int))
	<= parseable(C).
:- mode parse(in, out, in) is det.
:- mode parse(out, in, out) is semidet.

/**
 * search(PlayerID, ListPlayerBirthRecord, Result)

 * Search the list for the record of the given player identification.
 * Fails if does not found.
  
 */
:- pred search(key, list(playerBirthRecord(C)), playerBirthRecord(C)).
:- mode search(in, in, out) is semidet.

:- pred work_search(key, list(playerBirthRecord(C)), playerBirthRecord(C)).
:- mode work_search(in, in, out) is semidet.

/**
 * Return the number of births at the given site.
 */
:- func birthsAtSite(list(playerBirthRecord(C)), int) = int.

:- pred table_reset_for_search_3(io.state, io.state).
:- mode table_reset_for_search_3(di, uo) is det.

:- implementation.

:- import_module int.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

:- instance parseable(iterationBirthRecords(C)) <= parseable(C) where
[
	pred(parse/3) is ebea.streams.birth.parse
].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

:- instance parseable(playerBirthRecord(C)) <= parseable(C) where
[
	pred(parse/3) is parse_playerBirthRecord
].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

read(Stream, Iteration, DIResult, !MAdvancedResult, !Cache, !IO) :-
	!.MAdvancedResult = no,
	parseable.iou.read(Stream, 4096, no, !Cache, MIResult, !IO),
%	io.format("At %d read %s\n", [i(Iteration), s(string(MIResult))], !IO),
	(
		MIResult = ok(ok(AResult)),
		read(Stream, Iteration, DIResult, result(AResult), !:MAdvancedResult, !Cache, !IO)
		;
		MIResult = ok(eof),
		!:MAdvancedResult = eof,
		DIResult = ok(eof)
		;
		MIResult = ok(error(Error)),
		DIResult = ok(error(Error))
		;
		MIResult = parseError,
		DIResult = parseError
	)
	;
	!.MAdvancedResult = result(AdvancedResult),
%	io.format("Checking %d against %s\n", [i(Iteration), s(string(AdvancedResult))], !IO),
	(if
		AdvancedResult^iteration = Iteration
	then
		DIResult = ok(ok(AdvancedResult)),
		!:MAdvancedResult = no
	else
		DIResult = delayed
	)
	;
	!.MAdvancedResult = eof,
	DIResult = ok(eof)
	.

write(Stream, Iteration, Births, !IO) :-
	Births = []
	;
	Births = [_|_],
	list.map(map_playerBirthRecord, Births) = List,
	parseable.iou.write(Stream, ibr(Iteration, List), !IO)
	% parse(ibr(Iteration, List), Bytes, []),
	% list.foldl(io.write_byte(Stream), Bytes, !IO)
	.

foldInit(Player, AC) = [map_playerBirthRecord(Player) | AC].

writeInit(Stream, List, !IO) :-
	parseable.iou.write(Stream, ibr(-1, List), !IO).

read(BinStream, Result, !IO) :-
	parseable.iou.readAll(BinStream^bisBirth, 4096, no, Result, !IO).

foldlAll(List) = Result :-
	Fold =
	(func(PBR, AC) = R :-
		ebea.player.chromosome.fold(PBR^chromosome, AC) = R
	),
	Result = list.foldl(Fold, List, ebea.player.initAc).

foldlSite(List, SiteIndex) = Result :-
	Fold =
	(func(PBR, AC) = R :-
		(if
			SiteIndex = PBR^siteIndex
		then
			ebea.player.chromosome.fold(PBR^chromosome, AC) = R
		else
			R = AC
		)
	),
	Result = list.foldl(Fold, List, ebea.player.initAc).

parse(ibr(Iteration, PlayerBirthRecords)) -->
	parseable.int32(Iteration),
	parseable.parseList(withLength, PlayerBirthRecords).

search(ID, L, R) :-
	memo_search(ID, L, R).

table_reset_for_search_3(!IO) :-
	table_reset_for_memo_search_3(!IO).

birthsAtSite([], _) = 0.
birthsAtSite([Record | Rest], SiteIndex) =
	(if
		Record^siteIndex = SiteIndex
	then
		1
	else
		0
	) + birthsAtSite(Rest, SiteIndex).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- pragma memo(memo_search/3, [fast_loose, allow_reset]).

:- pred memo_search(ebea.population.players.key, list(playerBirthRecord(C)), playerBirthRecord(C)).
:- mode memo_search(in, in, out) is semidet.

memo_search(ID, L, R) :-
	work_search(ID, L, R).

%:- pred work_search(int, list(playerBirthRecord(C)), playerBirthRecord(C)).
%:- mode work_search(in, in, out) is semidet.

work_search(ID, [H | T], R) :-
	(if
		H^id = ID
	then
		R = H
	else
		work_search(ID, T, R)
	).

:- func map_playerBirthRecord(player(C, T)) = playerBirthRecord(C).

map_playerBirthRecord(player(ID, SiteIndex, Chromosome, _Traits)) = pbr(ID, SiteIndex, Chromosome).

:- pred parse_playerBirthRecord(playerBirthRecord(C), list(int), list(int))
	<= parseable(C).
:- mode parse_playerBirthRecord(in, out, in) is det.
:- mode parse_playerBirthRecord(out, in, out) is semidet.

parse_playerBirthRecord(pbr(ID, SiteIndex, Chromosome)) -->
	ebea.population.players.parseKey(ID),
	parseable.int32(SiteIndex),
	ebea.player.chromosome.parse(Chromosome).

:- end_module ebea.streams.birth.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
