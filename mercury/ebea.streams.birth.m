/**
 * 

 * @author Pedro Mariano
 * @version 1.0 2014/03/ 8
 */
:- module ebea.streams.birth.

:- interface.

:- import_module ebea.player.
:- import_module game.
:- import_module parseable.iou.

:- type iterationBirthRecords(C) --->
	ibr(
		iteration :: int,
		list(playerBirthRecord(C))
	).

:- type playerBirthRecord(C) --->
	pbr(
		id         :: int,
		siteIndex  :: int,
		chromosome :: ebea.player.chromosome(C)
	).

:- instance parseable(iterationBirthRecords(C)) <= parseable(C).

/**
 * read(Stream, Iteration, DIResult, !AdvancedResult, !Cache, !IO)
  
 */
:- pred read(
	io.binary_input_stream, int,
	parseable.iou.delayedResult(io.result(iterationBirthRecords(C))),
	maybe(iterationBirthRecords(C)), maybe(iterationBirthRecords(C)),
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


% /**
%  * Read the birth records that correspond to the initial population.  The
%  * predicate returns the advanced result and the cache that should be used
%  * in calls to predicate {@code read/9}.
%  */
% :- pred readInitialPopulation(
% 	io.binary_input_stream,
% 	G,
% 	parseable.iou.delayedResult(io.result(iterationBirthRecords(C))),
% 	maybe(iterationBirthRecords(C)),
% 	parseable.iou.cache,
% 	io.state, io.state
% 	)
% 	<= (asymmetricGame(G, C), parseable(C)).
% :- mode readInitialPopulation(in, in, out, out, out, di, uo).

:- pred parse(iterationBirthRecords(C), list(int), list(int))
	<= parseable(C).
:- mode parse(in, out, in) is det.
:- mode parse(out, in, out) is semidet.

/**
 * search(PlayerID, ListPlayerBirthRecord, Result)

 * Search the list for the record of the given player identification.
 * Fails if does not found.
  
 */
:- pred search(int, list(playerBirthRecord(C)), playerBirthRecord(C)).
:- mode search(in, in, out) is semidet.


:- implementation.

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
	!.MAdvancedResult = yes(AdvancedResult),
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
	!.MAdvancedResult = no,
	parseable.iou.read(Stream, 4096, no, !Cache, MIResult, !IO),
%	io.format("At %d read %s\n", [i(Iteration), s(string(MIResult))], !IO),
	(
		MIResult = ok(ok(AResult)),
		read(Stream, Iteration, DIResult, yes(AResult), !:MAdvancedResult, !Cache, !IO)
		;
		MIResult = ok(eof),
		DIResult = ok(eof)
		;
		MIResult = ok(error(Error)),
		DIResult = ok(error(Error))
		;
		MIResult = parseError,
		DIResult = parseError
	).

write(Stream, Iteration, Births, !IO) :-
	Births = []
	;
	Births = [_|_],
	list.map(map_playerBirthRecord, Births) = List,
	parseable.iou.write(Stream, ibr(Iteration, List), !IO)
	% parse(ibr(Iteration, List), Bytes, []),
	% list.foldl(io.write_byte(Stream), Bytes, !IO)
	.

% readInitialPopulation(Stream, Game, DIResult, MAdvancedResult, Cache, !IO) :-
% 	read(Stream, -1, DIResult, no, MAdvancedResult, parseable.iou.cacheInit, Cache, !IO).

parse(ibr(Iteration, PlayerBirthRecords)) -->
	parseable.int32(Iteration),
	parseable.parseList(withLength, PlayerBirthRecords).

search(ID, [H | T], R) :-
	(if
		H^id = ID
	then
		R = H
	else
		search(ID, T, R)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- func map_playerBirthRecord(player(C, T)) = playerBirthRecord(C).

map_playerBirthRecord(player(ID, SiteIndex, Chromosome, _Traits)) = pbr(ID, SiteIndex, Chromosome).

:- pred parse_playerBirthRecord(playerBirthRecord(C), list(int), list(int))
	<= parseable(C).
:- mode parse_playerBirthRecord(in, out, in) is det.
:- mode parse_playerBirthRecord(out, in, out) is semidet.

parse_playerBirthRecord(pbr(ID, SiteIndex, Chromosome)) -->
	parseable.int32(ID),
	parseable.int32(SiteIndex),
	ebea.player.parseChromosome(Chromosome).

:- end_module ebea.streams.birth.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
