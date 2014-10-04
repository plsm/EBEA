/**
 * Provides a type that represents the data stored in file {@code death}.
 * This file holds information about player deaths, namely iteration number
 * and player index.

 * @author Pedro Mariano
 * @version 1.0 2014/03/ 8
 */
:- module ebea.streams.death.

:- interface.

:- import_module ebea.streams.birth, ebea.population, ebea.population.players.
:- import_module parseable.iou.

:- type iterationDeathRecords --->
	idr(
		iteration        :: int,
		carryingCapacity :: list(key),
		oldAge           :: list(key),
		starvation       :: list(key)
	).

:- instance parseable(iterationDeathRecords).

/**
 * read(Stream, Iteration, DIResult, !AdvancedResult, !Cache, !IO)
  
 */
:- pred read(
	io.binary_input_stream, int,
	parseable.iou.delayedResult(io.result(iterationDeathRecords)),
	parseable.iou.advancedResult(iterationDeathRecords), parseable.iou.advancedResult(iterationDeathRecords),
	parseable.iou.cache, parseable.iou.cache,
	io.state, io.state
	).
:- mode read(in, in, out,  in, out,  in, out,  di, uo) is det.

/**
 * Reads the entire stream into a list.
 */
:- pred read(ebea.streams.inStreams, parseable.iou.ioResult(list(iterationDeathRecords)), io.state, io.state).
:- mode read(in(detailedBin), out, di, uo) is det.

:- pred write(io.binary_output_stream, int, list(key), list(key), list(key), io.state, io.state).
:- mode write(in, in, in, in, in, di, uo) is det.

:- pred parse(iterationDeathRecords, parseable.state, parseable.state).
:- mode parse(in, out, in) is det.
:- mode parse(out, in, out) is semidet.

/**
 * Return the number of deaths that occurred at the given site.  The first
 * argument is the list of all births that have occurred.
  
 */
:- func deathsAtSite(list(playerBirthRecord(C)), list(key), int) = int.

:- implementation.

:- import_module parseable.iou.
:- import_module int.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

:- instance parseable(iterationDeathRecords) where
[
	pred(parse/3) is ebea.streams.death.parse
].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

read(Stream, Iteration, DIResult, !MAdvancedResult, !Cache, !IO) :-
	!.MAdvancedResult = no,
	parseable.iou.read(Stream, 4096, no, !Cache, MIResult, !IO),
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

read(BinStream, Result, !IO) :-
	parseable.iou.readAll(BinStream^bisDeath, 4096, no, Result, !IO).

write(Stream, Iteration, CemeteryCarryingCapacity, CemeteryOldAge, CemeteryStarvation, !IO) :-
	(if
		CemeteryCarryingCapacity = [],
		CemeteryOldAge = [],
		CemeteryStarvation = []
	then
		true
	else
		parseable.iou.write(Stream, idr(Iteration, CemeteryCarryingCapacity, CemeteryOldAge, CemeteryStarvation), !IO)
	).

parse(idr(Iteration, CarryingCapacity, OldAge, Starvation)) -->
	parseable.int32(Iteration),
	parseable.parseList(withLength, CarryingCapacity),
	parseable.parseList(withLength, OldAge),
	parseable.parseList(normalType, Starvation).

deathsAtSite(_, [], _) = 0.
deathsAtSite(BirthRecords, [PlayerID | Rest], SiteIndex) =
	(if
		ebea.streams.birth.search(PlayerID, BirthRecords, R),
		R^siteIndex = SiteIndex
	then
		1
	else
		0
	) + deathsAtSite(BirthRecords, Rest, SiteIndex)
	.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- end_module ebea.streams.death.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
