/**
 * Provides a type that represents the data stored in file {@code death}.
 * This file holds information about player deaths, namely iteration number
 * and player index.

 * @author Pedro Mariano
 * @version 1.0 2014/03/ 8
 */
:- module ebea.streams.death.

:- interface.

:- type iterationDeathRecords --->
	idr(
		iteration        :: int,
		carryingCapacity :: list(int),
		oldAge           :: list(int),
		starvation       :: list(int)
	).

:- instance parseable(iterationDeathRecords).

:- pred write(io.binary_output_stream, int, list(int), list(int), list(int), io.state, io.state).
:- mode write(in, in, in, in, in, di, uo) is det.

:- pred parse(iterationDeathRecords, parseable.state, parseable.state).
:- mode parse(in, out, in) is det.
:- mode parse(out, in, out) is semidet.

:- implementation.

:- import_module parseable.iou.

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- end_module ebea.streams.death.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
