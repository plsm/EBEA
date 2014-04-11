/**
 * Provides a type that represents the information stored in {@code
 * phenotype.dat} file.  This file holds information about players'
 * phenotypic trait.

 * @author Pedro Mariano
 * @version 1.0 2014/03/ 8
 */
:- module ebea.streams.phenotype.

:- interface.

:- import_module ebea.population.
:- import_module ebea.player, ebea.player.age, ebea.player.energy, ebea.player.selection.
:- import_module parseable.iou.

:- type iterationPhenotypicRecords --->
	ipr(
		iteration :: int,
		list(playerPhenotypicRecord)
	).

/**
 * A player phenotypic record of some iteration of an EBEA run.
  
 * @cons ppr(ID, Age, Energy, Selection) Where {@code ID} represents
 * player's identification, {@code Age} is its age, {@code Energy} is the
 * energetic traits and {@code Selection} is the selection traits.
  
 */
:- type playerPhenotypicRecord --->
	ppr(
		id        :: int,
		age       :: ebea.player.age.trait,
		energy    :: ebea.player.energy.trait,
		selection :: ebea.player.selection.traits
	).

:- instance parseable(iterationPhenotypicRecords).

/**
 * Reads bytes from the binary stream and parse them to return a {@code
 * iterationPhenotypicRecords} instance.
 */

:- pred read(
	io.binary_input_stream :: in,
	parseable.iou.result(io.result(iterationPhenotypicRecords)) :: out,
	parseable.iou.cache    :: in, parseable.iou.cache :: out,
	io.state               :: di, io.state            :: uo
	) is det.

:- pred write(io.binary_output_stream, int, ebea.population.population(C, T), io.state, io.state).
:- mode write(in, in, in, di, uo) is det.

:- pred parse(iterationPhenotypicRecords, list(int), list(int)).
:- mode parse(in, out, in) is det.
:- mode parse(out, in, out) is semidet.

/**
 * search(PlayerID, ListPlayerBirthRecord, Result)

 * Search the list for the record of the given player identification.
 * Fails if does not found.
  
 */
:- pred search(int, list(playerPhenotypicRecord), playerPhenotypicRecord).
:- mode search(in, in, out) is semidet.


:- implementation.

:- import_module parseable.iou.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

:- instance parseable(iterationPhenotypicRecords) where
[
	pred(parse/3) is ebea.streams.phenotype.parse
].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

:- instance parseable(playerPhenotypicRecord) where
[
	pred(parse/3) is parse_playerPhenotypicRecord
].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

read(Stream, RIResult, !Cache, !IO) :-
	parseable.iou.read(Stream, 4096, no, !Cache, RIResult, !IO).
%	parseable.iou.read(Stream, 4096, no, ebea.streams.phenotype.parse, !Cache, RIResult, !IO).

write(Stream, Iteration, Population, !IO) :-
	List = ebea.population.transform_player(map_PlayerPhenotypicRecord, Population),
	parseable.iou.write(Stream, ipr(Iteration, List), !IO)
	.

parse(ipr(Iteration, ListPlayerPhenotypicRecords)) -->
	parseable.int32(Iteration),
	parseable.parseList(withLength, ListPlayerPhenotypicRecords).

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

:- func map_PlayerPhenotypicRecord(player(C, T)) = playerPhenotypicRecord.

map_PlayerPhenotypicRecord(player(ID, _SiteIndex, _Chromosome, Traits)) = ppr(ID, Traits^ageTrait, Traits^energyTrait, Traits^selectionTrait).

:- pred parse_playerPhenotypicRecord(playerPhenotypicRecord, list(int), list(int)).
:- mode parse_playerPhenotypicRecord(in, out, in) is det.
:- mode parse_playerPhenotypicRecord(out, in, out) is semidet.

parse_playerPhenotypicRecord(ppr(ID, Age, Energy, Selection)) -->
	parseable.int32(ID),
	ebea.player.age.parseTrait(Age),
	ebea.player.energy.parseTrait(Energy),
	ebea.player.selection.parseTraits(Selection)
	.

:- end_module ebea.streams.phenotype.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
