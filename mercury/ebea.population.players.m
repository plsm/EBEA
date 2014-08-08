/**
 * This module provides a type that represents how players are stored in a
 * population.  This module also provides predicates to iterate through the
 * players in a population or to map them.

 * @author Pedro Mariano
 * @version 1.0 2013/06/25
 */
:- module ebea.population.players.

:- interface.

:- import_module ebea.player.
:- import_module parseable.
:- import_module list.

/**
 * Represents all the players in the population.
 */
:- type players(C, T).

/**
 * Players are indexed by a key.
 */
:- type key.% == int.

:- instance parseable(key).

:- func int2key(int) = key.
:- func key2int(key) = int.

:- pred parseKey(key, parseable.state, parseable.state).
:- mode parseKey(in, out, in) is det.
:- mode parseKey(out, in, out) is semidet.

/**
 * Initialise the players of some population.
 */
:- pred init(players(C, T), key).
:- mode init(out, out) is det.

/**
 * Insert the given player to the population players.
 */
:- pred insert(player(C, T), key, key, players(C, T), players(C, T)).
:- mode insert(in, in, out, in, out) is det.

:- func append(players(C, T), list(player(C, T))) = players(C, T).

/**
 * Return the number of players in a population.
 */
:- func size(players(C, T)) = int.

/**
 * Retrieve the player with the given key from the population.  Throws an
 * exception if there is no such player.
  
 */
:- func player(players(C, T), key) = player(C, T).

:- pred player(players(C, T), key, player(C, T)).
:- mode player(in, out, out) is nondet.

/**
 * fold(Closure, Players, AC) = Result
 *
 * Apply the given closure to all players of some population and reduce them
 * to {@code Result}.

*/
:- func fold(func(player(C, P), A) = A, players(C, P), A) = A.

:- func map(func(player(C, P)) = player(C, P), players(C, P)) = players(C, P).

:- func transform(func(player(C, P)) = A, players(C, P)) = list(A).

/**
 * fold(Closure, Players, !AC)
 *
 * Apply the given closure to all players of some population and reduce them
 * to {@code Result}.

*/
:- pred fold(pred(player(C, P), A, A), players(C, P), A, A).
:- mode fold(in(pred(in, in, out) is det), in, in, out) is det.
:- mode fold(in(pred(in, di, uo)  is det), in, di, uo)  is det.

:- pred fold3(pred(player(C, P), T1, T1, T2, T2, T3, T3), players(C, P), T1, T1, T2, T2, T3, T3).
:- mode fold3(in(pred(in, in, out, in, out, in, out) is det), in, in, out, in, out, in, out) is det.
:- mode fold3(in(pred(in, in, out, in, out, di,  uo) is det), in, in, out, in, out, di,  uo) is det.

:- pred filterFold4(pred(player(C, P), bool, T1, T1, T2, T2, T3, T3, T4, T4), players(C, P), players(C, P), T1, T1, T2, T2, T3, T3, T4, T4).
:- mode filterFold4(in(pred(in, out, in, out, in, out, in, out, in, out) is det), in, out, in, out, in, out, in, out, in, out) is det.
:- mode filterFold4(in(pred(in, out, in, out, in, out, in, out, di,  uo) is det), in, out, in, out, in, out, in, out, di,  uo) is det.
%:- mode filterFold4(in(pred(in, out, in, out, in, out, in, out, in, out) is det), di,  uo, in, out, in, out, in, out, in, out) is det.
%:- mode filterFold4(in(pred(in, out, in, out, in, out, in, out, di,  uo) is det), di,  uo, in, out, in, out, in, out, di,  uo) is det.


:- pred mapFold(
	pred(player(C, T), player(C, T),  T1, T1) :: in(pred(in, out, in, out) is det),
	players(C, T) :: in, players(C, T) :: out,
	T1            :: in, T1            :: out
)
	is det.

:- pred mapFold4(
	pred(player(C, T), player(C, T),  T1, T1, T2, T2, T3, T3, T4, T4) :: in(pred(in, out, in, out, in, out, in, out, in, out) is det),
	players(C, T) :: in, players(C, T) :: out,
	T1            :: in, T1            :: out,
	T2            :: in, T2            :: out,
	T3            :: in, T3            :: out,
	T4            :: in, T4            :: out
)
	is det.

:- pred nextKey(key, key).
:- mode nextKey(in, out) is det.

% /**
%  * Given a list of players, return a random list of player's identification.

%  * TODO: Should be in ebea.population.players or ebea.population.neighbours
%  */
% :- pred randomIDs(int, list(int), list(player(C, T)), R, R)
% 	<= ePRNG(R).
% :- mode randomIDs(in, out, in, in, out) is det.

/**
 * update(ID, UpdateFunc, !Players)
  
 * Given a list of players, update the one with the given identification,
 * using the provided function.  The player to be updated is passed to the
 * function.
  

 * TODO: Should be in ebea.population.players or ebea.population.neighbours
 */
:- pred update(key, func(player(C, T)) = player(C, T), players(C, T), players(C, T)).
:- mode update(in, in, in, out) is det.

/**
 * player(Players, ID) = Result

 * Given a list of players and an identification, return the player with
 * that identification.  Throws an exception if there is no such player.
  
 * TODO: Should be in ebea.population.players or ebea.population.neighbours
 */
%:- func player(list(player(C, T)), int) = player(C, T).



:- implementation.

:- import_module map, int.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

:- type players(C, T) == map(key, player(C, T)).

%:- type key == int.
:- type key --->
	key(key :: int).

:- instance parseable(key) where
[
	pred(parse/3) is ebea.population.players.parseKey
].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

key2int(key(KI)) = KI.
int2key(IK) = key(IK).

parseKey(key(Key)) -->
	parseable.int32(Key).

init(map.init, key(0)).

size(Players) = map.count(Players).

:- pragma memo(player/2, [fast_loose, allow_reset]).

player(Players, Key) = Result :-
	map.lookup(Players, Key) = Result.

player(Players, Key, Player) :-
	map.member(Players, Key, Player).

append(Players, []) = Players.
append(Players, [Player | Rest]) = append(map.det_insert(Players, Player^id, Player), Rest).

insert(APlayer, !Key, !Players) :-
	(if
		APlayer^id = !.Key
	then
		map.det_insert(APlayer^id, APlayer, !Players),
		!:Key = key(!.Key^key + 1)
	else
		throw("ebea.population.players.insert/5: invalid player ID")
	).

fold(Func, Players, AC) = Result :-
	Pred =
	(pred(V::in, !.AC::in, !:AC::out) is det :-
		!:AC = Func(V, !.AC)
	),
	map.foldl_values(Pred, Players, AC, Result).

fold(Pred, Players, !AC) :-
	map.foldl_values(Pred, Players, !AC).

fold3(Pred, Players, !AC1, !AC2, !AC3) :-
	map.foldl3(withKey_fold3(Pred), Players, !AC1, !AC2, !AC3).
%	map.foldl3_values(Pred, Players, !AC1, !AC2, !AC3).

map(MapFunc, Players) = map.map_values_only(MapFunc, Players).

mapFold(Pred, !Players, !AC) :-
	map.map_values_foldl(Pred, !Players, !AC).

mapFold4(Pred, !Players, !AC1, !AC2, !AC3, !AC4) :-
	map.foldl4(withKey_mapFold4(Pred), !.Players, !Players, !AC1, {!.AC2, !.AC3}, {!:AC2, !:AC3}, !AC4).
%	map.foldl5(withKey_mapFold5(Pred), !.Players, !Players, !AC1, !AC2, !AC3, !AC4).

transform(MapFunc, Players) = Result :-
	Pred =
	(pred(P::in, !.List::in, !:List::out) is det :-
		list.cons(MapFunc(P), !List)
	),
	map.foldl_values(Pred, Players, [], Result).

filterFold4(Pred, !Players, !AC1, !AC2, !AC3, !AC4) :-
	map.foldl4(withKey_filterFold4(Pred), !.Players, !Players, !AC1, {!.AC2, !.AC3}, {!:AC2, !:AC3}, !AC4).
%	map.foldl5(withKey_filterFold5(Pred), !.Players, !Players, !AC1, !AC2, !AC3, !AC4).



% randomIDs(HowMany, Result, Players, !Random) :-
% 	(if
% 		HowMany =< 0
% 	then
% 		Result = []
% 	else
% 		rng.nextInt(0, list.length(Players) - 1, Index, !Random),
% 		removeIndex(Index, Players, Player, RestPlayers),
% 		Result = [Player^id | RestResult],
% 		randomIDs(HowMany - 1, RestResult, RestPlayers, !Random)
% 	).

update(ID, UpdateFunc, !Players) :-
	map.lookup(!.Players, ID) = Player,
	map.set(ID, UpdateFunc(Player), !Players)
	.

nextKey(key(Key), key(Key + 1)).

% player(Players, ID) = Result :-
% 	Players = [],
% 	throw("ebea.player.player/2: there is no such player identification in the list")
% 	;
% 	Players = [Player | Rest],
% 	(if
% 		Player^id = ID
% 	then
% 		Result = Player
% 	else
% 		Result = player(Rest, ID)
% 	).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- pred withKey_filterFold5(
	pred(player(C, T), bool, T1, T1, T2, T2, T3, T3, T4, T4) :: in(pred(in, out, in, out, in, out, in, out, in, out) is det),
	key           :: in,
	player(C, T)  :: in,
	players(C, T) :: in, players(C, T) :: out,
	T1            :: in, T1             :: out,
	T2            :: in, T2            :: out,
	T3            :: in, T3            :: out,
	T4            :: in, T4            :: out
) is det.

withKey_filterFold5(Pred, Key, Player, !Players, !AC1, !AC2, !AC3, !AC4) :-
	Pred(Player, Filter, !AC1, !AC2, !AC3, !AC4),
	(	%
		Filter = yes
	;
		Filter = no,
		map.delete(Key, !Players)
	).

:- pred withKey_fold3(
	pred(player(C, T), T1, T1, T2, T2, T3, T3),
	key,
	player(C, T),
	T1, T1,
	T2, T2,
	T3, T3
).
:- mode withKey_fold3(
	in(pred(in, in, out, in, out, di, uo) is det),
	in,
	in,
	in, out,
	in, out,
	di, uo
) is det.
:- mode withKey_fold3(
	in(pred(in, in, out, in, out, in, out) is det),
	in,
	in,
	in, out,
	in, out,
	in, out
) is det.

withKey_fold3(Pred, _Key, Player, !AC1, !AC2, !AC3) :-
	Pred(Player, !AC1, !AC2, !AC3).

:- pred withKey_filterFold4(
	pred(player(C, T), bool, T1, T1, T2, T2, T3, T3, T4, T4),
	key,
	player(C, T),
	players(C, T), players(C, T),
	T1,             T1,
	{T2, T3},       {T2, T3},
	T4,             T4
).

:- mode withKey_filterFold4(in(pred(in, out, in, out, in, out, in, out, in, out) is det), in, in, in, out, in, out, in, out, in, out) is det.
:- mode withKey_filterFold4(in(pred(in, out, in, out, in, out, in, out, di,  uo) is det),	in, in, in, out, in, out, in, out, di,  uo) is det.
%:- mode withKey_filterFold4(in(pred(in, out, in, out, in, out, in, out, in, out) is det), in, in, di,  uo, in, out, in, out, in, out) is det.
%:- mode withKey_filterFold4(in(pred(in, out, in, out, in, out, in, out, di,  uo) is det),	in, in, di,  uo, in, out, in, out, di,  uo) is det.

withKey_filterFold4(Pred, Key, Player, !Players, !AC1, {!.AC2, !.AC3}, {!:AC2, !:AC3}, !AC4) :-
	Pred(Player, Filter, !AC1, !AC2, !AC3, !AC4),
	(	%
		Filter = yes
	;
		Filter = no,
		map.delete(Key, !Players)
	).


:- pred withKey_mapFold4(
	pred(player(C, T), player(C, T),  T1, T1, T2, T2, T3, T3, T4, T4)
	   :: in(pred(in, out, in, out, in, out, in, out, in, out) is det),
	key          :: in,
	player(C, T) :: in,
	players(C, T) :: in,  players(C, T) :: out,
	T1            :: in,  T1            :: out,
	{T2, T3}      :: in,  {T2, T3}      :: out,
	T4            :: in,  T4            :: out
)
	is det.

withKey_mapFold4(Pred, Key, APlayer, !Players, !AC1, {!.AC2, !.AC3}, {!:AC2, !:AC3}, !AC4) :-
	Pred(APlayer, MappedPlayer, !AC1, !AC2, !AC3, !AC4),
	map.set(Key, MappedPlayer, !Players).

:- end_module ebea.population.players.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
