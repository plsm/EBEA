/**
 * Provides a type that represents the neighbours of some player and
 * predicates and functions to manipulate this instances.

 * @author Pedro Mariano
 * @version 1.0 2014/05/14
 */
:- module ebea.population.neighbours.

:- interface.

:- import_module ebea.player.
:- import_module rng.
:- import_module array, list.

/**
 * Represents the neighbours of some player in an EBEA population.
 */
:- type neighbours.

/**
 * Weight function used by weighted random sampling of neighbours.  You
 * must pass to predicate {@code weightedRandomElements/6} a function that
 * returns the weight of a neighbour given its identification.
  
 */
:- type weightFunc == (func(ebea.population.players.key) = int).

/**
 * init(Sites, Players, APlayer, Neighbours)

 * Unify {@code APlayer} with a player from the population and unify {@code
 * Neighbours} with its neighbours.  Parameters {@code Sites} and {@code
 * Players} should come from a {@code population(C,T)} instance.

 * @see ebea.population.population/2
 */
:- pred init(array(site), ebea.population.players.players(C, P), ebea.player.player(C, P), neighbours).
:- mode init(in, in, out, out) is nondet.

/**
 * init(Sites, Player) = Result
 
 * Return the neighbours of the given player.
 */
:- func init(array(site), ebea.player.player(C, P)) = neighbours.

/**
 * Return the number of neighbours.
 */
:- func size(neighbours) = int.

:- pred member(ebea.population.players.key, neighbours).
:- mode member(in, in) is semidet.
:- mode member(out, in) is nondet.


/**
 * randomElements(HowMany, Neighbours, Result, !Random)

 * Return {@code HowMany} elements from {@code Neighbours} using sampling
 * without repetition.
  
 */
:- pred randomElements(int, neighbours, list(ebea.population.players.key), R, R)
	<= ePRNG(R).
:- mode randomElements(in, in, out, in, out) is det.

/**
 * weightedRandomElements(Weight, HowMany, Neighbours, Result, !Random)

 * Return {@code HowMany} elements from {@code Neighbours} using sampling
 * without repetition.  Neighbours with zero weight are not returned.  If
 * there are not enough neighbours, the predicate fails.
  
 */
:- pred weightedRandomElements(
	weightFunc :: in,
	int        :: in,
	neighbours :: in,
	list(ebea.population.players.key) :: out,
	R :: in,  R :: out
)
	is semidet
	<= ePRNG(R).



:- func fold(func(ebea.population.players.key, A) = A, neighbours, A) = A.

:- pred fold(pred(ebea.population.players.key, A, A), neighbours, A, A).
:- mode fold(in(pred(in, in, out) is det), in, in, out) is det.
:- mode fold(in(pred(in, di, uo)  is det), in, di, uo)  is det.


:- implementation.

:- import_module float, int.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

:- type neighbours == list(list(ebea.population.players.key)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

init(ArraySites, Players, APlayer, Neighbours) :-
	array.member(ArraySites, Site),
	%
	list.append(Rest1IDs, [PlayerID | Rest2IDs], Site^playerIDs),
	APlayer = ebea.population.players.player(Players, PlayerID),
	SiteNeighbours = [Rest1IDs, Rest2IDs],
	%
	array.foldl(AppendPlayersSite, Site^neighbourSiteIdxs, SiteNeighbours) = Neighbours,
	AppendPlayersSite =
	(func(I, AC) = R :-
		S = array.lookup(ArraySites, I),
		R = [S^playerIDs | AC]
	).

init(ArraySites, APlayer) = Neighbours :-
	array.lookup(ArraySites, APlayer^siteIndex) = Site,
	%
	list.delete_all(Site^playerIDs, APlayer^id) = SiteNeighbours,
	%
	array.foldl(AppendPlayersSite, Site^neighbourSiteIdxs, [SiteNeighbours]) = Neighbours,
	AppendPlayersSite =
	(func(I, AC) = R :-
		S = array.lookup(ArraySites, I),
		R = [S^playerIDs | AC]
	).

size([]) = 0.
size([H | T]) = list.length(H) + size(T).

member(Element, AllNeighbours) :-
	list.member(SomeNeighbours, AllNeighbours),
	list.member(Element, SomeNeighbours).

randomElements(HowMany, Neighbours, Result, !Random) :-
	(if
		HowMany =< 0
	then
		Result = []
	else
		randomElements(HowMany, Neighbours, size(Neighbours) - 1, [], [], Result, !Random)
	).

weightedRandomElements(WeightFunc, HowMany, Neighbours, Result, !Random) :-
	(if
		HowMany =< 0
	then
		Result = []
	else
		SumWeight =
		(func(Key, A) = R :-
			R = A + WeightFunc(Key)
		),
		fold(SumWeight, Neighbours, 0) = TotalWeight,
		weightedRandomElementsLoop(WeightFunc, HowMany, Neighbours, TotalWeight, [], Result, !Random)
	).

fold(_, [], A) = A.
fold(Func, [H | T], A) = fold(Func, T, list.foldl(Func, H, A)).

fold(_, [], !A).
fold(Pred, [H | T], !A) :-
	list.foldl(Pred, H, !A),
	fold(Pred, T, !A).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions


/**
 * randomElements(HowMany, Neighbours, MaxIndex, Skip, Result, !Random)

 * This predicate performs the real work of computing a random sample
 * without repetition of neighbours.

 * @param Skip Contains the indexes that have been sampled and must be skipped.
  
 */
:- pred randomElements(
	int        :: in,
	neighbours :: in,
	int        :: in,
	list(int)  :: in,
	list(ebea.population.players.key) :: in, list(ebea.population.players.key) :: out,
	R :: in,  R :: out
)
	is det
	<= ePRNG(R).

randomElements(HowMany, Neighbours, MaxIndex, Skip, !Result, !Random) :-
	nextInt(0, MaxIndex, Index, !Random),
	(if
		list.member(Index, Skip)
	then
		randomElements(HowMany, Neighbours, MaxIndex, Skip, !Result, !Random)
	else
		index(Neighbours, Index) = Element,
		list.cons(Element, !Result),
		(if
			HowMany = 1
		then
			true
		else
			randomElements(HowMany - 1, Neighbours, MaxIndex, [Index | Skip], !Result, !Random)
		)
	).

/**
 * index(Neighbours, Index) = Element

 * Return the {@code Index}th element in the neighbours collection.

 * <p> Throws an exception if the index is invalid.
 */
:- func index(neighbours, int) = ebea.population.players.key.

index([], _) = throw("ebea.population.neighbours.index/2: Invalid index").
index([H | T], Index) = index(H, T, Index).

/**
 * index(SomeNeighbours, RestNeighbours, Index) = Element

 * This function performs the real work of returning the {@code Index}th
 * element in the neighbours collection.
 */

:- func index(list(ebea.population.players.key), neighbours, int) = ebea.population.players.key.

index([], RestNeighbours, Index) = index(RestNeighbours, Index).

index([H | T], RestNeighbours, Index) = Result :-
	(if
		Index = 0
	then
		Result = H
	else
		Result = index(T, RestNeighbours, Index - 1)
	).

/**
 * This predicate calculates a weighted random sample without repetition of neighbours.
 */
:- pred weightedRandomElementsLoop(
	weightFunc :: in,
	int        :: in,
	neighbours :: in,
	int        :: in,
	list(ebea.population.players.key) :: in, list(ebea.population.players.key) :: out,
	R :: in,  R :: out
)
	is semidet
	<= ePRNG(R).

weightedRandomElementsLoop(WeightFunc, HowMany, Neighbours, RemainingWeight, !Result, !Random) :-
	RemainingWeight > 0,
	nextInt(0, RemainingWeight, Rnd, !Random),
	pick(WeightFunc, Neighbours, Rnd, !.Result) = PickedID,
	list.cons(PickedID, !Result),
	(if
		HowMany = 1
	then
		true
	else
		weightedRandomElementsLoop(
			WeightFunc,
			HowMany - 1,
			Neighbours,
			RemainingWeight - WeightFunc(PickedID),
			!Result,
			!Random
		)
	).

/**
 * This predicate calculates a weighted random sample without repetition of neighbours.
 */
:- pred weightedRandomElementsLoop_v1(
	weightFunc :: in,
	int        :: in,
	neighbours :: in,
	int        :: in,
	int        :: in,
	list(ebea.population.players.key) :: in, list(ebea.population.players.key) :: out,
	R :: in,  R :: out
)
	is semidet
	<= ePRNG(R).

weightedRandomElementsLoop_v1(WeightFunc, HowMany, Neighbours, RemainingWeight, TotalWeight, !Result, !Random) :-
	RemainingWeight > 0,
	nextInt(0, TotalWeight, Rnd, !Random),
	pick(WeightFunc, Neighbours, Rnd) = PickedID,
	(if
		list.member(PickedID, !.Result)
	then
		weightedRandomElementsLoop_v1(WeightFunc, HowMany, Neighbours, RemainingWeight, TotalWeight, !Result, !Random)
	else
		list.cons(PickedID, !Result),
		(if
			HowMany = 1
		then
			true
		else
			weightedRandomElementsLoop_v1(
				WeightFunc,
				HowMany - 1,
				Neighbours,
				RemainingWeight - WeightFunc(PickedID),
				TotalWeight,
				!Result,
				!Random
			)
		)
	).

/**
 * This function together with {@code pickAux/7} retrieve the player's id
 * that was selected.
 */

:- func pick(weightFunc, neighbours, int, list(ebea.population.players.key)) = ebea.population.players.key.

pick(_, [], _, _) = throw("pick/4: invalid weight").

pick(WeightFunc, [H | T], RndWeight, PickedIDs) = pickAux(WeightFunc, H, T, RndWeight, PickedIDs).


:- func pickAux(weightFunc, list(ebea.population.players.key), neighbours, int, list(ebea.population.players.key)) = ebea.population.players.key.

pickAux(WeightFunc, [], RestNeighbours, RndWeight, PickedIDs) = pick(WeightFunc, RestNeighbours, RndWeight, PickedIDs).

pickAux(WeightFunc, [H | T], RestNeighbours, RndWeight, PickedIDs) = Result :-
	(if
		list.member(H, PickedIDs)
	then
		pickAux(WeightFunc, T, RestNeighbours, RndWeight, PickedIDs) = Result
	else
		HWeight = WeightFunc(H),
		(if
			RndWeight < HWeight
		then
			Result = H
		else
			Result = pickAux(WeightFunc, T, RestNeighbours, RndWeight - HWeight, PickedIDs)
		)
	).

/**
 * This function together with {@code pickAux/7} retrieve the player's id
 * that was selected.
 */

:- func pick(weightFunc, neighbours, int) = ebea.population.players.key.

pick(_, [], _) = throw("pick/4: invalid weight").

pick(WeightFunc, [H | T], RndWeight) = pickAux(WeightFunc, H, T, RndWeight).


:- func pickAux(weightFunc, list(ebea.population.players.key), neighbours, int) = ebea.population.players.key.

pickAux(WeightFunc, [], RestNeighbours, RndWeight) = pick(WeightFunc, RestNeighbours, RndWeight).

pickAux(WeightFunc, [H | T], RestNeighbours, RndWeight) = Result :-
	HWeight = WeightFunc(H),
	(if
		RndWeight < HWeight
	then
		Result = H
	else
		Result = pickAux(WeightFunc, T, RestNeighbours, RndWeight - HWeight)
	).

:- end_module ebea.population.neighbours.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
