%% ************************************************************************
%% Provides a type that represents a weight vector of elements.  This
%% vector is used to draw elements proportionally to their weights.
%%
%% @author Pedro Mariano
%%
%% @version 1.0 2014/09/05

:- module ebea.player.selection.wv.

:- interface.

%% ************************************************************************
:- type weightVector(T).

:- type weightVector == weightVector(ebea.population.players.key).


%% ************************************************************************
%% Weights are represented as floating point numbers.
:- type weight == float.

%% ************************************************************************
%% init = WeightVector
%%
%% Initialise an empty weight vector.
%%
:- func init = weightVector(T).

%% ************************************************************************
%% init(ElementGenerator, InitialWeight) = WeightVector
%%
%% Initialise a weight vector with the elements provided by the predicate.
%%
:- func init(pred(T), weight) = weightVector(T).
:- mode init(in(pred(out) is nondet), in) = out is det.

:- func length(weightVector) = int.

%% ************************************************************************
%% drawAsList(WeightVector, HowMany, Result, !Random)
%%
%% Draw elements without repetition from the weight vector.  The
%% probability of an element being drawn is proportional to its
%% weight.  An element with zero weight is not selected.  If all
%% elements have zero weight, then random selection of elements is
%% performed.
%%
%% @param Result A list with {@code HowMany} distinct elements from
%% {@code WeightVector}.
%%
%% @param !Random  Pseudo-random number generator used to draw elements.
%%
:- pred drawAsList(
	weightVector(T) :: in,
	int             :: in,
	list(T)         :: out,
	R :: in,  R :: out
) is det
	<= ePRNG(R)
.

%% ************************************************************************
%% addElements(Elements, InitialWeight, !WeightVector)
%%
%% Add elements to a weight vector with the given weight.
:- pred addElements(
	list(T) :: in,
	weight  :: in,
	weightVector(T) :: in,  weightVector(T) :: out
) is det.

%% ************************************************************************
%% removeElements(Elements, !WeightVector)
%%
%% Remove elements from the weight vector.
%%
:- pred removeElements(
	list(T) :: in,
	weightVector(T) :: in,  weightVector(T) :: out
) is det.


%% ************************************************************************
%% updateWeight(ProbabilitySelectedCombination, ScaledPayoff, ForElement, !WeightVector)
%%
%% Update the weight of the given element given the probability of the
%% combination he is and the payoff obtained by the focal player.
%%
:- pred updateWeight(
	float :: in,
	float :: in,
	T     :: in,
	weightVector(T) :: in,  weightVector(T) :: out
) is det.



/**
 * updatePlayerProbCombVectorsWeightVector(ProbCombVector, WeightVector, Player) = Result
  
 * Updates the data of a player after he has played a game.  In this module
 * this corresponds to replacing the probability and combination vectors and weight vector.
  
 */
:- func updatePlayerProbCombVectorsWeightVector(probabilityCombinationVector, weightVector, player(C, T)) = player(C, T).


:- pred parse(weightVector, list(int), list(int)).
:- mode parse(in, out, in) is det.
:- mode parse(out, in, out) is semidet.

:- pred fold(pred(T, float, A, A), weightVector(T), A, A).
:- mode fold(in(pred(in, in, in, out) is det), in, in, out) is det.
:- mode fold(in(pred(in, in, di, uo) is det), in, di, uo) is det.

:- implementation.

:- import_module float, int, map, solutions.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

:- type weightVector(T) --->
	wv(
		elements :: map(T, weightInternal),
		sum      :: weightInternal,
		size     :: int
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

:- type weightInternal == int.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

init = wv(map.init, 0, 0).

init(ElementGenerator, InitialWeight) = WeightVector :-
	InitialWeightInternal = float.round_to_int(InitialWeight * 1000.0),
	promise_equivalent_solutions [Elements, Size]
	solutions.unsorted_aggregate2(
		ElementGenerator,
		buildElements(InitialWeightInternal),
		map.init,  Elements,
		0,         Size
	),
	WeightVector^elements = Elements,
	WeightVector^sum = Size * InitialWeightInternal,
	WeightVector^size = Size
	.

length(WeightVector) = WeightVector^size.

drawAsList(WeightVector, HowMany, List, !Random) :-
	drawAsList(WeightVector, no, HowMany, 0, [], List, !Random).

addElements(Elements, InitialWeight, !WeightVector) :-
	Elements = []
	;
	Elements = [AnElement | RestElements],
	InitialWeightInternal = float.round_to_int(InitialWeight * 1000.0),
	map.det_insert(AnElement, InitialWeightInternal, !.WeightVector^elements, NewElements),
	!:WeightVector = wv(
		NewElements,
		!.WeightVector^sum + InitialWeightInternal,
		!.WeightVector^size + 1
	),
	addElements(RestElements, InitialWeight, !WeightVector)
	.
	

removeElements(Elements, !WeightVector) :-
	Elements = []
	;
	Elements = [AnElement | RestElements],
	(if
		map.remove(AnElement, HisWeight, !.WeightVector^elements, NewElements)
	then
		!:WeightVector = wv(
			NewElements,
			!.WeightVector^sum - HisWeight,
			!.WeightVector^size - 1
		)
	else
		true
	),
	removeElements(RestElements, !WeightVector)
	.

updateWeight(ProbabilitySelectedCombination, ScaledPayoff, ForElement, !WeightVector) :-
	(if
		map.search(!.WeightVector^elements, ForElement, _AnWeight)
	then
		OldWeight = map.lookup(!.WeightVector^elements, ForElement),
		ProbabilitySelectedCombinationInternal = float.round_to_int(1000.0 * ProbabilitySelectedCombination),
		ScaledPayoffInternal = float.round_to_int(1000.0 * ScaledPayoff),
		NewWeight =
			(OldWeight * (1000 - ProbabilitySelectedCombinationInternal)
			+ ScaledPayoffInternal * ProbabilitySelectedCombinationInternal) / 1000,
		map.det_update(ForElement, NewWeight, !.WeightVector^elements, NewElements),
		!:WeightVector = wv(
			NewElements,
			!.WeightVector^sum - OldWeight + NewWeight,
			!.WeightVector^size
	)
	else
	trace [io(!IO)] io.format("DEBUG weight vector %s\n", [s(string(!.WeightVector))], !IO),
	map.lookup(!.WeightVector^elements, ForElement, _)
	).
	

updatePlayerProbCombVectorsWeightVector(PCV, WV, Player) = Result :-
	Player^traits^selectionTrait = _SelectionPhe,
	Traits = partnerSelection(PCV, yes(WV)),
	PlayerTraits = 'selectionTrait :='(Player^traits, Traits),
	Result = 'traits :='(Player, PlayerTraits)
	;
	Player^traits^selectionTrait = random,
	throw("ebea.player.selection.wv.updatePlayerProbCombVectorsWeightVector/3: Invalid player traits")
	;
	Player^traits^selectionTrait = opinion(_, _),
	throw("ebea.player.selection.wv.updatePlayerProbCombVectorsWeightVector/3: Invalid player traits")
	.

parse(wv(Elements, Sum, Size)) -->
	parseElements(Elements),
	parseable.int32(Sum),
	parseable.int32(Size).

fold(Pred, wv(Elements, _, _), !AC) :-
	map.foldl(testaFerro(Pred), Elements, !AC).
/*
:- pred testaFerro(pred(ebea.population.players.key, float, A, A),  ebea.population.players.key, weightInternal, A, A).
:- mode testaFerro(in(pred(in, in, in, out) is det), in, in, in, out) is det.
:- mode testaFerro(in(pred(in, in, di, uo)  is det), in, in, di, uo) is det.
*/
:- pred testaFerro(pred(T, float, A, A),  T, weightInternal, A, A).
:- mode testaFerro(in(pred(in, in, in, out) is det), in, in, in, out) is det.
:- mode testaFerro(in(pred(in, in, di, uo)  is det), in, in, di, uo) is det.

testaFerro(Pred, K, V, !AC) :-
	RV = float(V) / 1000.0,
	Pred(K, RV, !AC)
	.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- pred buildElements(weightInternal, T, map(T, weightInternal), map(T, weightInternal), int, int).
:- mode buildElements(in, in, in, out, in, out) is det.

buildElements(InitialWeight, AnElement, !Elements, !Size) :-
	!:Size = !.Size + 1,
	map.det_insert(AnElement, InitialWeight, !Elements).


:- pred drawAsList(
	weightVector(T) :: in,
	bool            :: in,
	int             :: in,
	weightInternal  :: in,
	list(T)         :: in,
	list(T)         :: out,
	R :: in,  R :: out
) is det
	<= ePRNG(R)
.

drawAsList(WeightVector, WithRepetition, HowMany, SumWeightDrawn, !List, !Random) :-
	(if
		HowMany = 0
	then
		true
	else
		drawElement(WeightVector, SumWeightDrawn, Element, HisWeight, !Random),
		(if
			WithRepetition = no,
			list.member(Element, !.List)
		then
%			trace [io(!IO)] (io.format("Repeated %s\n", [s(string(WeightVector))], !IO)),
			drawAsList(WeightVector, WithRepetition, HowMany, SumWeightDrawn, !List, !Random)
		else
			list.cons(Element, !List),
			drawAsList(WeightVector, WithRepetition, HowMany - 1, SumWeightDrawn + HisWeight, !List, !Random)
		)
	).

:- pred drawElement(
	weightVector(T) :: in,
	weightInternal  :: in,
	T      :: out,
	weightInternal  :: out,
	R :: in,  R :: out
) is det
	<= ePRNG(R)
.

drawElement(WeightVector, SumWeightDrawn, Element, HisWeight, !Random) :-
	(if
		WeightVector^sum > 0,
		SumWeightDrawn < WeightVector^sum
	then
		rng.nextInt(0, WeightVector^sum, Value, !Random),
		map.foldl2(
			pickElementByWeight,
			WeightVector^elements,
			Value,  _,
			no,                        MResult
		)
	else
		rng.nextInt(0, WeightVector^size - 1, Index, !Random),
		map.foldl2(
			pickElementByIndex,
			WeightVector^elements,
			Index,  _,
			no,     MResult
		)
	),
	(	%
		MResult = no,
		rng.nextInt(0, WeightVector^size - 1, Gosh, !Random),
		map.foldl2(
			pickElementByIndex,
			WeightVector^elements,
			Gosh,  _,
			no,     MGoshResult
		),
		(	%
			MGoshResult = no,
			throw("ebea.player.selection.wv.drawElement/4: there is a bug")
		;
			MGoshResult = yes({Element, HisWeight})
		)
	;
		MResult = yes({Element, HisWeight})
	).

:- pred pickElementByWeight(
	T      :: in,
	weightInternal :: in,
	weightInternal   :: in,  weightInternal :: out,
	maybe({T, weightInternal}) :: in,  maybe({T, weightInternal}) :: out
) is det.

pickElementByWeight(AnElement, HisWeight, !RndWeight, !MResult) :-
	!.MResult = yes(_)
	;
	!.MResult = no,
	(if
		!.RndWeight < HisWeight
	then
		!:MResult = yes({AnElement, HisWeight})
	else
		!:RndWeight = !.RndWeight - HisWeight
	).

:- pred pickElementByIndex(
	T      :: in,
	weightInternal :: in,
	int      :: in,  int :: out,
	maybe({T, weightInternal}) :: in,  maybe({T, weightInternal}) :: out
) is det.

pickElementByIndex(AnElement, HisWeight, !Index, !MResult) :-
	!.MResult = yes(_)
	;
	!.MResult = no,
	(if
		!.Index = 0
	then
		!:MResult = yes({AnElement, HisWeight})
	else
		!:Index = !.Index - 1
	).

:- pred parseElements(map(ebea.population.players.key, weightInternal), list(int), list(int)).
:- mode parseElements(in, out, in) is det.
:- mode parseElements(out, in, out) is semidet.

:- pragma promise_pure(parseElements/3).

parseElements(Elements::in, !.List::out, !:List::in) :-
	parseable.parseList(normalType, map.keys(Elements), !List),
	parseable.parseList(normalType, map.values(Elements), !List).

parseElements(Elements::out, !.List::in, !:List::out) :-
	parseable.parseList(normalType, Keys, !List),
	parseable.parseList(normalType, Values, !List),
	Elements = map.set_from_corresponding_lists(map.init, Keys, Values).

:- end_module ebea.player.selection.wv.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:

