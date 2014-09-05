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
%% init(ElementGenerator, InitialWeight) = WeightVector
%%
%% Initialise a weight vector with the elements provided by the predicate.
%%
:- func init(pred(T), weight) = weightVector(T).
:- mode init(in(pred(out) is nondet), in) = out is det.

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

:- pred parse(weightVector, list(int), list(int)).
:- mode parse(in, out, in) is det.
:- mode parse(out, in, out) is semidet.

:- implementation.

:- import_module float, int, map, solutions.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

:- type weightVector(T) --->
	wv(
		elements :: map(T, weight),
		sum      :: weight,
		size     :: int
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

init(ElementGenerator, InitialWeight) = WeightVector :-
	promise_equivalent_solutions [Elements, Size]
	solutions.unsorted_aggregate2(
		ElementGenerator,
		buildElements(InitialWeight),
		map.init,  Elements,
		0,         Size
	),
	WeightVector^elements = Elements,
	WeightVector^sum = float(Size) * InitialWeight,
	WeightVector^size = Size
	.

drawAsList(WeightVector, HowMany, List, !Random) :-
	drawAsList(WeightVector, no, HowMany, [], List, !Random).

addElements(Elements, InitialWeight, !WeightVector) :-
	Elements = []
	;
	Elements = [AnElement | RestElements],
	map.det_insert(AnElement, InitialWeight, !.WeightVector^elements, NewElements),
	!:WeightVector = wv(
		NewElements,
		!.WeightVector^sum + InitialWeight,
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
	OldWeight = map.lookup(!.WeightVector^elements, ForElement),
	NewWeight =
		OldWeight * (1.0 - ProbabilitySelectedCombination)
		+ ScaledPayoff * ProbabilitySelectedCombination,
	map.det_update(ForElement, NewWeight, !.WeightVector^elements, NewElements),
	!:WeightVector = wv(
		NewElements,
		!.WeightVector^sum - OldWeight + NewWeight,
		!.WeightVector^size
	).

parse(wv(Elements, Sum, Size)) -->
	parseElements(Elements),
	parseable.float32(Sum),
	parseable.int32(Size).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- pred buildElements(weight, T, map(T, weight), map(T, weight), int, int).
:- mode buildElements(in, in, in, out, in, out) is det.

buildElements(InitialWeight, AnElement, !Elements, !Size) :-
	!:Size = !.Size + 1,
	map.det_insert(AnElement, InitialWeight, !Elements).


:- pred drawAsList(
	weightVector(T) :: in,
	bool            :: in,
	int             :: in,
	list(T)         :: in,
	list(T)         :: out,
	R :: in,  R :: out
) is det
	<= ePRNG(R)
.

drawAsList(WeightVector, WithRepetition, HowMany, !List, !Random) :-
	(if
		HowMany = 0
	then
		true
	else
		drawElement(WeightVector, Element, !Random),
		(if
			WithRepetition = no,
			list.member(Element, !.List)
		then
			drawAsList(WeightVector, WithRepetition, HowMany, !List, !Random)
		else
			list.cons(Element, !List),
			drawAsList(WeightVector, WithRepetition, HowMany - 1, !List, !Random)
		)
	).




:- pred drawElement(
	weightVector(T) :: in,
	T               :: out,
	R :: in,  R :: out
) is det
	<= ePRNG(R)
.


drawElement(WeightVector, Result, !Random) :-
	(if
		WeightVector^sum > 0.0
	then
		rng.nextFloat(Value, !Random),
		map.foldl2(
			pickElementByWeight,
			WeightVector^elements,
			Value * WeightVector^sum,  _,
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
		throw("ebea.player.selection.wv.drawElement/4: there is a bug")
	;
		MResult = yes(Result)
	).

:- pred pickElementByWeight(
	T      :: in,
	weight :: in,
	weight   :: in,  weight :: out,
	maybe(T) :: in,  maybe(T) :: out
) is det.

pickElementByWeight(AnElement, HisWeight, !RndWeight, !MResult) :-
	!.MResult = yes(_)
	;
	!.MResult = no,
	(if
		!.RndWeight < HisWeight
	then
		!:MResult = yes(AnElement)
	else
		!:RndWeight = !.RndWeight - HisWeight
	).

:- pred pickElementByIndex(
	T      :: in,
	weight :: in,
	int      :: in,  int :: out,
	maybe(T) :: in,  maybe(T) :: out
) is det.

pickElementByIndex(AnElement, _, !Index, !MResult) :-
	!.MResult = yes(_)
	;
	!.MResult = no,
	(if
		!.Index = 0
	then
		!:MResult = yes(AnElement)
	else
		!:Index = !.Index - 1
	).



:- pred parseElements(map(ebea.population.players.key, float), list(int), list(int)).
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
