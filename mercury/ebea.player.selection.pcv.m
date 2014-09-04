/**
 * Provides predicates and functions related to selection based on the
 * probability and combination vectors.

 * @author Pedro Mariano
 * @version 1.0 2014/01/26
 */
:- module ebea.player.selection.pcv.

:- interface.

%% ************************************************************************
%% Chromosome genes responsible for the selection based on the probability
%% and combination vectors.

:- type chromosome --->
	partnerSelection(
		poolSize                :: int ,
		bitsPerProbability      :: int ,
		probabilityUpdateFactor :: float ,
		payoffThreshold         :: float
	).


/**
 * The probability and combination vectors used by this partner selection
 * model.  Both vectors are stored in an array where each array element
 * represents a probability and a combination.  Array elements are
 * represented by type {@code slot}.

 * <p> Probabilities are stored in integers.  A slot in index <i>i</i>
 * contains the sum of probabilities from slot zero up to slot <i>i</i>.
 * This permits using a binary search when selecting a combination.
 */

:- type probabilityCombinationVector == array(slot).

:- type slot --->
	slot(
		probability :: int,
		combination :: combination
	).

:- type combination == list(ebea.population.players.key).

:- inst pcv == bound(parterSelection(ground)).

:- inst playerPCV == bound(player(ground, ground, ground, bound(traits(ground, ground, pcv)))).

/**
 * initProbabilityCombinationVector(Quotient, Remainder, Index) = Result
  
 * Initialise the probabilities in the probability vector.

 * <p> The combinations are left uninitialised, i.e., they are unified with
 * an empty list.  If a slot with an empty combination is selected, we draw
 * a random combination of elements.  That combination is the used to
 * update the slot (if the payoff was greater than the threshold).

 * <p> Each slot contains a probability of selecting all slots ranging from
 * 0th element to {@code index}th element.  Selecting a slot is done by a
 * binary search which is a faster method compared to scanning an entire
 * vector.

 * @see born/1
 */

:- func initProbabilityVector(int, int, int) = slot.

/**
 * select(NumberPartners, Neighbours, ProbCombVectors, !Random, SelectedSlot, CombinationIDs, Combination)
  
 * Select a combination of partners using the given partner selection state
 * and pseudo-random number generator.  If the pool size is zero, {@code
 * NumberPartners} random partners are selected from {@code Neighbours}.
 * If the pool size is one, we always return the combination in that slot.
 * If the pool size is greater than one, a binary search is performed.
 * This is delegated to predicate {@code search/6}.

 * <p> The pool size may have empty combinations.  In such case, we select
 * {@code NumberPartners} random partners from {@code
 * Neighbours}.  This combination is put in the slot by predicate {@code
 * update/10}.

 * <p> When a selection chromosome with partner selection genes develops,
 * the traits contains empty combinations.  This is due because the type
 * class method does not get the partners.  Only when a slot is selected
 * the first time, we compute a random combination.  If the payoff obtained
 * was good, it goes to the combination vector.

 * @see search/6
  
 */
:- pred select(int, ebea.population.neighbours.neighbours, probabilityCombinationVector, R, R, int, list(key))
	<= ePRNG(R).
:- mode select(in, in, in, in, out, out, out) is det.

/**
 * updateProbCombVectors(Game, EnergyScaling, CombinationSize, Chromosome, SelectedCombinationIDs, Partners, SelectedSlot, Payoff, !Random, !ProbCombVectors)

 * Update the probability and combination vectors given the game result and
 * the selected combination.  If the payoff was lower than the threshold,
 * the combination slot is replaced using predicate {@code Partners} to
 * generate a new combination.  The probability of this combination is
 * decreased, and distributed among the others.

 * <p> Since probabilities are represented by integers, the amount to
 * decrease may not be a multiple of the number of slots minus one.  The
 * remainder is assigned to a random slot.  Which can be the selected slot.
 */
:- pred updateProbCombVectors(
	G, energyScaling,
	int, ebea.player.selection.pcv.chromosome, list(key), ebea.population.neighbours.neighbours,
	int, float,
	R, R,
	probabilityCombinationVector, probabilityCombinationVector
	)
	<= (abstractGame(G), ePRNG(R)).
:- mode updateProbCombVectors(in, in, in, in, in, in, in, in, in, out, in, out) is det.
%:- mode updateProbCombVectors(in, in, in, in(bound(partnerSelection(ground,ground,ground,ground))), in, in, in, in, in, out, di, uo) is det.
%:- mode update(in, in(pred(di, uo, out) is det), in, in, di, uo, di, uo) is det.


/**
 * updatePlayerProbCombVectors(Payoff, Player) = Result
  
 * Updates the data of a player after he has played a game.  In this module
 * this corresponds to replacing the probability and combination vectors.
  
 */
:- func updatePlayerProbCombVectors(probabilityCombinationVector, player(C, T)) = player(C, T).


/**
 * update(NumberPartners, Neighbours, DeadPlayerIDs, !Slot, !Random)
  
 * Updates the slot of the partner combination vectors after the birth and
 * death process of a round.  If a slot contains references to dead
 * players, it is replaced by a new combination.  It may happen that the
 * player does not have enough partners to form a combination.  In this
 * case field {@code combination} is assigned the empty list.

 * <p> The empty list is used in slot initialisation.

 * <p> When a player selects a slot with an empty list, it randomly selects
 * partners to fill that slot.  The slot is updated in predicate {@code
 * updateProbCombVectors/12}.

 * @see ebea.player.selection.round/7
 
 */
:- pred checkForDeadPlayers(int, ebea.population.neighbours.neighbours, list(key), slot, slot, R, R)
	<= ePRNG(R).
:- mode checkForDeadPlayers(in, in, in, in, out, in, out) is det.


/**
 * copyPercentageCombinations(Percentage, Source, !Destiny)
  
 * Copy a percentage of combinations from slots in {@code Source} to {@code
 * !Destiny}.  The probabilities in {@code !Destiny} remain unchanged.
  
 */
% :- pred copyPercentageCombinations(int, probabilityCombinationVector, probabilityCombinationVector, probabilityCombinationVector, R, R)
% 	<= ePRNG(R).
% :- mode copyPercentageCombinations(in, in, array_di, array_uo, in, out) is det.
% %:- mode copyPercentageCombinations(in, in, in, out, in, out) is det.


:- pred copyPercentageCombinations(ebea.player.selection.parameters, player(C, T), player(C, T), player(C, T), R, R)
	<= ePRNG(R).
%:- mode copyPercentageCombinations(in, in(playerPCV), in(playerPCV), out, in, out) is det.
:- mode copyPercentageCombinations(in, in, in, out, in, out) is det.


/**
 * probability(ProbCombVectors, Index, Value)

 * Return the raw probability of selecting the {@code Index} combination.
 */
:- pred probability(probabilityCombinationVector, int, int).
:- mode probability(in, in, out) is det.

:- pred parse(probabilityCombinationVector, list(int), list(int)).
:- mode parse(in, out, in) is det.
:- mode parse(out, in, out) is semidet.

:- pred test(io.state, io.state).
:- mode test(di, uo) is det.

:- implementation.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

:- instance parseable(slot) where
[
	pred(parse/3) is parseSlot
].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

initProbabilityVector(Quotient, Remainder, Index) = Slot :-
	% (if
	% 	Index = 0
	% then
	%	Slot^probability = (Index + 1) * Quotient + Remainder
	% else
	% 	Slot^probability = (Index + 1) * Quotient
	% ),
	Slot^probability = (Index + 1) * Quotient + Remainder,
	Slot^combination = [].

select(NumberPartners, Neighbours, ProbCombVectors, !Random, SelectedSlot, CombinationIDs) :-
	% calculate TmpCombinationIDs
	(if
		array.size(ProbCombVectors) = 0
	then
		TmpCombinationIDs = [],
		SelectedSlot = -1
	else if
		array.size(ProbCombVectors) = 1
	then
		SelectedSlot = 0,
		array.lookup(ProbCombVectors, 0, Slot),
		TmpCombinationIDs = Slot^combination
	else
		LastIndex = array.size(ProbCombVectors) - 1,
		array.lookup(ProbCombVectors, LastIndex, LastSlot),
		MaxProbability = LastSlot^probability,
		rng.nextInt(0, MaxProbability, SelectedProbability, !Random),
		search(0, LastIndex, SelectedProbability, ProbCombVectors, SelectedSlot, TmpCombinationIDs)
	),
	% check emptiness of TmpCombinationIDs (zero pool size, uninitialised slot, neighbours recovered)
	(if
		TmpCombinationIDs = []
	then
		ebea.population.neighbours.randomElements(NumberPartners, Neighbours, CombinationIDs, !Random)
	else
		CombinationIDs = TmpCombinationIDs
	).

updateProbCombVectors(Game, EnergyScaling, CombinationSize, Chromosome, SelectedCombinationIDs, Partners, SelectedSlot, Payoff, !Random, !ProbCombVectors) :-
	VectorSize = array.size(!.ProbCombVectors),
	(if
		scaledPayoffToThreshold(Game, EnergyScaling, Payoff) >= Chromosome^payoffThreshold
	then
		(if
			array.size(!.ProbCombVectors) > 0,
			array.lookup(!.ProbCombVectors, SelectedSlot)^combination = []
		then
			Slot^probability = array.lookup(!.ProbCombVectors, SelectedSlot)^probability,
			Slot^combination = SelectedCombinationIDs,
			array.slow_set(SelectedSlot, Slot, !ProbCombVectors)
			%array.set(SelectedSlot, Slot, !ProbCombVectors)
		else
			true
		)
	else
		(if
			array.size(!.ProbCombVectors) = 0
		then
			true
		else
			TP0 = array.lookup(!.ProbCombVectors, VectorSize - 1)^probability,
			ebea.population.neighbours.randomElements(CombinationSize, Partners, NewCombinationIDs, !Random),
			(if
				array.size(!.ProbCombVectors) = 1
			then
				Slot^probability = array.lookup(!.ProbCombVectors, 0)^probability,
				Slot^combination = NewCombinationIDs,
				array.slow_set(0, Slot, !ProbCombVectors)
				%array.set(0, Slot, !ProbCombVectors)
			else
				probability(!.ProbCombVectors, SelectedSlot, OldProbability),
				rng.nextInt(0, array.size(!.ProbCombVectors) - 1, RemainderSlot, !Random),
				HowMuch = float.round_to_int(float(OldProbability) * (1.0 - Chromosome^probabilityUpdateFactor)),
				Distribute = HowMuch / (array.size(!.ProbCombVectors) - 1),
				Remainder = HowMuch rem (array.size(!.ProbCombVectors) - 1),
				array.map_foldl(updateSlot(HowMuch, Distribute, Remainder, RemainderSlot, SelectedSlot, NewCombinationIDs), !ProbCombVectors, 0, _),
				%
				TP1 = array.lookup(!.ProbCombVectors, VectorSize - 1)^probability,
				(if
					TP0 \= TP1
				then
					trace [io(!IO)] (io.format("TP0:%d TP1:%d  HowMuch:%d  Distribute=%d Remainder=%d  RemainderSlot=%d  SelectedSlot=%d   PCV:%s\n",
						[i(TP0),
						 i(TP1),
						 i(HowMuch),
						 i(Distribute),
						 i(Remainder),
						 i(RemainderSlot),
						 i(SelectedSlot),
						 s(string(!.ProbCombVectors))], !IO))
				else
					true
				)
				%trace [io(!IO)] (io.format("Before  %s\n", [s(string(!.ProbCombVectors))], !IO))
			)
		)
	).



updatePlayerProbCombVectors(PCV, Player) = Result :-
	Player^traits^selectionTrait = SelectionPhe,
	SelectionPhe = partnerSelection(_),
	Traits = 'pcv :='(SelectionPhe, PCV),
	PlayerTraits = 'selectionTrait :='(Player^traits, Traits),
	Result = 'traits :='(Player, PlayerTraits)
	;
	Player^traits^selectionTrait = random,
	throw("ebea.player.selection.pcv.updatePlayerProbCombVectors/2: Invalid player traits")
	;
	Player^traits^selectionTrait = opinion(_, _),
	throw("ebea.player.selection.pcv.updatePlayerProbCombVectors/2: Invalid player traits")
	.

checkForDeadPlayers(NumberPartners, Neighbours, DeadPlayerIDs, !Slot, !Random) :-
	(if
		list.member(ID, DeadPlayerIDs),
		list.member(ID, !.Slot^combination)
	then
		(if
			ebea.population.neighbours.size(Neighbours) < NumberPartners
		then
			CombinationIDs = []
		else
			ebea.population.neighbours.randomElements(NumberPartners, Neighbours, CombinationIDs, !Random)
		),
		!:Slot = 'combination :='(!.Slot, CombinationIDs)
	else
		true
	).

copyPercentageCombinations(Parameters, Parent, !Offspring, !Random) :-
	(if
		Parent^traits^selectionTrait = partnerSelection(ParentPCV),
		!.Offspring^traits^selectionTrait = partnerSelection(OldPCV)
	then
		Remaining = int.min(
			float.round_to_int(float(Parameters^poolSizePercentageTransmission * array.size(ParentPCV)) / 100.0),
			array.size(OldPCV)),
		(if
			Remaining > 0
		then
			copyCombination(Remaining, array.size(ParentPCV) - 1, array.copy(ParentPCV), OldPCV, NewPCV, !Random),
			!:Offspring = 'traits :='(
				!.Offspring,
				'selectionTrait :='(
					!.Offspring^traits,
					OffspringTrait
				)
			),
			OffspringTrait = partnerSelection(NewPCV)
		else
			true
		)
	else
		throw("copyPercentageCombinations/6: never reached")
	).
/*
copyPercentageCombinations(Percentage, Source, !Destiny, !Random) :-
	Remaining = int.min(Percentage * array.size(Source) / 100, array.size(!.Destiny)),
	copyCombination(Remaining, array.size(Source) - 1, array.copy(Source), !Destiny, !Random).
*/
probability(ProbCombVectors, Index, Value) :-
	(if
		Index = 0
	then
		array.lookup(ProbCombVectors, Index, Slot),
		Value = Slot^probability
	else
		array.lookup(ProbCombVectors, Index - 1, SlotP),
		array.lookup(ProbCombVectors, Index, SlotI),
		Value = SlotI^probability - SlotP^probability
	).

:- pragma promise_pure(parse/3).

parse(PCV::in, ListOut::out, ListIn::in) :-
	parseable.parseList(normalType, array.to_list(PCV), ListOut, ListIn).

parse(PCV::out, ListOut::in, ListIn::out) :-
	parseable.parseList(normalType, X, ListOut, ListIn),
	PCV = array.from_list(X).

:- import_module random.

test(!IO) :-
	io.print("Enter vector size and RNG seed:\n", !IO),
	io.read_line_as_string(ILine0, !IO),
	(if
		ILine0 = ok(Line0),
		string.words(Line0) = [SVectorSize, SSeed, SPercentage],
		string.to_int(SVectorSize, VectorSize),
		string.to_int(SSeed, Seed),
		VectorSize > 0,
		string.to_int(SPercentage, Percentage),
		Percentage >= 0,
		Percentage =< 100
	then
		random.init(Seed, Supply),
		FuncGenerateParent =
		(func(Index) = Result :-
			Result = slot(Index, [ebea.population.players.int2key(Index), ebea.population.players.int2key(-1)])
		),
		array.generate(VectorSize, FuncGenerateParent) = PCVParent,
		FuncGenerateOffspring =
		(func(Index) = Result :-
			Result = slot(Index, [ebea.population.players.int2key(Index)])
		),
		array.generate(VectorSize, FuncGenerateOffspring) = PCVOffspring,
		testLoop(Percentage, 0, PCVParent, PCVOffspring, Supply, _, !IO)
	else
		true
	).
  /*
	io.print("Enter vector size and total probability:\n", !IO),
	io.read_line_as_string(ILine0, !IO),
	(if
		ILine0 = ok(Line0),
		string.words(Line0) = [SVectorSize, STotalProbability],
		string.to_int(SVectorSize, VectorSize),
		string.to_int(STotalProbability, TotalProbability),
		VectorSize > 0,
		TotalProbability > 0
	then
		Quotient = TotalProbability / VectorSize,
		Remainder = TotalProbability rem VectorSize,
		PCV = array.generate(
			VectorSize,
			ebea.player.selection.pcv.initProbabilityVector(Quotient, Remainder)),
		testLoop(PCV, !IO)
	else
		true
	).*/


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

/**
 * copyCombination(Remaining, Size, Source, !Destiny)

 * Copy a random combination taken from slots {@code 0} to {@code Size} of
 * array {@code Source}.  The combination is placed in position {@code
 * Remaining-1} of {@code !Destiny}. Stops when {@code Remaining} is zero.
  
 */
:- pred copyCombination(int, int, probabilityCombinationVector, probabilityCombinationVector, probabilityCombinationVector, R, R)
	<= ePRNG(R).
:- mode copyCombination(in, in, array_di, array_di, array_uo, in, out) is det.

copyCombination(Remaining, Size, Source, !Destiny, !Random) :-
	(if
		Remaining = 0
	then
		true
	else
		rng.nextInt(0, Size, Index, !Random),
		ASlot = array.lookup(Source, Index),
		Combination = ASlot^combination,
		array.lookup(!.Destiny, Remaining - 1, SlotToModify),
%		ModifiedSlot = 'combination :='(SlotToModify, Combination),
		ModifiedSlot = SlotToModify^combination := Combination,
		array.set(Remaining - 1, ModifiedSlot, !Destiny),
		(if
			Index = Size
		then
			copyCombination(Remaining - 1, Size - 1, Source, !Destiny, !Random)
		else
			array.set(Index, array.lookup(Source, Size), Source, TmpSource),
			array.set(Size, ASlot, TmpSource, NextSource),
			copyCombination(Remaining - 1, Size - 1, NextSource, !Destiny, !Random)
		)
	).

/**
 * search(Low, High, SelectedProbability, ProbCombVectors, Combination)
  
 * Search the slot in the probability and combination vectors associated
 * with the given probability.  This predicate performs a binary search on
 * array {@code ProbCombVectors}.  Parameters {@code Low} and {@code High}
 * represent the range where the search is.

 */
:- pred search(int, int, int, probabilityCombinationVector, int, list(ebea.population.players.key)).
:- mode search(in, in, in, in, out, out) is det.

search(Low, High, SelectedProbability, ProbCombVectors, SelectedSlot, Combination) :-
%	trace [io(!IOT)] (io.format("search %d  [%d %d]\n", [i(SelectedProbability), i(Low), i(High)], !IOT)),
	(if
		Low >= High
	then
		array.lookup(ProbCombVectors, Low, Slot),
		SelectedSlot = Low,
		Combination = Slot^combination
	else
		Middle = (High + Low) / 2,
		array.lookup(ProbCombVectors, Middle, Slot),
		(if
			Slot^probability =< SelectedProbability
		then
			search(Middle + 1, High, SelectedProbability, ProbCombVectors, SelectedSlot, Combination)
		else
			search(Low, Middle, SelectedProbability, ProbCombVectors, SelectedSlot, Combination)
		)
	).


/**
 * Update a slot of the probability and combination vectors after a player
 * has played a game with a selected combination and got an utility lower
 * than the threshold
 */
:- pred updateSlot(int, int, int, int, int, combination, slot, slot, int, int).
:- mode updateSlot(in, in, in, in, in, in, in, out, in, out) is det.

updateSlot(HowMuch, Distribute, Remainder, RemainderSlot, SelectedSlot, NewCombination, Slot, MapSlot, Index, NextIndex) :-
	NextIndex = Index + 1,
	(if
		Index = SelectedSlot
	then
		MapSlot^combination = NewCombination
	else
		MapSlot^combination = Slot^combination
	),
	(if
		RemainderSlot < SelectedSlot,
		Index < RemainderSlot
		;
		RemainderSlot = SelectedSlot,
		Index < SelectedSlot
		;
		RemainderSlot > SelectedSlot,
		Index < SelectedSlot
	then
		MapSlot^probability = Slot^probability + Distribute * (Index + 1)
	else if
		RemainderSlot < SelectedSlot,
		Index < SelectedSlot
	then
		MapSlot^probability = Slot^probability + Distribute * (Index + 1) + Remainder
	else if
		RemainderSlot > SelectedSlot,
		Index >= SelectedSlot,
		Index < RemainderSlot
	then
		MapSlot^probability = Slot^probability + Distribute * Index - HowMuch
	else
		MapSlot^probability = Slot^probability + Distribute * Index - HowMuch + Remainder
	).


:- pred parseSlot(slot, list(int), list(int)).
:- mode parseSlot(in, out, in) is det.
:- mode parseSlot(out, in, out) is semidet.

parseSlot(Slot) -->
	parseable.int32(Slot^probability),
	parseable.parseList(normalType, Slot^combination)
	.




:- pred testLoop(probabilityCombinationVector, io.state, io.state).
:- mode testLoop(in, di, uo) is det.

testLoop(PCV, !IO) :-
	VectorSize = array.size(PCV),
	PredPrintValue =
	(pred(Index::in, IOdi::di, IOuo::uo) is det :-
		io.format(" %4d", [i(array.lookup(PCV, Index)^probability)], IOdi, IOuo)
	),
	PredPrintProbability =
	(pred(Index::in, IOdi::di, IOuo::uo) is det :-
		probability(PCV, Index, Value),
		io.format(" %4d", [i(Value)], IOdi, IOuo)
	),
	io.print("pv:   ", !IO),
	int.fold_up(PredPrintValue, 0, (VectorSize - 1), !IO),
	io.nl(!IO),
	io.print("prob: ", !IO),
	int.fold_up(PredPrintProbability, 0, (VectorSize - 1), !IO),
	io.nl(!IO),
	
	io.print("Enter selected slot, amount to decrease and remainder slot:\n", !IO),
	io.read_line_as_string(ILine1, !IO),
	(if
		ILine1 = ok(Line1),
		string.words(Line1) = [SSelectedSlot, SHowMuch, SRemainderSlot],
		string.to_int(SSelectedSlot, SelectedSlot),
		string.to_int(SHowMuch, HowMuch),
		string.to_int(SRemainderSlot, RemainderSlot)
	then
		Distribute = HowMuch / (VectorSize - 1),
		Remainder = HowMuch rem (VectorSize - 1),
		array.map_foldl(updateSlot(HowMuch, Distribute, Remainder, RemainderSlot, SelectedSlot, []), PCV, NextPCV, 0, _),
		testLoop(NextPCV, !IO)
	else
		ebea.player.selection.pcv.test(!IO)
	).


:- pred testLoop(int, int, probabilityCombinationVector, probabilityCombinationVector, R, R, io.state, io.state) <= ePRNG(R).
:- mode testLoop(in, in, in, array_di, in, out, di, uo) is det.

testLoop(Percentage, Trial, Parent, Offspring, !Random, !IO) :-
	Remaining = int.min(Percentage * array.size(Parent) / 100, array.size(Offspring)),
	copyCombination(Remaining, array.size(Parent) - 1, array.copy(Parent), Offspring, NewOffspring, !Random),
%	copyPercentageCombinations(Percentage, Parent, Offspring, NewOffspring, !Random),
	io.print(NewOffspring, !IO),
	io.nl(!IO),
	(if
		Trial < 10
	then
		testLoop(Percentage, Trial + 1, Parent, NewOffspring, !Random, !IO)
	else
		true
	).

:- end_module ebea.player.selection.pcv.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
