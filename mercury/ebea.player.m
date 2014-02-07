/**
 * 

 * @author Pedro Mariano
 * @version 1.0 2012/07/ 4
 */
:- module ebea.player.

:- interface.

:- include_module age, energy, selection.
:- import_module ebea.player.age, ebea.player.energy, ebea.player.selection.
:- import_module chromosome, rng, rng.distribution.
:- import_module foldable, parseable, printable.
:- import_module userInterface.
:- import_module io, maybe, list.


/**
 * The chromosome of an EBEA player.  It is represented in a list type
 * structure.  The first node contains the genes related to the energy
 * process.  The second node contains the genes related to partner
 * selection process.  The last node contains the genes that control the
 * behaviour in the game being used.  Type parameter {@code C} represents
 * the game strategy.
 */
:- type chromosome(S) --->
	chromosome(
		ageGenes       :: ebea.player.age.chromosome,
		energyGenes    :: ebea.player.energy.chromosome,
		selectionGenes :: ebea.player.selection.chromosome,
		strategyGenes  :: S
	).

:- type traits --->
	traits(
		ageTrait       :: ebea.player.age.trait,
		energyTrait    :: ebea.player.energy.trait,
		selectionTrait :: ebea.player.selection.traits
	).

:- type traits(_) == ebea.player.traits.

/**
 * Parameters that regulate the behaviour of a player in an Energy Based
 * Evolutionary Algorithm.  The parameters are stored in a list type
 * structure.  There is a node for parameters related to the energy
 * behaviour, parameters related to the partner selection process, and
 * parameters specific to the game being used.
 */
:- type parameters(P) --->
	parameters(
		mutationProbability  :: float,
		agePar               :: ebea.player.age.parameters,
		energyPar            :: ebea.player.energy.parameters,
%		opinionPar           :: ebea.player.opinion.parameters,
		selectionPar         :: ebea.player.selection.parameters,
		gamePar              :: P
	).

/**
 * Represents a player in the Energy Based Evolutionary Algorithm.  The
 * player contains it chromosome and it traits.  Type parameters {@code
 * C} and {@code T} represent the chromosome and the traits of the game
 * being used in EBEA.  The chromosome also contains genes related to the
 * energy component and selection process of the EBEA.  The same is true
 * for the phenotype.
 */
:- type player(C, T) --->
	player(
		id          :: int,
		siteIndex   :: int,
		chromosome  :: ebea.player.chromosome(C),
		traits      :: ebea.player.traits(T)
	).

/**
 * Accumulator used to reduce the genomes of a collection of players.
 */
:- type ac(A).

:- instance printable(ac(A)) <= printable(A).

:- func defaultChromosome(S) = chromosome(S).

/**
 * init(Parameters, Chromosome, ID, SiteIndex)
  
 * Initialise a player given its chromosome.
 */
:- pred init(ebea.player.parameters(P), ebea.player.chromosome(C), int, int, player(C, T), R, R)
	<= (chromosome(C, T, P), ePRNG(R)).
:- mode init(in, in, in, in, out, in, out) is det.


:- func dialog(list(dialogItem(CS))) = list(dialogItem(ebea.player.chromosome(CS))).

/**
 * Return the strategy used by this player.
 */
:- func strategy(player(C, T)) = C.

/**
 * Return the identification of a player.
 */
:- func 'ID'(player(C, T)) = int.

/**
 * reproduce(Parameters, Parent, NextParent, NewID, Offspring,, !Random)

 * Player {@code Parent} is going to reproduce and give to birth {@code
 * Offspring}.  The parent is unchanged.  Parameter {@code Parameters} is
 * used in the reproduction process, in this case, if there is a
 * probability to occur an one-point mutation.
 */

:- pred reproduce(ebea.player.parameters(P), player(C, T), player(C, T), int, maybe(player(C, T)), distribution, distribution, R, R)
	<= (ePRNG(R), chromosome(C, T, P)).
:- mode reproduce(in, in, out, in, out, in, out, in, out) is det.

/**
 * Given a list of players, return a random list of player's identification.
 */
:- pred randomIDs(int, list(int), list(player(C, T)), R, R)
	<= ePRNG(R).
:- mode randomIDs(in, out, in, in, out) is det.

/**
 * update(ID, UpdateFunc, !Players)
  
 * Given a list of players, update the one with the given identification,
 * using the provided function.  The player to be updated is passed to the
 * function.
  
 */
:- pred update(int, func(player(C, T)) = player(C, T), list(player(C, T)), list(player(C, T))).
:- mode update(in, in, in, out) is det.

/**
 * player(Players, ID) = Result

 * Given a list of players and an identification, return the player with
 * that identification.  Throws an exception if there is no such player.
  
 */
:- func player(list(player(C, T)), int) = player(C, T).

/**
 * Return the initial value of the accumulator.
 */
:- func initAc = ac(A) <= foldable(C, A).

/**
 * Update the accumulator with the new player info.
 */
:- func foldChromosome(player(C, T), ac(A)) = ac(A)
	<= (chromosome(C, T, P), foldable(C, A)).

/**
 * Print a player to the given stream.
 */
:- pred print(io.output_stream, player(C, T), io, io)
	<= (chromosome(C, T, P), printable(C)).
:- mode print(in, in, di, uo) is det.

:- pred printChromosome(io.output_stream, player(C, T), io, io)
	<= (chromosome(C, T, P), printable(C)).
:- mode printChromosome(in, in, di, uo) is det.

:- pred printTraits(io.output_stream, player(C, T), io, io)
	<= (chromosome(C, T, P)).
:- mode printTraits(in, in, di, uo) is det.

:- pred parseChromosome(chromosome(C), list(int), list(int))
	<= parseable(C).
:- mode parseChromosome(in, out, in) is det.
:- mode parseChromosome(out, in, out) is semidet.

:- implementation.

:- import_module bool, exception, float, int, list, string.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

:- type ac(A) --->
	ac(
		ebea.player.age.ac,
		ebea.player.energy.ac,
		ebea.player.selection.ac,
		A
	).

:- instance printable(ac(A)) <= printable(A)
	where
	[
		pred(print/4) is printAc
	].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%:- instance chromosome(ebea.player.chromosome(C), ebea.player.traits, ebea.player.parameters(P), ebea.player.ac(A))
%	<= chromosome(C, T, P, A).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

defaultChromosome(StrategyChromosome) =
	chromosome(
		ebea.player.age.defaultChromosome,
		ebea.player.energy.defaultChromosome,
		ebea.player.selection.defaultChromosome,
		StrategyChromosome
	).

init(Parameters, Chromosome, ID, SiteIndex, Result, !Random) :-
	Result^id = ID,
	Result^siteIndex = SiteIndex,
	Result^chromosome = Chromosome,
	Result^traits = traits(
		chromosome.born(Parameters^agePar,       Chromosome^ageGenes),
		chromosome.born(Parameters^energyPar,    Chromosome^energyGenes),
		SelectionTrait),
	ebea.player.selection.born(Chromosome^selectionGenes, SelectionTrait, !Random).


dialog(DialogStrategyGenes) =
	[
%	di(label("age genes"),        'new editField'(  get_ageGenes,        set(set_ageGenes),       ebea.player.age.dialogChromosome)),
%	di(label("energy genes"),     'new editField'(  get_energyGenes,     set(set_energyGenes),    ebea.player.energy.dialogChromosome)),
	di(label("selection genes"),  'new selectOneOf'(  get_selectionGenes,  set(set_selectionGenes), ebea.player.selection.dialogChromosome)),
	di(label("strategy genes"),   'new editField'(  get_strategyGenes,   set(set_strategyGenes),  DialogStrategyGenes))
	].



strategy(Player) = Result :-
	Result = Player^chromosome^strategyGenes.

'ID'(Player) = Result :-
	Result = Player^id.

reproduce(Parameters, Parent, NextParent, NewID, MOffspring, !Distribution, !Random) :-
	(if
		ebea.player.energy.canReproduce(Parent, Parameters, NextEnergyTraits)
	then
		rng.flipCoin(Parameters^mutationProbability, Mutation, !Random),
		(
			Mutation = yes,
			rng.nextInt(0, ebea.player.numberGenes(Parent^chromosome) - 1, MutatedGeneIndex, !Random),
			ebea.player.mutateGene(Parameters, MutatedGeneIndex, !Distribution, !Random, Parent^chromosome, NewChromosome)
			;
			Mutation = no,
			NewChromosome = Parent^chromosome
		),
		init(Parameters, NewChromosome, NewID, Parent^siteIndex, NewBorn, !Random),
		ebea.player.selection.teachKnowHow(
			Parameters^selectionPar,
			Parent,
			NewBorn,
			Offspring
		), 
		NextTraits = 'energyTrait :='(Parent^traits, NextEnergyTraits),
		NextParent = 'traits :='(Parent, NextTraits),
		MOffspring = yes(Offspring)
	else
		NextParent = Parent,
		MOffspring = no
	).

randomIDs(HowMany, Result, Players, !Random) :-
	(if
		HowMany =< 0
	then
		Result = []
	else
		rng.nextInt(0, list.length(Players) - 1, Index, !Random),
		removeIndex(Index, Players, Player, RestPlayers),
		Result = [Player^id | RestResult],
		randomIDs(HowMany - 1, RestResult, RestPlayers, !Random)
	).

update(ID, UpdateFunc, Players, Result) :-
	Players = [],
	Result = []
	;
	Players = [Player | Rest],
	(if
		Player^id = ID
	then
		Result = [UpdateFunc(Player) | Rest]
	else
		update(ID, UpdateFunc, Rest, RestResult),
		Result = [Player | RestResult]
	).

player(Players, ID) = Result :-
	Players = [],
	throw("ebea.player.player/2: there is no such player identification in the list")
	;
	Players = [Player | Rest],
	(if
		Player^id = ID
	then
		Result = Player
	else
		Result = player(Rest, ID)
	).

initAc = Result :-
	Result = ac(
		foldable.initAC,
		foldable.initAC,
		foldable.initAC,
		foldable.initAC
		).

foldChromosome(Player, AC) = Result :-
	AC = ac(A, E, Se, St),
	Result = ac(
		foldable.fold(Player^chromosome^ageGenes, A),
		foldable.fold(Player^chromosome^energyGenes, E),
		foldable.fold(Player^chromosome^selectionGenes, Se),
		foldable.fold(Player^chromosome^strategyGenes, St)
		).

print(Stream, Player, !IO) :-
	io.print(Stream, Player^id, !IO),
	io.print(Stream, " ", !IO),
	printChromosome(Stream, Player, !IO),
	io.print(Stream, " ", !IO),
	printTraits(Stream, Player, !IO).

printChromosome(Stream, Player, !IO) :-
	printable.print(Stream, Player^chromosome^energyGenes, !IO),
	io.print(Stream, " ", !IO),
	printable.print(Stream, Player^chromosome^selectionGenes, !IO),
	io.print(Stream, " ", !IO),
	printable.print(Stream, Player^chromosome^strategyGenes, !IO).

printTraits(Stream, Player, !IO) :-
	printable.print(Stream, Player^traits^ageTrait, !IO),
	io.print(Stream, " ", !IO),
	printable.print(Stream, Player^traits^energyTrait, !IO),
	io.print(Stream, " ", !IO),
	printable.print(Stream, Player^traits^selectionTrait, !IO)
	.

parseChromosome(C) -->
	ebea.player.age.parseChromosome(C^ageGenes),
	ebea.player.energy.parseChromosome(C^energyGenes),
	ebea.player.selection.parseChromosome(C^selectionGenes),
	parseable.parse(C^strategyGenes).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

/**
 * numberGenes(chromosome(S)) = int.
 
 * Return the number of genes in the given player's chromosome.
 */
:- func numberGenes(chromosome(CS)) = int
	<= chromosome(CS, T, P).

numberGenes(Chromosome) = Result :-
	Result =
		  chromosome.numberGenes(Chromosome^ageGenes)
		+ chromosome.numberGenes(Chromosome^energyGenes)
		+ chromosome.numberGenes(Chromosome^selectionGenes)
		+ chromosome.numberGenes(Chromosome^strategyGenes).

:- pred mutateGene(ebea.player.parameters(P), int, distribution, distribution, R, R, ebea.player.chromosome(CS), ebea.player.chromosome(CS))
	<= (chromosome(CS, T, P), ePRNG(R)).
:- mode mutateGene(in, in, in, out, in, out, in, out) is det.

mutateGene(Parameters, Index, !Distribution, !Random, Chromosome, Result) :-
	(if
		Index < chromosome.numberGenes(Chromosome^ageGenes)
	then
		chromosome.mutateGene(Parameters^agePar, Index, !Distribution, !Random, Chromosome^ageGenes, ResultAge),
		Result = 'ageGenes :='(Chromosome, ResultAge)
	else if
		Index <
			  chromosome.numberGenes(Chromosome^ageGenes)
			+ chromosome.numberGenes(Chromosome^energyGenes)
	then
		ThisIndex = Index - chromosome.numberGenes(Chromosome^ageGenes),
		chromosome.mutateGene(Parameters^energyPar, ThisIndex, !Distribution, !Random, Chromosome^energyGenes, ResultEnergy),
		Result = 'energyGenes :='(Chromosome, ResultEnergy)
	else if
		Index <
			  chromosome.numberGenes(Chromosome^ageGenes)
			+ chromosome.numberGenes(Chromosome^energyGenes)
			+ chromosome.numberGenes(Chromosome^selectionGenes)
	then
		ThisIndex = Index
			- chromosome.numberGenes(Chromosome^ageGenes)
			- chromosome.numberGenes(Chromosome^energyGenes),
		chromosome.mutateGene(Parameters^selectionPar, ThisIndex, !Distribution, !Random, Chromosome^selectionGenes, ResultSelection),
		Result = 'selectionGenes :='(Chromosome, ResultSelection)
	else if
		Index <
			  chromosome.numberGenes(Chromosome^ageGenes)
			+ chromosome.numberGenes(Chromosome^energyGenes)
			+ chromosome.numberGenes(Chromosome^selectionGenes)
			+ chromosome.numberGenes(Chromosome^strategyGenes)
	then
		ThisIndex = Index
			- chromosome.numberGenes(Chromosome^ageGenes)
			- chromosome.numberGenes(Chromosome^energyGenes)
			- chromosome.numberGenes(Chromosome^selectionGenes),
		chromosome.mutateGene(Parameters^gamePar, ThisIndex, !Distribution, !Random, Chromosome^strategyGenes, ResultStrategy),
		Result = 'strategyGenes :='(Chromosome, ResultStrategy)
	else
		throw("ebea.player.mutateGene/8: Invalid gene index")
	).

:- pred removeIndex(int, list(T), T, list(T)).
:- mode removeIndex(in, in, out, out) is det.

removeIndex(Index, List, Element, Rest) :-
	(if
		Index = 0
	then
		List = [Element | Rest]
		;
		List = [],
		throw("removeIndex/4: invalid element index list pair")
	else
		List = [HeadRest | NextList],
		removeIndex(Index - 1, NextList, Element, TailRest),
		Rest = [HeadRest | TailRest]
		;
		List = [],
		throw("removeIndex/4: invalid element index list pair")
	).


:- pred printAc(io.output_stream, ebea.player.ac(A), io, io)
	<= printable(A).
:- mode printAc(in, in, di, uo) is det.


printAc(Stream, AC, !IO) :-
	AC = ac(A, E, Se, St),
	printable.print(Stream, A, !IO),
	io.print(Stream, " ", !IO),
	printable.print(Stream, E, !IO),
	io.print(Stream, " ", !IO),
	printable.print(Stream, Se, !IO),
	io.print(Stream, " ", !IO),
	printable.print(Stream, St, !IO).








:- func get_ageGenes(ebea.player.chromosome(CS)) = ebea.player.age.chromosome.

get_ageGenes(P) = P^ageGenes.


:- func set_ageGenes(ebea.player.chromosome(CS), ebea.player.age.chromosome) = ebea.player.chromosome(CS).

set_ageGenes(P, V) = 'ageGenes :='(P, V).



:- func get_energyGenes(ebea.player.chromosome(CS)) = ebea.player.energy.chromosome.

get_energyGenes(P) = P^energyGenes.


:- func set_energyGenes(ebea.player.chromosome(CS), ebea.player.energy.chromosome) = ebea.player.chromosome(CS).

set_energyGenes(P, V) = 'energyGenes :='(P, V).



:- func get_selectionGenes(ebea.player.chromosome(CS)) = ebea.player.selection.chromosome.

get_selectionGenes(P) = P^selectionGenes.


:- func set_selectionGenes(ebea.player.chromosome(CS), ebea.player.selection.chromosome) = ebea.player.chromosome(CS).

set_selectionGenes(P, V) = 'selectionGenes :='(P, V).



:- func get_strategyGenes(ebea.player.chromosome(CS)) = CS.

get_strategyGenes(P) = P^strategyGenes.


:- func set_strategyGenes(ebea.player.chromosome(CS), CS) = ebea.player.chromosome(CS).

set_strategyGenes(P, V) = 'strategyGenes :='(P, V).



:- end_module ebea.player.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
