/**
 * 

 * @author Pedro Mariano
 * @version 1.0 2012/07/ 4
 */
:- module ebea.player.

:- interface.

:- include_module chromosome, age, energy, selection.
:- import_module ebea.player.chromosome, ebea.player.age, ebea.player.energy, ebea.player.selection, ebea.player.selection.chromosome.
:- import_module ebea.population, ebea.population.players.
:- import_module chromosome, rng, rng.distribution.
:- import_module foldable, parseable, printable.
:- import_module userInterface.
:- import_module io, maybe, list.

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
		id          :: ebea.population.players.key,
		siteIndex   :: int,
		chromosome  :: ebea.player.chromosome.chromosome(C),
		traits      :: ebea.player.traits(T)
	).

/**
 * Accumulator used to reduce the genomes of a collection of players.
 */
%:- type ac(A).
:- type ac(A) --->
	ac(
		ebea.player.age.ac,
		ebea.player.energy.ac,
		ebea.player.selection.ac,
		A
	).

:- instance printable(ac(A)) <= printable(A).

/**
 * init(Parameters, Chromosome, ID, SiteIndex)
  
 * Initialise a player given its chromosome.
 */
:- pred init(
	ebea.player.parameters(P),
	ebea.player.chromosome.chromosome(C),
	ebea.population.players.key,
	int,
	player(C, T),
	R, R)
	<= (chromosome(C, T, P), ePRNG(R)).
:- mode init(in, in, in, in, out, in, out) is det.


/**
 * Return the strategy used by this player.
 */
:- func strategy(player(C, T)) = C.

/**
 * Return the identification of a player.
 */
:- func 'ID'(player(C, T)) = ebea.population.players.key.

/**
 * reproduce(Parameters, Parent, NextParent, NewID, Offspring,, !Random)

 * Player {@code Parent} is going to reproduce and give to birth {@code
 * Offspring}.  The parent is unchanged.  Parameter {@code Parameters} is
 * used in the reproduction process, in this case, if there is a
 * probability to occur an one-point mutation.
 */

:- pred reproduce(ebea.player.parameters(P), player(C, T), player(C, T), ebea.population.players.key, maybe(player(C, T)), distribution, distribution, R, R)
	<= (ePRNG(R), chromosome(C, T, P)).
:- mode reproduce(in, in, out, in, out, in, out, in, out) is det.

/**
 * Return the initial value of the accumulator.
 */
:- func initAc = ac(A) <= foldable(C, A).

/**
 * Update the accumulator with the new player info.
 */
:- func foldChromosome(player(C, T), ac(A)) = ac(A)
	<= foldable(C, A).
%	<= (chromosome(C, T, P), foldable(C, A)).

:- pred printAc(io.output_stream, ebea.player.ac(A), io, io)
	<= printable(A).
:- mode printAc(in, in, di, uo) is det.



/**
 * Print a player to the given stream.
 */
:- pred print(io.output_stream, player(C, T), io, io)
	<= (chromosome(C, T, P), printable(C)).
:- mode print(in, in, di, uo) is det.


:- pred printTraits(io.output_stream, player(C, T), io, io)
	<= (chromosome(C, T, P)).
:- mode printTraits(in, in, di, uo) is det.

:- implementation.

:- import_module bool, exception, float, int, list, string.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

% :- type ac(A) --->
% 	ac(
% 		ebea.player.age.ac,
% 		ebea.player.energy.ac,
% 		ebea.player.selection.ac,
% 		A
% 	).

:- instance printable(ac(A)) <= printable(A)
	where
	[
		pred(print/4) is printAc
	].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%:- instance chromosome(ebea.player.chromosome.chromosome(C), ebea.player.traits, ebea.player.parameters(P), ebea.player.ac(A))
%	<= chromosome(C, T, P, A).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

init(Parameters, Chromosome, ID, SiteIndex, Result, !Random) :-
	Result^id = ID,
	Result^siteIndex = SiteIndex,
	Result^chromosome = Chromosome,
	Result^traits = traits(
		chromosome.born(Parameters^agePar,       Chromosome^ageGenes),
		chromosome.born(Parameters^energyPar,    Chromosome^energyGenes),
		SelectionTrait),
	ebea.player.selection.born(Chromosome^selectionGenes, SelectionTrait, !Random).

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
			Offspring,
			!Random
		), 
		NextTraits = 'energyTrait :='(Parent^traits, NextEnergyTraits),
		NextParent = 'traits :='(Parent, NextTraits),
		MOffspring = yes(Offspring)
	else
		NextParent = Parent,
		MOffspring = no
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

printAc(Stream, AC, !IO) :-
	AC = ac(A, E, Se, St),
	printable.print(Stream, A, !IO),
	io.print(Stream, " ", !IO),
	printable.print(Stream, E, !IO),
	io.print(Stream, " ", !IO),
	printable.print(Stream, Se, !IO),
	io.print(Stream, " ", !IO),
	printable.print(Stream, St, !IO).

print(Stream, Player, !IO) :-
	io.print(Stream, Player^id, !IO),
	io.print(Stream, " ", !IO),
	ebea.player.chromosome.print(Stream, Player^chromosome, !IO),
	io.print(Stream, " ", !IO),
	printTraits(Stream, Player, !IO).

printTraits(Stream, Player, !IO) :-
	printable.print(Stream, Player^traits^ageTrait, !IO),
	io.print(Stream, " ", !IO),
	printable.print(Stream, Player^traits^energyTrait, !IO),
	io.print(Stream, " ", !IO),
	printable.print(Stream, Player^traits^selectionTrait, !IO)
	.

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

:- pred mutateGene(ebea.player.parameters(P), int, distribution, distribution, R, R, ebea.player.chromosome.chromosome(CS), ebea.player.chromosome.chromosome(CS))
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













:- end_module ebea.player.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
