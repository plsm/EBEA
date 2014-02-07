/**
 * 
 * This module is responsible for selecting partners for a player and
 * updating any player information after the game has played.

 * @author Pedro Mariano
 * @version 1.0 2012/09/14
 * @version 2.0 2013/12/30
 */
:- module ebea.player.selection.

:- interface.

:- include_module opinion.

:- include_module pcv.

:- import_module ebea.population.
:- import_module game.
:- import_module userInterface.

/**
 * The chromosome part that is responsible for partner selection.
 * Selection can be random, based on the partner selection algorithm
 * presented in Mariano 2009, or based on generic opinion.
 */
:- type chromosome --->
	random ;
	partnerSelection(
		poolSize                :: int ,
		bitsPerProbability      :: int ,
		probabilityUpdateFactor :: float ,
		payoffThreshold_PS      :: float
	) ;
	opinion(
		payoffThreshold_O  :: float,
		initialUncertainty :: float
	)
	.

/**
 * Return a default selection chromosome that can be used con construct a
 * player chromosome.
  
 */
:- func defaultChromosome = ebea.player.selection.chromosome.

:- func dialogChromosome = userInterface.options(ebea.player.selection.chromosome).

/**
 * The parameters that control the dynamics of the partner selection module
 * of Energy Based Evolutionary Algorithm.
 */
:- type parameters --->
	sp(
		poolSizeStdDev                :: float,
		bitsPerProbabilityStdDev      :: float,
		probabilityUpdateFactorStdDev :: float,
		payoffThresholdStdDev         :: float,
		
		uncertaintyIncreaseFactor     :: float,
		mu                            :: float
	).

:- func defaultParameters = ebea.player.selection.parameters.

:- func dialogParameters = list(dialogItem(ebea.player.selection.parameters)).



/**
 * The traits part that is responsible for partner selection.
 */
:- type traits.


/**
 * The accumulator used by type class functions {@code fold/0} and {@code fold/2}.
 */
:- type ac.

/**
 * Given the selection part of an EBEA chromosome return the individual
 * that can develop from this chromosome.
 */
:- pred born(ebea.player.selection.chromosome, ebea.player.selection.traits, R, R) <= ePRNG(R).
:- mode born(in, out, in, out) is det.


/**
 * roundSelectPartnersPlayGame(PlayerParameters, Game, Player, Neighbours, !NextRoundPopulation, !Random)

 * Select partners for player {@code Player} from collection {@code
 * Neighbours}.  Play game {@code Game} between the selected player
 * profile.  This is handled by predicate {@code ebea.energy.round}.  After
 * playing, update the traits of {@code Player}.

 * <p> This predicate performs the core of the selection process of an
 * Energy Based Evolutionary Algorithm.
  
 */
:- pred roundSelectPartnersPlayGame(
	ebea.player.parameters(P), G, player(C, T), list(player(C, T)),
	population(C, T), population(C, T),
	list(list(int)), list(list(int)),
	R, R)
	<= (game(G, C), ePRNG(R)).
:- mode roundSelectPartnersPlayGame(in, in, in, in, in, out, in, out, in, out) is det.

/**
 * roundCheckForDeadPlayers(Game, DeadPlayerIDs, Player, Neighbours, NextPlayer, !Random)

 * <p> Check if player {@code Player} has any reference to dead players,
 * and replace them by players from {@code Neighbours}.  This predicate is
 * called after a round of game playing and death and birth process.

 * <p> This predicate assumes that there are enough players in {@code Neighbours}.
 
 */
:- pred roundCheckForDeadPlayers(G, list(int), player(C, T), neighbours(C, T), player(C, T), R, R)
	<= (game(G, C), ePRNG(R)).
:- mode roundCheckForDeadPlayers(in, in, in, in, out, in, out) is det.


/**
 * teachKnowHow(Parent, !Offspring)

 * After a player has given birth to an offspring, he can pass some
 * selection knowledge to his child.
  
 */
:- pred teachKnowHow(ebea.player.selection.parameters, player(C, T), player(C, T), player(C, T)).
:- mode teachKnowHow(in, in, in, out) is det.

/**
 * scaledPayoffToThreshold(Game, EnergyScaling, Payoff) = Threshold

 * Transform a scaled payoff as determined by parameter {@code
 * EnergyScaling} to a threshold value which is a value between zero and
 * one.
  
 */
:- func scaledPayoffToThreshold(G, energyScaling, float) = float
	<= game(G, C).

:- pred parseChromosome(ebea.player.selection.chromosome, list(int), list(int)).
:- mode parseChromosome(in, out, in) is det.
:- mode parseChromosome(out, in, out) is semidet.

:- pred parseParameters(ebea.player.selection.parameters, list(int), list(int)).
:- mode parseParameters(in, out, in) is det.
:- mode parseParameters(out, in, out) is semidet.

:- pred parseTraits(ebea.player.selection.traits, list(int), list(int)).
:- mode parseTraits(in, out, in) is det.
:- mode parseTraits(out, in, out) is semidet.

:- instance chromosome(ebea.player.selection.chromosome, ebea.player.selection.traits, ebea.player.selection.parameters).

:- instance foldable(ebea.player.selection.chromosome, ebea.player.selection.ac).

:- instance printable(ebea.player.selection.chromosome).

:- instance printable(ebea.player.selection.traits).

:- instance printable(ebea.player.selection.ac).

:- implementation.

:- import_module ebea.player.selection.opinion, ebea.player.selection.pcv.
:- import_module parseable.
:- import_module array.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

:- type traits --->
	random
	;
	partnerSelection(
		pcv :: probabilityCombinationVector
	)
	;
	opinion(
		opinionValue :: float,
		uncertainty  :: float
	).


:- type ac --->
	ac(
		qty_PS                     :: int,
		sumPoolSize                :: int,
		sumBitsPerProbability      :: int,
		sumProbabilityUpdateFactor :: float,
		sumPayoffThreshold_PS      :: float,
		qty_O                      :: int,
		sumPayoffThreshold_O       :: float
	).

:- instance chromosome(ebea.player.selection.chromosome, ebea.player.selection.traits, ebea.player.selection.parameters)
	where
[
	func(numberGenes/1) is ebea.player.selection.numberGenes,
	pred(mutateGene/8)  is ebea.player.selection.mutateGene,
	func(born/2)        is ebea.player.selection.born
].

:- instance foldable(ebea.player.selection.chromosome, ebea.player.selection.ac)
	where
[
	func(fold/2)    is ebea.player.selection.fold,
	func(initAC/0)  is ebea.player.selection.fold
].

:- instance printable(ebea.player.selection.chromosome)
	where
[
	pred(print/4) is ebea.player.selection.printChromosome
].

:- instance printable(ebea.player.selection.traits)
	where
[
	pred(print/4) is ebea.player.selection.printTraits
].

:- instance printable(ebea.player.selection.ac)
	where
[
	pred(print/4) is ebea.player.selection.printAccumulator
].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

:- type probabilityCombinationVector == array(slot).

:- type slot --->
	slot(
		probability :: int,
		combination :: combination
	).

:- type selectionType --->
	random ;
	partnerSelection
	.

:- type combination == list(int).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

defaultChromosome = random.

dialogChromosome = options(
	getChromosomeIndex,
	[
	 ci(label("random"),             random, []),
	 ci(label("partner selection"),  partnerSelection(default_poolSize, default_bitsPerProbability, default_probabilityUpdateFactor, default_payoffThreshold_PS),
		[
		 di(label("pool size"),                  updateFieldInt(   get_poolSize,                 checkInt(   "pool size",                  bounded(0, yes), unbound,             set_poolSize))),
		 di(label("bits per probability"),       updateFieldInt(   get_bitsPerProbability,       checkInt(   "bits per probability",       bounded(1, yes), unbound,             set_bitsPerProbability))),
		 di(label("probability update factor"),  updateFieldFloat( get_probabilityUpdateFactor,  checkFloat( "probability update factor",  bounded(0.0, yes), bounded(1.0, yes), set_probabilityUpdateFactor))),
		 di(label("payoff threshold"),           updateFieldFloat( get_payoffThreshold_PS,       checkFloat( "payoff threshold",           bounded(-1.0, yes), bounded(1.0, yes), set_payoffThreshold_PS)))
		]),
	 ci(label("opinion"),           opinion(default_payoffThreshold_O, default_initialUncertainty),
		 [
		  di(label("payoff threshold (as fraction of game payoff)"),  updateFieldFloat( get_payoffThreshold_O,       checkFloat( "payoff threshold",     bounded(0.0, yes),  bounded(1.0, yes), set_payoffThreshold_O))),
		  di(label("initial uncertainty"),                            updateFieldFloat( get_initialUncertainty,      checkFloat( "initial uncertainty",  bounded(-1.0, yes), bounded(1.0, yes), set_initialUncertainty)))
		 ])
	]).
	% [
	% di(label("random"),            newValue(random)),
	% di(label("partner selection"),  subdialog( [
	% 	di(label("pool size"),                  updateFieldInt(   get_poolSize,                 checkInt(   "pool size",                  bounded(0, yes), unbound,             set_poolSize))),
	% 	di(label("bits per probability"),       updateFieldInt(   get_bitsPerProbability,       checkInt(   "bits per probability",       bounded(1, yes), unbound,             set_bitsPerProbability))),
	% 	di(label("probability update factor"),  updateFieldFloat( get_probabilityUpdateFactor,  checkFloat( "probability update factor",  bounded(0.0, yes), bounded(1.0, yes), set_probabilityUpdateFactor))),
	% 	di(label("payoff threshold"),           updateFieldFloat( get_payoffThreshold_PS,       checkFloat( "payoff threshold",           bounded(-1.0, yes), bounded(1.0, yes), set_payoffThreshold_PS)))
	% 	])),
	% di(label("opinion"),           subdialog( [
	% 	di(label("payoff threshold (as fraction of game payoff)"),  updateFieldFloat( get_payoffThreshold_O,       checkFloat( "payoff threshold",           bounded(-1.0, yes), bounded(1.0, yes), set_payoffThreshold_O)))
	% 	]))
	% ].


defaultParameters = Result :-
	Result^poolSizeStdDev = default_poolSizeStdDev,
	Result^bitsPerProbabilityStdDev = default_bitsPerProbabilityStdDev,
	Result^probabilityUpdateFactorStdDev = default_probabilityUpdateFactorStdDev,
	Result^payoffThresholdStdDev = default_payoffThresholdStdDev,
	Result^uncertaintyIncreaseFactor = default_uncertaintyIncreaseFactor,
	Result^mu = default_mu.

dialogParameters =
	[
	di(label("pool size std dev"),                  updateFieldFloat( get_poolSizeStdDev,                 checkFloat( "pool size stddev",                  bounded(0.0, yes), unbound, set_poolSizeStdDev))),
	di(label("bits per probability std dev"),       updateFieldFloat( get_bitsPerProbabilityStdDev,       checkFloat( "bits per probability stddev",       unbound, unbound, set_bitsPerProbabilityStdDev))),
	di(label("probability update factor std dev"),  updateFieldFloat( get_probabilityUpdateFactorStdDev,  checkFloat( "probability update factor stddev",  unbound, unbound, set_probabilityUpdateFactorStdDev))),
	di(label("payoff threshold std dev"),           updateFieldFloat( get_payoffThresholdStdDev,          checkFloat( "payoff threshold stddev",           unbound, unbound, set_payoffThresholdStdDev))),
	di(label("uncertainty increase factor"),        updateFieldFloat( get_uncertaintyIncreaseFactor,      checkFloat( "uncertainty increase factor",       bounded(1.0, yes), unbound, set_uncertaintyIncreaseFactor))),
	di(label("mu"),                                 updateFieldFloat( get_mu,                             checkFloat( "mu",                                bounded(0.0, no),  unbound, set_mu)))
	].

born(Chromosome, Result, !Random) :-
	Chromosome = random,
	Result = random
	;
	Chromosome = partnerSelection(_, _, _, _),
	(if
		Chromosome^poolSize = 0
	then
		PCV = array.make_empty_array
	else
		Quotient = (1 << Chromosome^bitsPerProbability) / Chromosome^poolSize,
		Remainder = (1 << Chromosome^bitsPerProbability) rem Chromosome^poolSize,
		PCV = array.generate(
			Chromosome^poolSize,
			ebea.player.selection.pcv.initProbabilityVector(Quotient, Remainder))
	),
	Result = partnerSelection(PCV)
	;
	Chromosome = opinion(_, _),
	rng.nextFloat(OV, !Random),
	Result^opinionValue = 2.0 * OV - 1.0,
	Result^uncertainty = Chromosome^initialUncertainty
	.



roundSelectPartnersPlayGame(
	PlayerParameters, Game, Player, Neighbours,
	!NextRoundPopulation,
	!PlayerProfile,
	!Random
) :-
	NumberPartners = game.numberPlayers(Game) - 1,
	Chromosome = Player^chromosome^selectionGenes,
	Traits = Player^traits^selectionTrait,
	(if
		NumberPartners > list.length(Neighbours)
	then
		true
	else
		Chromosome = random,
		(if
			Traits = random
		then
			/* create the strategy profile */
			rng.randomElementsList(NumberPartners, Neighbours, RestProfile, !Random),
			/* play the game */
			ebea.player.energy.stepPlayGame(PlayerParameters^energyPar, Game, Player, RestProfile, _Payoffs, !NextRoundPopulation, !PlayerProfile, !Random),
			/* no selection traits to update */
			true
		else
			throw("ebea.player.selection.round/8: Invalid combination of chromosome and phenotipic trait")
		)
		;
		Chromosome = partnerSelection(_, _, _, _),
		(if
			Traits = partnerSelection(_)
		then
			/* create the strategy profile */
			select(NumberPartners, Neighbours, Traits^pcv, !Random, SelectedSlot, RestProfileIDs, RestProfile),
			/* play the game */
			ebea.player.energy.stepPlayGame(PlayerParameters^energyPar, Game, Player, RestProfile, Payoffs, !NextRoundPopulation, !PlayerProfile, !Random),
			/* update the selection traits */
			PlayerPayoff = array.lookup(Payoffs, 0),
			ebea.player.selection.pcv.updateProbCombVectors(
				Game, PlayerParameters^energyPar^energyScaling,
				NumberPartners, Chromosome, RestProfileIDs, Neighbours, SelectedSlot, PlayerPayoff,
				!Random,
				Traits^pcv, NextProbCombVector
			),
			ebea.population.update(
				Player^id,
				ebea.player.selection.pcv.updatePlayerProbCombVectors(NextProbCombVector),
				!NextRoundPopulation)
		else
			throw("ebea.player.selection.round/8: Invalid combination of chromosome and phenotipic trait")
		)
		;
		Chromosome = opinion(_, _),
		(if
			Traits = opinion(_, _)
		then
			% /* create the strategy profile */
			% ebea.player.selection.opinion.selectPartners(Traits, NumberPartners, Neighbours, RestProfile, !Random),
			% /* play the game */
			% ebea.player.energy.stepPlayGame(PlayerParameters^energyPar, Game, Player, RestProfile, Payoffs, !NextRoundPopulation, !PlayerProfile, !Random),
			% /* update the selection traits */
			% ebea.player.selection.opinion.updateOpinions(Game, PlayerParameters, [Player | RestProfile], Payoffs, !NextRoundPopulation)

			/* create the strategy profile */
			rng.randomElementsList(NumberPartners, Neighbours, RestProfile, !Random),
			(if
				ebea.player.selection.opinion.checkPartnerOpinion(Traits, RestProfile)
			then
				/* play the game */
				ebea.player.energy.stepPlayGame(PlayerParameters^energyPar, Game, Player, RestProfile, Payoffs, !NextRoundPopulation, !PlayerProfile, !Random),
				/* update the selection traits */
				ebea.player.selection.opinion.updateOpinions(Game, PlayerParameters, [Player | RestProfile], Payoffs, !NextRoundPopulation)
			else
				ebea.population.update(
					Player^id,
					ebea.player.selection.opinion.increaseUncertainty(PlayerParameters^selectionPar),
					!NextRoundPopulation)
			)
		else
			throw("ebea.player.selection.round/8: Invalid combination of chromosome and traits")
		)
	).

roundCheckForDeadPlayers(Game, DeadPlayerIDs, Player, Neighbours, NextPlayer, !Random) :-
	NumberPartners = game.numberPlayers(Game) - 1,
	Traits = Player^traits^selectionTrait,
	(
		Traits = random,
		NextPlayer = Player
		;
		Traits = partnerSelection(PCV),
		array.map_foldl(
			ebea.player.selection.pcv.checkForDeadPlayers(NumberPartners, Neighbours, DeadPlayerIDs),
			PCV, NextPCV,
			!Random),
		NextTraits = 'pcv :='(Traits, NextPCV),
		NextPlayerTraits = 'selectionTrait :='(Player^traits, NextTraits),
		NextPlayer = 'traits :='(Player, NextPlayerTraits)
		;
		Traits = opinion(_, _),
		NextPlayer = Player
	).

teachKnowHow(Parameters, Parent, !Offspring) :-
	ParentTrait = Parent^traits^selectionTrait,
	(
		ParentTrait = random
		;
		ParentTrait = partnerSelection(_)
		;
		ParentTrait = opinion(_, _),
		!:Offspring = 'traits :='(
			!.Offspring,
			'selectionTrait :='(
				!.Offspring^traits,
				OffspringTrait
			)
		),
		OffspringTrait^opinionValue = ParentTrait^opinionValue,
		OffspringTrait^uncertainty = float.min(2.0, ParentTrait^uncertainty * Parameters^uncertaintyIncreaseFactor)
	)
	.

%:- pragma memo(scaledPayoffToThreshold/3).

scaledPayoffToThreshold(Game, unscaled, Payoff) = Result :-
	Scale = game.highestPayoff(Game) - game.lowestPayoff(Game),
	Result = (Payoff - game.lowestPayoff(Game)) / Scale.

scaledPayoffToThreshold(_Game, scaled, Payoff) = (Payoff + 1.0) / 2.0.

scaledPayoffToThreshold(_Game, scaledPositive, Payoff) = Payoff.

parseChromosome(C) -->
	{C = random},
	[0]
	;
	{C = partnerSelection(_, _, _, _)},
	[1, C^poolSize, C^bitsPerProbability],
	parseable.float32(C^probabilityUpdateFactor),
	parseable.float32(C^payoffThreshold_PS)
	;
	{C = opinion(_, _)},
	[2],
	parseable.float32(C^payoffThreshold_O),
	parseable.float32(C^initialUncertainty)
	.

parseParameters(P) -->
	parseable.float32(P^poolSizeStdDev),
	parseable.float32(P^bitsPerProbabilityStdDev),
	parseable.float32(P^probabilityUpdateFactorStdDev),
	parseable.float32(P^payoffThresholdStdDev),
	parseable.float32(P^uncertaintyIncreaseFactor),
	parseable.float32(P^mu).

parseTraits(T) -->
	{T = random},
	[0]
	;
	{T = partnerSelection(PCV)},
	[1],
	ebea.player.selection.pcv.parse(PCV)
	;
	{T = opinion(_, _)},
	[2],
	parseable.float32(T^opinionValue),
	parseable.float32(T^uncertainty)
	.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions


/**
 * Return the number of genes in the given selection chromosome.
 */
:- func numberGenes(ebea.player.selection.chromosome) = int.

numberGenes(Chromosome) = Result :-
	Chromosome = random,
	Result = 0
	;
	Chromosome = partnerSelection(_, _, _, _),
	Result = 4
	;
	Chromosome = opinion(_, _),
	Result = 1
	.

/**
 * mutateGene(Parameters, Index, !Distribution, !Random, Chromosome, Result)

 * Mutate the {@code Index}th gene of {@code Chromosome} and the return the
 * result in {@code Result}.  Accumulator parameters {@code Distribution}
 * and {@code Random} are used to calculate the new gene value.
 */

:- pred mutateGene(ebea.player.selection.parameters, int, distribution, distribution, R, R, ebea.player.selection.chromosome, ebea.player.selection.chromosome)
	<= ePRNG(R).
:- mode mutateGene(in, in, in, out, in, out, in, out) is det.

mutateGene(Parameters, Index, !Distribution, !Random, Chromosome, Result) :-
	Chromosome = random,
	throw("ebea.player.selection.mutateGene/8: No genes to mutate in chromosome random")
	;
	Chromosome = partnerSelection(_, _, _, _),
	(if
		Index = 0
	then
		rng.distribution.unitGaussian(Perturb0, !Distribution, !Random),
		Perturb = Perturb0 * Parameters^poolSizeStdDev,
		NextPoolSize = int.max(0, Chromosome^poolSize + float.round_to_int(Perturb)),
		Result = 'poolSize :='(Chromosome, NextPoolSize)
	else if
		Index = 1
	then
		rng.distribution.unitGaussian(Perturb0, !Distribution, !Random),
		Perturb = Perturb0 * Parameters^bitsPerProbabilityStdDev,
		NextBitsPerProbability = int.max(0, Chromosome^bitsPerProbability + float.round_to_int(Perturb)),
		Result = 'bitsPerProbability :='(Chromosome, NextBitsPerProbability)
	else if
		Index = 2
	then
		rng.distribution.unitGaussian(Perturb0, !Distribution, !Random),
		Perturb = Perturb0 * Parameters^probabilityUpdateFactorStdDev,
		NextProbabilityUpdateFactor = float.max(0.0, float.min(Chromosome^probabilityUpdateFactor + Perturb, 1.0)),
		Result = 'probabilityUpdateFactor :='(Chromosome, NextProbabilityUpdateFactor)
	else if
		Index = 3
	then
		rng.distribution.unitGaussian(Perturb0, !Distribution, !Random),
		Perturb = Perturb0 * Parameters^payoffThresholdStdDev,
		NextPayoffThreshold = float.max(0.0, float.min(Chromosome^payoffThreshold_PS + Perturb, 1.0)),
		Result = 'payoffThreshold_PS :='(Chromosome, NextPayoffThreshold)
	else
		throw("ebea.player.selection.mutateGene/8: invalid gene index for partnerSelection chromosome")
	)
	;
	Chromosome = opinion(_, _),
	(if
		Index = 0
	then
		rng.distribution.unitGaussian(Perturb0, !Distribution, !Random),
		Perturb = Perturb0 * Parameters^payoffThresholdStdDev,
		NextPayoffThreshold = float.max(0.0, float.min(Chromosome^payoffThreshold_O + Perturb, 1.0)),
		Result = 'payoffThreshold_O :='(Chromosome, NextPayoffThreshold)
	else
		throw("ebea.player.selection.mutateGene/8: invalid gene index for opinion chromosome")
	)
	.

/**
 * Given the selection part of an EBEA chromosome return the individual
 * that can develop from this chromosome.
 */
:- func born(ebea.player.selection.parameters, ebea.player.selection.chromosome) = ebea.player.selection.traits.

born(_, Chromosome) = Result :-
	Chromosome = random,
	Result = random
	;
	Chromosome = partnerSelection(_, _, _, _),
	(if
		Chromosome^poolSize = 0
	then
		PCV = array.make_empty_array
	else
		Quotient = (1 << Chromosome^bitsPerProbability) / Chromosome^poolSize,
		Remainder = (1 << Chromosome^bitsPerProbability) rem Chromosome^poolSize,
		PCV = array.generate(
			Chromosome^poolSize,
			ebea.player.selection.pcv.initProbabilityVector(Quotient, Remainder))
	),
	Result = partnerSelection(PCV)
	;
	Chromosome = opinion(_, _),
	Result^opinionValue = 0.0,
	Result^uncertainty = Chromosome^initialUncertainty
	.

:- func fold = ebea.player.selection.ac.

fold = ac(0, 0, 0, 0.0, 0.0, 0, 0.0).


:- func fold(ebea.player.selection.chromosome, ebea.player.selection.ac) = ebea.player.selection.ac.

fold(Chromosome, AC) = Result :-
	Chromosome = random,
	Result = AC
	;
	Chromosome = partnerSelection(_, _, _, _),
	Result^qty_PS = AC^qty_PS + 1,
	Result^sumPoolSize = AC^sumPoolSize + Chromosome^poolSize,
	Result^sumBitsPerProbability = AC^sumBitsPerProbability + Chromosome^bitsPerProbability,
	Result^sumProbabilityUpdateFactor = AC^sumProbabilityUpdateFactor + Chromosome^probabilityUpdateFactor,
	Result^sumPayoffThreshold_PS = AC^sumPayoffThreshold_PS + Chromosome^payoffThreshold_PS,
	Result^qty_O = AC^qty_O,
	Result^sumPayoffThreshold_O = AC^sumPayoffThreshold_O
	;
	Chromosome = opinion(_, _),
	Result =
	'sumPayoffThreshold_O :='(
	'qty_O :='(
		AC,
		AC^qty_O + 1
	),
		AC^sumPayoffThreshold_O + Chromosome^payoffThreshold_O
	)
	.









% :- pred initCombinationVector(int, list(player(C, T)), slot, slot, R, R)
% 	<= ePRNG(R).
% :- mode initCombinationVector(in, in, in, out, in, out) is det.

% initCombinationVector(CombinationSize, Neighbours, Slot, MappedSlot, !Random) :-
% 	ebea.player.randomIDs(CombinationSize, Combination, Neighbours, !Random),
% 	MappedSlot = 'combination :='(Slot, Combination).


/**
 * Print the selection part of an EBEA chromosome.
 */
:- pred printChromosome(io.output_stream, ebea.player.selection.chromosome, io, io).
:- mode printChromosome(in, in, di, uo) is det.

printChromosome(Stream, Chromosome, !IO) :-
	Chromosome = random,
	io.print(Stream, "random", !IO)
	;
	Chromosome = partnerSelection(_, _, _, _),
	io.print(Stream, Chromosome^poolSize, !IO),
	io.print(Stream, " ", !IO),
	io.print(Stream, Chromosome^bitsPerProbability, !IO),
	io.print(Stream, " ", !IO),
	io.print(Stream, Chromosome^probabilityUpdateFactor, !IO),
	io.print(Stream, " ", !IO),
	io.print(Stream, Chromosome^payoffThreshold_PS, !IO)
	;
	Chromosome = opinion(_, _),
	io.print(Stream, Chromosome^payoffThreshold_O, !IO)
	.


/**
 * Print the traits of the selection gene group of an EBEA chromosome.
 */
:- pred printTraits(io.output_stream, ebea.player.selection.traits, io, io).
:- mode printTraits(in, in, di, uo) is det.


printTraits(Stream, Traits, !IO) :-
	Traits = random
	;
	Traits = partnerSelection(PCV),
	array.foldl2(printSlot(Stream), PCV, yes, _, !IO)
%	io.print(Stream, PCV, !IO)
	;
	Traits = opinion(_, _),
	io.print(Stream, Traits^opinionValue, !IO),
	io.print(Stream, " ", !IO),
	io.print(Stream, Traits^uncertainty, !IO)
	.

:- pred printAccumulator(io.output_stream, ebea.player.selection.ac, io, io).
:- mode printAccumulator(in, in, di, uo) is det.

printAccumulator(Stream, AC, !IO) :-
	(if
		AC^qty_PS = 0
	then
		io.print(Stream, "1/0 1/0 1/0 1/0", !IO)
	else
		FQty_PS = float(AC^qty_PS),
		io.format(Stream, "%f %f %f %f",
			[f(float(AC^sumPoolSize) / FQty_PS),
			 f(float(AC^sumBitsPerProbability) / FQty_PS),
			 f(AC^sumProbabilityUpdateFactor / FQty_PS),
			 f(AC^sumPayoffThreshold_PS / FQty_PS)], !IO)
	),
	io.print(Stream, '\t', !IO),
	(if
		AC^qty_O = 0
	then
		io.print(Stream, "1/0", !IO)
	else
		FQty_O = float(AC^qty_O),
		io.print(Stream, AC^sumPayoffThreshold_O / FQty_O, !IO)
	).

/**
 * Print a slot of the probability and combination vectors.
 */
:- pred printSlot(io.output_stream, slot, bool, bool, io, io).
:- mode printSlot(in, in, in, out, di, uo) is det.

printSlot(Stream, Slot, Flag, no, !IO) :-
	( % switch
		Flag = yes
		;
		Flag = no,
		io.print(Stream, " ", !IO)
	),
	io.print(Stream, Slot^probability, !IO),
	io.print(Stream, " ", !IO),
	PrintIDs =
	(pred(ID::in, Fin::in, Fout::out, IOdi::di, IOuo::uo) is det :-
		Fout = no,
		(
			Fin = yes,
			IO1 = IOdi
			;
			Fin = no,
			io.print(Stream, ",", IOdi, IO1)
		),
		io.print(Stream, ID, IO1, IOuo)
	),
	list.foldl2(PrintIDs, Slot^combination, yes, _, !IO).


:- func getChromosomeIndex(ebea.player.selection.chromosome) = maybe(int).

getChromosomeIndex(random) = yes(0).
getChromosomeIndex(partnerSelection(_, _, _, _)) = yes(1).
getChromosomeIndex(opinion(_, _)) = yes(2).



:- func get_poolSize(ebea.player.selection.chromosome) = int.

get_poolSize(P) = R :-
	P = random,
	R = default_poolSize
	;
	P = partnerSelection(_, _, _, _),
	R = P^poolSize
	;
	P = opinion(_, _),
	R = default_poolSize
	.

:- func set_poolSize(ebea.player.selection.chromosome, int) = ebea.player.selection.chromosome.

set_poolSize(P, V) = R :-
	P = random,
	R = partnerSelection(V, default_bitsPerProbability, default_probabilityUpdateFactor, default_payoffThreshold_PS)
	;
	P = partnerSelection(_, _, _, _),
	R = 'poolSize :='(P, V)
	;
	P = opinion(_, _),
	R = partnerSelection(V, default_bitsPerProbability, default_probabilityUpdateFactor, default_payoffThreshold_PS)
	.


:- func get_bitsPerProbability(ebea.player.selection.chromosome) = int.

get_bitsPerProbability(P) = R :-
	P = random,
	R = default_bitsPerProbability
	;
	P = partnerSelection(_, _, _, _),
	R = P^bitsPerProbability
	;
	P = opinion(_, _),
	R = default_bitsPerProbability
	.

:- func set_bitsPerProbability(ebea.player.selection.chromosome, int) = ebea.player.selection.chromosome.

set_bitsPerProbability(P, V) = R :-
	P = random,
	R = partnerSelection(default_poolSize, V, default_probabilityUpdateFactor, default_payoffThreshold_PS)
	;
	P = partnerSelection(_, _, _, _),
	R = 'bitsPerProbability :='(P, V)
	;
	P = opinion(_, _),
	R = partnerSelection(default_poolSize, V, default_probabilityUpdateFactor, default_payoffThreshold_PS)
	.


:- func get_probabilityUpdateFactor(ebea.player.selection.chromosome) = float.

get_probabilityUpdateFactor(P) = R :-
	P = random,
	R = default_probabilityUpdateFactor
	;
	P = partnerSelection(_, _, _, _),
	R = P^probabilityUpdateFactor
	;
	P = opinion(_, _),
	R = default_probabilityUpdateFactor
	.

:- func set_probabilityUpdateFactor(ebea.player.selection.chromosome, float) = ebea.player.selection.chromosome.

set_probabilityUpdateFactor(P, V) = R :-
	P = random,
	R = partnerSelection(default_poolSize, default_bitsPerProbability, V, default_payoffThreshold_PS)
	;
	P = partnerSelection(_, _, _, _),
	R = 'probabilityUpdateFactor :='(P, V)
	;
	P = opinion(_, _),
	R = partnerSelection(default_poolSize, default_bitsPerProbability, V, default_payoffThreshold_PS)
	.


:- func get_payoffThreshold_PS(ebea.player.selection.chromosome) = float.

get_payoffThreshold_PS(P) = R :-
	P = random,
	R = default_payoffThreshold_PS
	;
	P = partnerSelection(_, _, _, _),
	R = P^payoffThreshold_PS
	;
	P = opinion(_, _),
	R = default_payoffThreshold_PS
	.

:- func set_payoffThreshold_PS(ebea.player.selection.chromosome, float) = ebea.player.selection.chromosome.

set_payoffThreshold_PS(P, V) = R :-
	P = random,
	R = partnerSelection(default_poolSize, default_bitsPerProbability, default_probabilityUpdateFactor, V)
	;
	P = partnerSelection(_, _, _, _),
	R = 'payoffThreshold_PS :='(P, V)
	;
	P = opinion(_, _),
	R = partnerSelection(default_poolSize, default_bitsPerProbability, default_probabilityUpdateFactor, V)
	.


:- func get_payoffThreshold_O(ebea.player.selection.chromosome) = float.

get_payoffThreshold_O(P) = R :-
	P = random,
	R = default_payoffThreshold_O
	;
	P = partnerSelection(_, _, _, _),
	R = default_payoffThreshold_O
	;
	P = opinion(_, _),
	R = P^payoffThreshold_O
	.

:- func set_payoffThreshold_O(ebea.player.selection.chromosome, float) = ebea.player.selection.chromosome.

set_payoffThreshold_O(P, V) = R :-
	P = random,
	R = opinion(V, default_initialUncertainty)
	;
	P = partnerSelection(_, _, _, _),
	R = opinion(V, default_initialUncertainty)
	;
	P = opinion(_, _),
	R = 'payoffThreshold_O :='(P, V)
	.

:- func get_initialUncertainty(ebea.player.selection.chromosome) = float.

get_initialUncertainty(P) = R :-
	P = random,
	R = default_initialUncertainty
	;
	P = partnerSelection(_, _, _, _),
	R = default_initialUncertainty
	;
	P = opinion(_, _),
	R = P^initialUncertainty
	.

:- func set_initialUncertainty(ebea.player.selection.chromosome, float) = ebea.player.selection.chromosome.

set_initialUncertainty(P, V) = R :-
	P = random,
	R = opinion(default_payoffThreshold_O, V)
	;
	P = partnerSelection(_, _, _, _),
	R = opinion(default_payoffThreshold_O, V)
	;
	P = opinion(_, _),
	R = 'initialUncertainty :='(P, V)
	.






:- func default_poolSize = int.

default_poolSize = 0.

:- func default_bitsPerProbability = int.

default_bitsPerProbability = 1.

:- func default_probabilityUpdateFactor = float.

default_probabilityUpdateFactor = 0.0.

:- func default_payoffThreshold_PS = float.

default_payoffThreshold_PS = 0.0.


:- func default_payoffThreshold_O = float.

default_payoffThreshold_O = 0.0.

:- func default_initialUncertainty = float.

default_initialUncertainty = 1.0.





:- func get_poolSizeStdDev(ebea.player.selection.parameters) = float.

get_poolSizeStdDev(P) = P^poolSizeStdDev.


:- func set_poolSizeStdDev(ebea.player.selection.parameters, float) = ebea.player.selection.parameters.

set_poolSizeStdDev(P, V) = 'poolSizeStdDev :='(P, V).



:- func get_bitsPerProbabilityStdDev(ebea.player.selection.parameters) = float.

get_bitsPerProbabilityStdDev(P) = P^bitsPerProbabilityStdDev.


:- func set_bitsPerProbabilityStdDev(ebea.player.selection.parameters, float) = ebea.player.selection.parameters.

set_bitsPerProbabilityStdDev(P, V) = 'bitsPerProbabilityStdDev :='(P, V).



:- func get_probabilityUpdateFactorStdDev(ebea.player.selection.parameters) = float.

get_probabilityUpdateFactorStdDev(P) = P^probabilityUpdateFactorStdDev.


:- func set_probabilityUpdateFactorStdDev(ebea.player.selection.parameters, float) = ebea.player.selection.parameters.

set_probabilityUpdateFactorStdDev(P, V) = 'probabilityUpdateFactorStdDev :='(P, V).



:- func get_payoffThresholdStdDev(ebea.player.selection.parameters) = float.

get_payoffThresholdStdDev(P) = P^payoffThresholdStdDev.


:- func set_payoffThresholdStdDev(ebea.player.selection.parameters, float) = ebea.player.selection.parameters.

set_payoffThresholdStdDev(P, V) = 'payoffThresholdStdDev :='(P, V).


:- func get_uncertaintyIncreaseFactor(ebea.player.selection.parameters) = float.

get_uncertaintyIncreaseFactor(P) = P^uncertaintyIncreaseFactor.


:- func set_uncertaintyIncreaseFactor(ebea.player.selection.parameters, float) = ebea.player.selection.parameters.

set_uncertaintyIncreaseFactor(P, V) = 'uncertaintyIncreaseFactor :='(P, V).


:- func get_mu(ebea.player.selection.parameters) = float.

get_mu(P) = P^mu.


:- func set_mu(ebea.player.selection.parameters, float) = ebea.player.selection.parameters.

set_mu(P, V) = 'mu :='(P, V).



:- func default_poolSizeStdDev = float.

default_poolSizeStdDev = 1.0.

:- func default_bitsPerProbabilityStdDev = float.

default_bitsPerProbabilityStdDev = 1.0.

:- func default_probabilityUpdateFactorStdDev = float.

default_probabilityUpdateFactorStdDev = 0.1.

:- func default_payoffThresholdStdDev = float.

default_payoffThresholdStdDev = 0.1.

:- func default_uncertaintyIncreaseFactor = float.

default_uncertaintyIncreaseFactor = 1.0.

:- func default_mu = float.

default_mu = 0.5.



:- end_module ebea.player.selection.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
