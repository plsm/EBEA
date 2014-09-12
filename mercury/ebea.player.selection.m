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
:- include_module chromosome.
:- include_module wv.

:- import_module ebea.player.selection.chromosome.
:- import_module ebea.player.selection.pcv.
:- import_module ebea.player.selection.wv.
:- import_module ebea.population, ebea.population.neighbours.
:- import_module game.
:- import_module userInterface.
:- import_module array.

/**
 * The parameters that control the dynamics of the partner selection module
 * of Energy Based Evolutionary Algorithm.
 */
:- type parameters --->
	sp(
		poolSizeStdDev                 :: float,
		bitsPerProbabilityStdDev       :: float,
		probabilityUpdateFactorStdDev  :: float,
		payoffThresholdStdDev          :: float,
		
		uncertaintyIncreaseFactor      :: float,
		mu                             :: float,
		poolSizePercentageTransmission :: int
	).

:- func defaultParameters = ebea.player.selection.parameters.

:- func dialogParameters = list(dialogItem(ebea.player.selection.parameters)).

/**
 * The traits part that is responsible for partner selection.
 */
:- type traits --->
	random
	;
	partnerSelection(
		pcv :: probabilityCombinationVector,
		mwv :: maybe(weightVector)
	)
	;
	opinion(
		opinionValue :: float,
		uncertainty  :: float
	).

/**
 * The accumulator used by type class functions {@code fold/0} and {@code fold/2}.
 */
:- type ac.

/**
 * Given the selection part of an EBEA chromosome return the individual
 * that can develop from this chromosome.
 */
:- pred born(ebea.player.selection.chromosome.chromosome, ebea.player.selection.traits, R, R) <= ePRNG(R).
:- mode born(in, out, in, out) is det.


/**
 * stepSelectPartnersPlayGame2(PlayerParameters, Game, Player, Neighbours, !NextRoundPopulation, !Random)

 * Select partners for player {@code Player} from collection {@code
 * Neighbours}.  Play game {@code Game} between the selected player
 * profile.  This is handled by predicate {@code ebea.energy.round}.  After
 * playing, update the traits of {@code Player}.

 * <p> This predicate performs the core of the selection process of an
 * Energy Based Evolutionary Algorithm.
  
 */
:- pred stepSelectPartnersPlayGame2(
	ebea.player.parameters(P), G, player(C, T), ebea.population.neighbours.neighbours,
	population(C, T), population(C, T),
	list(list(ebea.population.players.key)), list(list(ebea.population.players.key)),
	R, R)
	<= (asymmetricGame(G, C), ePRNG(R)).
:- mode stepSelectPartnersPlayGame2(in, in, in, in, in, out, in, out, in, out) is det.

%% ************************************************************************
%%
%% stepSelectPartnersPlayGame3(PlayerParameters, Game, Player, Neighbours, !NextRoundPopulation, !PlayerProfiles, !SiteActionAccumulator, !Random)
%%
%% Select partners for player {@code Player} from collection {@code
%% Neighbours} using a game that returns the action done by players.  Play
%% game {@code Game} between the selected player profile.  This is handled
%% by predicate {@code ebea.energy.round}.  After playing, update the
%% traits of {@code Player}.
%%
%% <p> This predicate performs the core of the selection process of an
%% Energy Based Evolutionary Algorithm.
%%
%% @param P player parameters which contains game and energy parameters.
%%
%% @param G The game player by players
%%
%% @param CS The game strategy and strategy genes.
%%
%% @param T  The player phenotype that results from the strategy genes.
%%
%% @param R The pseudo-random number generator.
%%
%% @param A The game actions.
%%
:- pred stepSelectPartnersPlayGame3(
	ebea.player.parameters(P)             :: in,
	G                                     :: in,
	player(CS, T)                         :: in,
	ebea.population.neighbours.neighbours :: in,
	population(CS, T)                       :: in,  population(CS, T)                       :: out,
	list(list(ebea.population.players.key)) :: in,  list(list(ebea.population.players.key)) :: out,
	R                                       :: in,  R                                       :: out,
	array(AA) :: di,  array(AA):: uo
) is det
	<= (
	asymmetricGame(G, CS, A),
	ePRNG(R),
	foldable(A, AA)
).

%% ****************************************************************************
%% stepProcessBornPlayersCheckForDeadPlayers(Game, DeadPlayerIDs, NewBornIDs, Neighbours, !Player, !Random)
%%
%% Updates the selection traits given the id of dead and new born players.
%%
%% <p> In random partner selection nothing needs to be done.
%%
%% <p> In partner selection based in probability and combination vectors,
%% combinations with dead players are removed and replaced by a new
%% combination if there are sufficient neighbours.  If this selection mode
%% has a weight vector, weights of dead players are removed and newborns
%% are assigned a weight equal to the game Pareto payoff.
%%
%% <p> In opinion based selection nothing needs to be done.
%%
%% TODO: decrease uncertainty
%%
:- pred stepProcessBornPlayersCheckForDeadPlayers(
	G                                     :: in,
	list(ebea.population.players.key)     :: in,
	list(ebea.population.players.key)     :: in,
	ebea.population.neighbours.neighbours :: in,
	player(C, T) :: in,  player(C, T) :: out,
	R            :: in,  R            :: out
) is det
	<= (
	abstractGame(G),
	ePRNG(R)
).

/**
 * teachKnowHow(Parameters, Parent, !Offspring)

 * After a player has given birth to an offspring, he can pass some
 * selection knowledge to his child.
  
 */
:- pred teachKnowHow(ebea.player.selection.parameters, player(C, T), player(C, T), player(C, T), R, R)
	<= ePRNG(R).
:- mode teachKnowHow(in, in, in, out, in, out) is det.

/**
 * scaledPayoffToThreshold(Game, EnergyScaling, Payoff) = Threshold

 * Transform a scaled payoff as determined by parameter {@code
 * EnergyScaling} to a threshold value which is a value between zero and
 * one.
  
 */
:- func scaledPayoffToThreshold(G, energyScaling, float) = float
	<= abstractGame(G).

% :- pred parseChromosome(ebea.player.selection.chromosome.chromosome, list(int), list(int)).
% :- mode parseChromosome(in, out, in) is det.
% :- mode parseChromosome(out, in, out) is semidet.

:- pred parseParameters(ebea.player.selection.parameters, list(int), list(int)).
:- mode parseParameters(in, out, in) is det.
:- mode parseParameters(out, in, out) is semidet.

:- pred parseTraits(ebea.player.selection.traits, list(int), list(int)).
:- mode parseTraits(in, out, in) is det.
:- mode parseTraits(out, in, out) is semidet.

:- instance chromosome(ebea.player.selection.chromosome.chromosome, ebea.player.selection.traits, ebea.player.selection.parameters).

:- instance foldable(ebea.player.selection.chromosome.chromosome, ebea.player.selection.ac).

:- instance printable(ebea.player.selection.traits).

:- instance printable(ebea.player.selection.ac).

:- implementation.

:- import_module ebea.player.selection.opinion, ebea.player.selection.pcv.
:- import_module parseable.
:- import_module array, solutions.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types



:- type ac --->
	ac(
		qty_NPS                    :: int,
		qty_WPS                    :: int,
		sumPoolSize                :: int,
		sumBitsPerProbability      :: int,
		sumProbabilityUpdateFactor :: float,
		sumPayoffThreshold_PS      :: float,
		qty_O                      :: int,
		sumPayoffThreshold_O       :: float
	).

:- instance chromosome(ebea.player.selection.chromosome.chromosome, ebea.player.selection.traits, ebea.player.selection.parameters)
	where
[
	func(numberGenes/1) is ebea.player.selection.chromosome.numberGenes,
	pred(mutateGene/8)  is ebea.player.selection.chromosome.mutateGene,
	func(born/2)        is ebea.player.selection.chromosome.born
].

:- instance foldable(ebea.player.selection.chromosome.chromosome, ebea.player.selection.ac)
	where
[
	func(fold/2)    is ebea.player.selection.fold,
	func(initAC/0)  is ebea.player.selection.fold
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


:- type selectionType --->
	random ;
	partnerSelection
	.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

defaultParameters = Result :-
	Result^poolSizeStdDev = default_poolSizeStdDev,
	Result^bitsPerProbabilityStdDev = default_bitsPerProbabilityStdDev,
	Result^probabilityUpdateFactorStdDev = default_probabilityUpdateFactorStdDev,
	Result^payoffThresholdStdDev = default_payoffThresholdStdDev,
	Result^uncertaintyIncreaseFactor = default_uncertaintyIncreaseFactor,
	Result^mu = default_mu,
	Result^poolSizePercentageTransmission = 0.

dialogParameters =
	[
	di(label("pool size std dev"),                       updateFieldFloat( get_poolSizeStdDev,                 checkFloat( "pool size stddev",                  bounded(0.0, yes), unbound, set_poolSizeStdDev))),
	di(label("bits per probability std dev"),            updateFieldFloat( get_bitsPerProbabilityStdDev,       checkFloat( "bits per probability stddev",       unbound, unbound, set_bitsPerProbabilityStdDev))),
	di(label("probability update factor std dev"),       updateFieldFloat( get_probabilityUpdateFactorStdDev,  checkFloat( "probability update factor stddev",  unbound, unbound, set_probabilityUpdateFactorStdDev))),
	di(label("payoff threshold std dev"),                updateFieldFloat( get_payoffThresholdStdDev,          checkFloat( "payoff threshold stddev",           unbound, unbound, set_payoffThresholdStdDev))),
	di(label("uncertainty increase factor"),             updateFieldFloat( get_uncertaintyIncreaseFactor,      checkFloat( "uncertainty increase factor",       bounded(1.0, yes), unbound, set_uncertaintyIncreaseFactor))),
	di(label("mu"),                                      updateFieldFloat( get_mu,                             checkFloat( "mu",                                bounded(0.0, no),  unbound, set_mu))),
	di(label("% combination slots passed to offspring"), updateFieldInt(   get_poolSizePercentageTransmission,  checkInt( "percentage",                          bounded(0, yes),  bounded(100, yes), set_poolSizePercentageTransmission)))
	].

born(Chromosome, Result, !Random) :-
	Chromosome = random,
	Result = random
	;
	(
		Chromosome = normalPartnerSelection(PS)
	;
		Chromosome = weightedPartnerSelection(PS)
	),
	(if
		PS^poolSize = 0
	then
		PCV = array.make_empty_array
	else
		Quotient = (1 << PS^bitsPerProbability) / PS^poolSize,
		Remainder = (1 << PS^bitsPerProbability) rem PS^poolSize,
		PCV = array.generate(
			PS^poolSize,
			ebea.player.selection.pcv.initProbabilityVector(Quotient, Remainder))
	),
	(
		Chromosome = normalPartnerSelection(_),
		Result = partnerSelection(PCV, no)
	;
		Chromosome = weightedPartnerSelection(_),
		Result = partnerSelection(PCV, yes(ebea.player.selection.wv.init))
	 )
	;
	Chromosome = opinion_old(_, _),
	rng.nextFloat(OV, !Random),
	Result^opinionValue = 2.0 * OV - 1.0,
	Result^uncertainty = float.max(0.0, Chromosome^initialUncertainty + 0.5 * UV - 0.25),
	rng.nextFloat(UV, !Random)
	;
	Chromosome = opinion_old(_, _, _, _),
	rng.distribution.unitGaussian(InitialOpinion0, rng.distribution.init, Dst, !Random),
	InitialOpinion1 = InitialOpinion0 * Chromosome^initialStdDevOpinion + Chromosome^initialAverageOpinion,
	Result^opinionValue = float.max(-1.0, float.min(InitialOpinion1, 1.0)),
	rng.distribution.unitGaussian(InitialUncertainty0, Dst, _, !Random),
	InitialUncertainty1 = InitialUncertainty0 * Chromosome^initialStdDevUncertainty + Chromosome^initialAverageUncertainty,
	Result^uncertainty = float.max(0.0, float.min(InitialUncertainty1, 2.0))
	.





stepSelectPartnersPlayGame2(
	PlayerParameters, Game, Player, Neighbours,
	!NextRoundPopulation,
	!PlayerProfile,
	!Random
) :-
	NumberPartners = game.numberPlayers(Game) - 1,
	Chromosome = Player^chromosome^selectionGenes,
	Traits = Player^traits^selectionTrait,
	(if
		NumberPartners > ebea.population.neighbours.size(Neighbours)
	then
		true
	else
		Chromosome = random,
		(if
			Traits = random
		then
			/* create the strategy profile */
			ebea.population.neighbours.randomElements(NumberPartners, Neighbours, PartnersID, !Random),
			list.map(ebea.population.players.player(!.NextRoundPopulation^players), PartnersID) = RestProfile,
			/* play the game */
			ebea.player.energy.stepPlayGame2(PlayerParameters^energyPar, Game, Player, RestProfile, _MPayoffs, !NextRoundPopulation, !PlayerProfile, !Random),
			/* no selection traits to update */
			true
		else
			throw("ebea.player.selection.stepSelectPartnersPlayGame2/10: Invalid combination of chromosome and phenotipic trait")
		)
		;
		Chromosome = normalPartnerSelection(PS),
		(if
			Traits = partnerSelection(_, no)
		then
			/* create the strategy profile */
			ebea.player.selection.pcv.select(NumberPartners, Neighbours, Traits^pcv, !Random, SelectedSlot, PartnersIDs),
			list.map(ebea.population.players.player(!.NextRoundPopulation^players), PartnersIDs) = RestProfile,
			/* play the game */
			ebea.player.energy.stepPlayGame2(PlayerParameters^energyPar, Game, Player, RestProfile, MPayoffs, !NextRoundPopulation, !PlayerProfile, !Random),
			/* update the selection traits */
			(
				MPayoffs = yes(Payoffs),
				PlayerPayoff = array.lookup(Payoffs, 0)
				;
				MPayoffs = no,
				PlayerPayoff = game.lowestPayoff(Game)
			),
			ebea.player.selection.pcv.updateProbCombVectors(
				Game, PlayerParameters^energyPar^energyScaling,
				PS, PartnersIDs, SelectedSlot, PlayerPayoff,
				ebea.population.neighbours.randomElements(NumberPartners, Neighbours),
				!Random,
				Traits^pcv, NextProbCombVector
			),
			ebea.population.update(
				Player^id,
				ebea.player.selection.pcv.updatePlayerProbCombVectors(NextProbCombVector),
				!NextRoundPopulation
			)
		else
			throw("ebea.player.selection.stepSelectPartnersPlayGame2/10: Invalid combination of chromosome and phenotipic trait")
		)
		;
		Chromosome = weightedPartnerSelection(PS),
		(if
			Traits = partnerSelection(_, yes(OldWeightVector))
		then
			/* create the strategy profile */
			ebea.player.selection.pcv.select(NumberPartners, Neighbours, Traits^pcv, !Random, SelectedSlot, PartnersIDs),
			list.map(ebea.population.players.player(!.NextRoundPopulation^players), PartnersIDs) = RestProfile,
			/* play the game */
			ebea.player.energy.stepPlayGame2(PlayerParameters^energyPar, Game, Player, RestProfile, MPayoffs, !NextRoundPopulation, !PlayerProfile, !Random),
			/* update the selection traits */
			(
				MPayoffs = yes(Payoffs),
				PlayerPayoff = array.lookup(Payoffs, 0),
				WeightFactor = PlayerPayoff - game.lowestPayoff(Game)
				;
				MPayoffs = no,
				PlayerPayoff = game.lowestPayoff(Game),
				WeightFactor = 0.0
			),
			ebea.player.selection.pcv.probabilityFloat(PS, Traits^pcv, SelectedSlot, ProbabilitySelectedCombination),
			list.foldl(
				ebea.player.selection.wv.updateWeight(
					ProbabilitySelectedCombination,
					WeightFactor),
				PartnersIDs,
				OldWeightVector,
				NextWeightVector),
			ebea.player.selection.pcv.updateProbCombVectors(
				Game, PlayerParameters^energyPar^energyScaling,
				PS, PartnersIDs, SelectedSlot, PlayerPayoff,
				ebea.player.selection.wv.drawAsList(NextWeightVector, NumberPartners),
				!Random,
				Traits^pcv, NextProbCombVector
			),
			ebea.population.update(
				Player^id,
				ebea.player.selection.wv.updatePlayerProbCombVectorsWeightVector(NextProbCombVector, NextWeightVector),
				!NextRoundPopulation
			)
		else
			throw("ebea.player.selection.stepSelectPartnersPlayGame/10: Invalid combination of chromosome and phenotipic trait")
		)
		;
		(
			Chromosome = opinion_old(_, _) ;
			Chromosome = opinion_old(_, _, _, _)
		),
		(if
			Traits = opinion(_, _)
		then
			(if
				/* create the strategy profile */
				ebea.player.selection.opinion.selectPartners(
					Traits,
					NumberPartners,
					!.NextRoundPopulation^players,
					Neighbours,
					RestProfile,
					!Random)
			then
				/* play the game */
				ebea.player.energy.stepPlayGame2(PlayerParameters^energyPar, Game, Player, RestProfile, MPayoffs, !NextRoundPopulation, !PlayerProfile, !Random),
				/* update the selection traits */
				(
					MPayoffs = yes(Payoffs),
					ebea.player.selection.opinion.updateOpinions(Game, PlayerParameters, [Player | RestProfile], Payoffs, !NextRoundPopulation)
					;
					MPayoffs = no
				)
			else
				/* update the selection traits */
				ebea.population.update(
					Player^id,
					ebea.player.selection.opinion.increaseUncertainty(PlayerParameters^selectionPar),
					!NextRoundPopulation)
			)
		else
			throw("ebea.player.selection.stepSelectPartnersPlayGame/10: Invalid combination of chromosome and traits")
		)
	).


stepSelectPartnersPlayGame3(
	PlayerParameters, Game, Player, Neighbours,
	!NextRoundPopulation,
	!PlayerProfile,
	!Random,
	!SiteActionAccumulator
) :-
	NumberPartners = game.numberPlayers(Game) - 1,
	Chromosome = Player^chromosome^selectionGenes,
	Traits = Player^traits^selectionTrait,
	(if
		NumberPartners > ebea.population.neighbours.size(Neighbours)
	then
		true
	else
		Chromosome = random,
		(if
			Traits = random
		then
			/* create the strategy profile */
			ebea.population.neighbours.randomElements(NumberPartners, Neighbours, PartnersID, !Random),
			list.map(ebea.population.players.player(!.NextRoundPopulation^players), PartnersID) = RestProfile,
			/* play the game */
			ebea.player.energy.stepPlayGame3(PlayerParameters^energyPar, Game, Player, RestProfile, _MPayoffs, !NextRoundPopulation, !PlayerProfile, !Random, !SiteActionAccumulator),
			/* no selection traits to update */
			true
		else
			throw("ebea.player.selection.stepSelectPartnersPlayGame/10: Invalid combination of chromosome and phenotipic trait")
		)
		;
		Chromosome = normalPartnerSelection(PS),
		(if
			Traits = partnerSelection(_, no)
		then
			/* create the strategy profile */
			ebea.player.selection.pcv.select(NumberPartners, Neighbours, Traits^pcv, !Random, SelectedSlot, PartnersIDs),
			list.map(ebea.population.players.player(!.NextRoundPopulation^players), PartnersIDs) = RestProfile,
			/* play the game */
			ebea.player.energy.stepPlayGame3(PlayerParameters^energyPar, Game, Player, RestProfile, MPayoffs, !NextRoundPopulation, !PlayerProfile, !Random, !SiteActionAccumulator),
			/* update the selection traits */
			(
				MPayoffs = yes(Payoffs),
				PlayerPayoff = array.lookup(Payoffs, 0)
				;
				MPayoffs = no,
				PlayerPayoff = game.lowestPayoff(Game)
			),
			ebea.player.selection.pcv.updateProbCombVectors(
				Game, PlayerParameters^energyPar^energyScaling,
				PS, PartnersIDs, SelectedSlot, PlayerPayoff,
				ebea.population.neighbours.randomElements(NumberPartners, Neighbours),
				!Random,
				Traits^pcv, NextProbCombVector
			),
			ebea.population.update(
				Player^id,
				ebea.player.selection.pcv.updatePlayerProbCombVectors(NextProbCombVector),
				!NextRoundPopulation
			)
		else
			throw("ebea.player.selection.stepSelectPartnersPlayGame/10: Invalid combination of chromosome and phenotipic trait")
		)
		;
		Chromosome = weightedPartnerSelection(PS),
		(if
			Traits = partnerSelection(_, yes(OldWeightVector))
		then
			/* create the strategy profile */
			ebea.player.selection.pcv.select(NumberPartners, Neighbours, Traits^pcv, !Random, SelectedSlot, PartnersIDs),
			list.map(ebea.population.players.player(!.NextRoundPopulation^players), PartnersIDs) = RestProfile,
			/* play the game */
			ebea.player.energy.stepPlayGame3(PlayerParameters^energyPar, Game, Player, RestProfile, MPayoffs, !NextRoundPopulation, !PlayerProfile, !Random, !SiteActionAccumulator),
			/* update the selection traits */
			(
				MPayoffs = yes(Payoffs),
				PlayerPayoff = array.lookup(Payoffs, 0),
				WeightFactor = PlayerPayoff - game.lowestPayoff(Game)
				;
				MPayoffs = no,
				PlayerPayoff = game.lowestPayoff(Game),
				WeightFactor = 0.0
			),
			ebea.player.selection.pcv.probabilityFloat(PS, Traits^pcv, SelectedSlot, ProbabilitySelectedCombination),
			list.foldl(
				ebea.player.selection.wv.updateWeight(
					ProbabilitySelectedCombination,
					WeightFactor),
				PartnersIDs,
				OldWeightVector,
				NextWeightVector),
			ebea.player.selection.pcv.updateProbCombVectors(
				Game, PlayerParameters^energyPar^energyScaling,
				PS, PartnersIDs, SelectedSlot, PlayerPayoff,
				ebea.player.selection.wv.drawAsList(NextWeightVector, NumberPartners),
				!Random,
				Traits^pcv, NextProbCombVector
			),
			ebea.population.update(
				Player^id,
				ebea.player.selection.wv.updatePlayerProbCombVectorsWeightVector(NextProbCombVector, NextWeightVector),
				!NextRoundPopulation
			)
		else
			throw("ebea.player.selection.stepSelectPartnersPlayGame/10: Invalid combination of chromosome and phenotipic trait")
		)
		;
		(
			Chromosome = opinion_old(_, _) ;
			Chromosome = opinion_old(_, _, _, _)
		),
		(if
			Traits = opinion(_, _)
		then
			(if
				/* create the strategy profile */
				ebea.player.selection.opinion.selectPartners(
					Traits,
					NumberPartners,
					!.NextRoundPopulation^players,
					Neighbours,
					RestProfile,
					!Random)
			then
				/* play the game */
				ebea.player.energy.stepPlayGame3(PlayerParameters^energyPar, Game, Player, RestProfile, MPayoffs, !NextRoundPopulation, !PlayerProfile, !Random, !SiteActionAccumulator),
				/* update the selection traits */
				(
					MPayoffs = yes(Payoffs),
					ebea.player.selection.opinion.updateOpinions(Game, PlayerParameters, [Player | RestProfile], Payoffs, !NextRoundPopulation)
					;
					MPayoffs = no
				)
			else
				ebea.population.update(
					Player^id,
					ebea.player.selection.opinion.increaseUncertainty(PlayerParameters^selectionPar),
					!NextRoundPopulation)
			)
		else
			throw("ebea.player.selection.stepSelectPartnersPlayGame/10: Invalid combination of chromosome and traits")
		)
	).

stepProcessBornPlayersCheckForDeadPlayers(Game, DeadPlayerIDs, NewBornIDs, Neighbours, !Player, !Random) :-
	NumberPartners = game.numberPlayers(Game) - 1,
	Traits = !.Player^traits^selectionTrait,
	(
		Traits = random
	;
		Traits = partnerSelection(PCV, MWV),
		array.map_foldl(
			ebea.player.selection.pcv.checkForDeadPlayers(NumberPartners, Neighbours, DeadPlayerIDs),
			PCV, NextPCV,
			!Random),
		(	%
			MWV = no,
			TmpTraits = Traits
		;
			MWV = yes(OldWeightVector),
			NewElements = solutions.solutions(
				(pred(E::out) is nondet :-
					list.member(E, NewBornIDs),
					ebea.population.neighbours.member(E, Neighbours)
				)
			),
			ebea.player.selection.wv.addElements(
				NewElements,
				game.paretoPayoff(Game),
				OldWeightVector, TmpWeightVector
			),
			ebea.player.selection.wv.removeElements(DeadPlayerIDs, TmpWeightVector, NewWeightVector),
			TmpTraits = 'mwv :='(Traits, yes(NewWeightVector))
		),
		NextTraits = 'pcv :='(TmpTraits, NextPCV),
		NextPlayerTraits = 'selectionTrait :='(!.Player^traits, NextTraits),
		!:Player = 'traits :='(!.Player, NextPlayerTraits)
	;
		Traits = opinion(_, _)
	).

teachKnowHow(Parameters, Parent, !Offspring, !Random) :-
	ParentTrait = Parent^traits^selectionTrait,
	(
		ParentTrait = random
		;
		ParentTrait = partnerSelection(_ParentPCV, _ParentMWV),
		(if
			!.Offspring^traits^selectionTrait = partnerSelection(_OldPCV, _OldMWV)
		then
			ebea.player.selection.pcv.copyPercentageCombinations(Parameters, Parent, !Offspring, !Random)
		else
			throw("teachKnowHow/6: Never reached")
		)
		;
		ParentTrait = opinion(_, _),
		(if
			!.Offspring^traits^selectionTrait = opinion(_OpinionValue, _Uncertainty)
		then
			!:Offspring = 'traits :='(
				!.Offspring,
				'selectionTrait :='(
					!.Offspring^traits,
					OffspringTrait
				)
			),
			OffspringTrait^opinionValue = ParentTrait^opinionValue,
			OffspringTrait^uncertainty = float.min(2.0, ParentTrait^uncertainty * Parameters^uncertaintyIncreaseFactor)
		else
			throw("teachKnowHow/6: Never reached")
		)
	%,
	%	OffspringTrait^uncertainty = float.min(2.0, ParentTrait^uncertainty * Parameters^uncertaintyIncreaseFactor)
	)
	.

%:- pragma memo(scaledPayoffToThreshold/3).

scaledPayoffToThreshold(Game, unscaled, Payoff) = Result :-
	Scale = game.highestPayoff(Game) - game.lowestPayoff(Game),
	Result = (Payoff - game.lowestPayoff(Game)) / Scale.

scaledPayoffToThreshold(_Game, scaled, Payoff) = (Payoff + 1.0) / 2.0.

scaledPayoffToThreshold(_Game, scaledPositive, Payoff) = Payoff.

parseParameters(P) -->
	parseable.float32(P^poolSizeStdDev),
	parseable.float32(P^bitsPerProbabilityStdDev),
	parseable.float32(P^probabilityUpdateFactorStdDev),
	parseable.float32(P^payoffThresholdStdDev),
	parseable.float32(P^uncertaintyIncreaseFactor),
	parseable.float32(P^mu),
	parseable.int8(P^poolSizePercentageTransmission)
	.

parseTraits(T) -->
	{T = random},
	[0]
	;
	{T = partnerSelection(PCV, no)},
	[1],
	ebea.player.selection.pcv.parse(PCV)
	;
	{T = partnerSelection(PCV, yes(WV))},
	[3],
	ebea.player.selection.pcv.parse(PCV),
	ebea.player.selection.wv.parse(WV)
	;
	{T = opinion(_, _)},
	[2],
	parseable.float32(T^opinionValue),
	parseable.float32(T^uncertainty)
	.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- pred stepSelectPartnersPlayGame(
	pred(
		player(CS, T),
		list(player(CS, T)),
		maybe(array(float)),
		population(CS, T),  population(CS, T),
		R,                  R,
		A,                  A
		) ::
		in(pred(in, in, out, in, out, in, out, in, out) is det),
	ebea.player.parameters(P)             :: in,
	G                                     :: in,
	player(CS, T)                         :: in,
	ebea.population.neighbours.neighbours :: in,
	population(CS, T)                       :: in,  population(CS, T)                       :: out,
	R                                       :: in,  R                                       :: out,
	A                                       :: in,  A                                       :: out
) is det
	<= (
	abstractGame(G),
	ePRNG(R)
).

stepSelectPartnersPlayGame(
	PredPlayGame,
	PlayerParameters,
	Game,
	Player,
	Neighbours,
	!NextRoundPopulation,
	!Random,
	!Extra
) :-
	NumberPartners = game.numberPlayers(Game) - 1,
	Chromosome = Player^chromosome^selectionGenes,
	Traits = Player^traits^selectionTrait,
	(if
		NumberPartners > ebea.population.neighbours.size(Neighbours)
	then
		true
	else
		Chromosome = random,
		(if
			Traits = random
		then
			/* create the strategy profile */
			ebea.population.neighbours.randomElements(NumberPartners, Neighbours, PartnersID, !Random),
			list.map(ebea.population.players.player(!.NextRoundPopulation^players), PartnersID) = RestProfile,
			/* play the game */
			PredPlayGame(Player, RestProfile, _MPayoffs, !NextRoundPopulation, !Random, !Extra),
			/* no selection traits to update */
			true
		else
			throw("ebea.player.selection.stepSelectPartnersPlayGame2/10: Invalid combination of chromosome and phenotipic trait")
		)
		;
		Chromosome = normalPartnerSelection(PS),
		(if
			Traits = partnerSelection(_, no)
		then
			/* create the strategy profile */
			ebea.player.selection.pcv.select(NumberPartners, Neighbours, Traits^pcv, !Random, SelectedSlot, PartnersIDs),
			list.map(ebea.population.players.player(!.NextRoundPopulation^players), PartnersIDs) = RestProfile,
			/* play the game */
			PredPlayGame(Player, RestProfile, MPayoffs, !NextRoundPopulation, !Random, !Extra),
			/* update the selection traits */
			(
				MPayoffs = yes(Payoffs),
				PlayerPayoff = array.lookup(Payoffs, 0)
				;
				MPayoffs = no,
				PlayerPayoff = game.lowestPayoff(Game)
			),
			ebea.player.selection.pcv.updateProbCombVectors(
				Game, PlayerParameters^energyPar^energyScaling,
				PS, PartnersIDs, SelectedSlot, PlayerPayoff,
				ebea.population.neighbours.randomElements(NumberPartners, Neighbours),
				!Random,
				Traits^pcv, NextProbCombVector
			),
			ebea.population.update(
				Player^id,
				ebea.player.selection.pcv.updatePlayerProbCombVectors(NextProbCombVector),
				!NextRoundPopulation
			)
		else
			throw("ebea.player.selection.stepSelectPartnersPlayGame2/10: Invalid combination of chromosome and phenotipic trait")
		)
		;
		Chromosome = weightedPartnerSelection(PS),
		(if
			Traits = partnerSelection(_, yes(OldWeightVector))
		then
			/* create the strategy profile */
			ebea.player.selection.pcv.select(NumberPartners, Neighbours, Traits^pcv, !Random, SelectedSlot, PartnersIDs),
			list.map(ebea.population.players.player(!.NextRoundPopulation^players), PartnersIDs) = RestProfile,
			/* play the game */
			PredPlayGame(Player, RestProfile, MPayoffs, !NextRoundPopulation, !Random, !Extra),
			/* update the selection traits */
			(
				MPayoffs = yes(Payoffs),
				PlayerPayoff = array.lookup(Payoffs, 0),
				WeightFactor = PlayerPayoff - game.lowestPayoff(Game)
				;
				MPayoffs = no,
				PlayerPayoff = game.lowestPayoff(Game),
				WeightFactor = 0.0
			),
			ebea.player.selection.pcv.probabilityFloat(PS, Traits^pcv, SelectedSlot, ProbabilitySelectedCombination),
			list.foldl(
				ebea.player.selection.wv.updateWeight(
					ProbabilitySelectedCombination,
					WeightFactor),
				PartnersIDs,
				OldWeightVector,
				NextWeightVector),
			ebea.player.selection.pcv.updateProbCombVectors(
				Game, PlayerParameters^energyPar^energyScaling,
				PS, PartnersIDs, SelectedSlot, PlayerPayoff,
				ebea.player.selection.wv.drawAsList(NextWeightVector, NumberPartners),
				!Random,
				Traits^pcv, NextProbCombVector
			),
			ebea.population.update(
				Player^id,
				ebea.player.selection.wv.updatePlayerProbCombVectorsWeightVector(NextProbCombVector, NextWeightVector),
				!NextRoundPopulation
			)
		else
			throw("ebea.player.selection.stepSelectPartnersPlayGame/10: Invalid combination of chromosome and phenotipic trait")
		)
		;
		(
			Chromosome = opinion_old(_, _) ;
			Chromosome = opinion_old(_, _, _, _)
		),
		(if
			Traits = opinion(_, _)
		then
			(if
				/* create the strategy profile */
				ebea.player.selection.opinion.selectPartners(
					Traits,
					NumberPartners,
					!.NextRoundPopulation^players,
					Neighbours,
					RestProfile,
					!Random)
			then
				/* play the game */
				PredPlayGame(Player, RestProfile, MPayoffs, !NextRoundPopulation, !Random, !Extra),
				/* update the selection traits */
				(
					MPayoffs = yes(Payoffs),
					ebea.player.selection.opinion.updateOpinions(Game, PlayerParameters, [Player | RestProfile], Payoffs, !NextRoundPopulation)
					;
					MPayoffs = no
				)
			else
				/* update the selection traits */
				ebea.population.update(
					Player^id,
					ebea.player.selection.opinion.increaseUncertainty(PlayerParameters^selectionPar),
					!NextRoundPopulation)
			)
		else
			throw("ebea.player.selection.stepSelectPartnersPlayGame/10: Invalid combination of chromosome and traits")
		)
	).


:- func fold = ebea.player.selection.ac.

fold = ac(0, 0, 0, 0, 0.0, 0.0, 0, 0.0).


:- func fold(ebea.player.selection.chromosome.chromosome, ebea.player.selection.ac) = ebea.player.selection.ac.

fold(Chromosome, AC) = Result :-
	Chromosome = random,
	Result = AC
	;
	Chromosome = normalPartnerSelection(PS),
	Result^qty_NPS = AC^qty_NPS + 1,
	Result^qty_WPS = AC^qty_WPS,
	Result^sumPoolSize = AC^sumPoolSize + PS^poolSize,
	Result^sumBitsPerProbability = AC^sumBitsPerProbability + PS^bitsPerProbability,
	Result^sumProbabilityUpdateFactor = AC^sumProbabilityUpdateFactor + PS^probabilityUpdateFactor,
	Result^sumPayoffThreshold_PS = AC^sumPayoffThreshold_PS + PS^payoffThreshold,
	Result^qty_O = AC^qty_O,
	Result^sumPayoffThreshold_O = AC^sumPayoffThreshold_O
	;
	Chromosome = weightedPartnerSelection(PS),
	Result^qty_NPS = AC^qty_NPS,
	Result^qty_WPS = AC^qty_WPS + 1,
	Result^sumPoolSize = AC^sumPoolSize + PS^poolSize,
	Result^sumBitsPerProbability = AC^sumBitsPerProbability + PS^bitsPerProbability,
	Result^sumProbabilityUpdateFactor = AC^sumProbabilityUpdateFactor + PS^probabilityUpdateFactor,
	Result^sumPayoffThreshold_PS = AC^sumPayoffThreshold_PS + PS^payoffThreshold,
	Result^qty_O = AC^qty_O,
	Result^sumPayoffThreshold_O = AC^sumPayoffThreshold_O
	;
	Chromosome = opinion_old(_, _),
	Result =
	'sumPayoffThreshold_O :='(
	'qty_O :='(
		AC,
		AC^qty_O + 1
	),
		AC^sumPayoffThreshold_O + Chromosome^payoffThreshold_O
	)
	;
	Chromosome = opinion_old(_, _, _, _),
	Result = AC
	.









% :- pred initCombinationVector(int, list(player(C, T)), slot, slot, R, R)
% 	<= ePRNG(R).
% :- mode initCombinationVector(in, in, in, out, in, out) is det.

% initCombinationVector(CombinationSize, Neighbours, Slot, MappedSlot, !Random) :-
% 	ebea.player.randomIDs(CombinationSize, Combination, Neighbours, !Random),
% 	MappedSlot = 'combination :='(Slot, Combination).


/**
 * Print the traits of the selection gene group of an EBEA chromosome.
 */
:- pred printTraits(io.output_stream, ebea.player.selection.traits, io, io).
:- mode printTraits(in, in, di, uo) is det.


printTraits(Stream, Traits, !IO) :-
	Traits = random
	;
	Traits = partnerSelection(PCV, _),
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
		AC^qty_NPS + AC^qty_WPS = 0
	then
		io.print(Stream, "1/0 1/0 1/0 1/0", !IO)
	else
		FQty_PS = float(AC^qty_NPS+ AC^qty_WPS),
		io.format(Stream, "%f %f %f %f %d %d",
			[f(float(AC^sumPoolSize) / FQty_PS),
			 f(float(AC^sumBitsPerProbability) / FQty_PS),
			 f(AC^sumProbabilityUpdateFactor / FQty_PS),
			 f(AC^sumPayoffThreshold_PS / FQty_PS),
			 i(AC^qty_NPS),
			 i(AC^qty_WPS)
			 ], !IO)
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












% :- func default_poolSize = int.

% default_poolSize = 0.

% :- func default_bitsPerProbability = int.

% default_bitsPerProbability = 1.

% :- func default_probabilityUpdateFactor = float.

% default_probabilityUpdateFactor = 0.0.

% :- func default_payoffThreshold_PS = float.

% default_payoffThreshold_PS = 0.0.


% :- func default_payoffThreshold_O = float.

% default_payoffThreshold_O = 0.0.

% :- func default_initialUncertainty = float.

% default_initialUncertainty = 1.0.





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


:- func get_poolSizePercentageTransmission(ebea.player.selection.parameters) = int.

get_poolSizePercentageTransmission(P) = P^poolSizePercentageTransmission.

:- func set_poolSizePercentageTransmission(ebea.player.selection.parameters, int) = ebea.player.selection.parameters.

set_poolSizePercentageTransmission(P, V) = 'poolSizePercentageTransmission :='(P, V).



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
