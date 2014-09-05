/**
 * Provides predicates and functions to handle partner selection based on opinion.


 * TODO: player selects a partner, if partner opinion is outside player
 * uncertainty, the player does not play the game.

 * TODO: opinion chromosome specifies initial opinion and uncertainty

 * TODO: introduce parameter to have positive or negative opinion influence.

 * BUG: checkPartnerOpinion compared the overlap with partner uncertainty
 * instead of player uncertainty
  
 * @author Pedro Mariano
 * @version 1.0 2014/01/ 2
 */
:- module ebea.player.selection.opinion.

:- interface.

:- inst opinion == bound(opinion(ground, ground)).

/**
 * Succeeds if the partner profile is composed of opinion players and the
 * overlap between player and partner is greater than the partner
 * uncertainty.
 */

:- pred checkPartnerOpinion(ebea.player.selection.traits, list(player(C, T))).
:- mode checkPartnerOpinion(in(opinion), in) is semidet.


/**
 * selectPartners(PlayerSelectionTraits, NumberPartners, Players, Neighbours, SelectedPartners, !Random)
  
 * Selects partners based on their opinion.  The probability of a neighbour
 * being selected is proportional to how similar is its opinion to the
 * selecting player.  For each neighbour we computed a weight based on
 * opinion similarity.  For neighbours that do not use the opinion based,
 * we use a base weight.
  
 */

:- pred selectPartners(
	ebea.player.selection.traits          :: in(opinion),
	int                                   :: in,
	ebea.population.players.players(C, T) :: in,
	ebea.population.neighbours.neighbours :: in,
	list(player(C, T))                    :: out,
	R :: in,  R :: out
)
	is semidet
	<= ePRNG(R)
	.

% /**
%  * Select a number of partners based on the opinion of the given player and
%  * partners.  The probability of a partner being selected is inversely
%  * proportional to the absolute opinion difference.
%  */

% :- pred selectPartners_v1(ebea.player.selection.traits, int, list(player(C, T)), list(player(C, T)), R, R) <= ePRNG(R).
% :- mode selectPartners_v1(in(opinion), in, in, out, in, out) is det.

% /**
%  * Select a number of partners based on the opinion of the given player and
%  * partners.  The probability of a partner being selected is inversely
%  * proportional to the absolute opinion difference.
%  */

% :- pred selectPartners_v2(ebea.player.selection.traits, int, list(player(C, T)), list(player(C, T)), R, R) <= ePRNG(R).
% :- mode selectPartners_v2(in(opinion), in, in, out, in, out) is semidet.


/**
 * After the players have played the game, they update the opinions they have.
 */
:- pred updateOpinions(G, ebea.player.parameters(P), list(player(C, T)), array(float), population(C, T), population(C, T))
	<= abstractGame(G).
:- mode updateOpinions(in, in, in, in, in, out) is det.

:- func increaseUncertainty(ebea.player.selection.parameters, player(C, T)) = player(C, T).

:- pred debug(R, R, io.state, io.state) <= ePRNG(R).
:- mode debug(in, out, di, uo) is det.

:- implementation.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

:- type weightFunc == (func(ebea.player.selection.traits, ebea.player.selection.traits) = float).

:- inst weightFunc == ((func(in(opinion), in) = out) is det).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

checkPartnerOpinion(TraitPlayer, Profile) :-
	Profile = []
	;
	Profile = [Partner | RestProfile],
	Partner^traits^selectionTrait = TraitPartner,
	TraitPartner = opinion(_, _),
	overlap(TraitPlayer, TraitPartner) > TraitPlayer^uncertainty,
%	overlap(TraitPlayer, TraitPartner) > TraitPartner^uncertainty,
	checkPartnerOpinion(TraitPlayer, RestProfile)
	.

selectPartners(TraitPlayer, NumberPartners, Players, Neighbours, SelectedPartners, !Random) :-
	ebea.population.neighbours.weightedRandomElements(
		weight(Players, TraitPlayer),
		NumberPartners,
		Neighbours,
		SelectedPartnerIDs,
		!Random),
	SelectedPartners = list.map(ebea.population.players.player(Players), SelectedPartnerIDs)
	.

% selectPartners_v1(TraitPlayer, NumberPartners, Neighbours, Partners, !Random) :-
% 	list.foldl3(selectAPartner_v1(TraitPlayer), 1..NumberPartners, [], Partners, Neighbours, _, !Random).

% selectPartners_v2(TraitPlayer, NumberPartners, Neighbours, Partners, !Random) :-
% 	list.foldl3(selectAPartner_high(weight_v2, TraitPlayer), 1..NumberPartners, [], Partners, Neighbours, _, !Random).

updateOpinions(Game, Parameters, Players, Payoffs, !NextRoundPopulation) :-
	Players = [FocalPlayer | SelectedPartners],
	list.foldl2(updateOpinion(Game, Parameters, Payoffs, FocalPlayer), SelectedPartners, 1, _, !NextRoundPopulation)
	;
	Players = [],
	throw("updateOpinions/6: never reached otherwise there is an invalid game")
	.

increaseUncertainty(Parameters, Player) = Result :-
	Opinion = Player^traits^selectionTrait,
	(if
		Opinion = opinion(_, _)
	then
		NewUncertainty = float.min(2.0, Opinion^uncertainty * Parameters^uncertaintyIncreaseFactor),
		NextOpinion = 'uncertainty :='(Opinion, NewUncertainty),
		PlayerTraits = 'selectionTrait :='(Player^traits, NextOpinion),
		Result = 'traits :='(Player, PlayerTraits)
	else
		throw("Never reached")
	).

debug(!Random, !IO) :-
	io.open_output("opinion.txt", IOpinionStream, !IO),
	io.open_output("uncertainty.txt", IUncertaintyStream, !IO),
	(if
		IOpinionStream = ok(OpinionStream),
		IUncertaintyStream = ok(UncertaintyStream)
	then
		PredInit =
		(pred(_::in, Trait::out, RndDI::in, RndUO::out) is det :-
			rng.nextFloat(OV, RndDI, R),
			Trait^opinionValue = 2.0 * OV - 1.0,
			rng.nextFloat(U, R, RndUO),
			Trait^uncertainty = U
		),
		PredPrint =
		(pred(ST::in, IOdi::di, IOuo::uo) is det :-
			(if
				ST = opinion(_, _)
			then
				io.format(OpinionStream, "\t%f", [f(ST^opinionValue)], IOdi, IO),
				io.format(UncertaintyStream, "\t%f", [f(ST^uncertainty)], IO, IOuo)
			else
				IOuo = IOdi
			)
		),
		list.map_foldl(PredInit, 1..10, Population, !Random),
		PredStep =
		(pred(_::in, PopIn::in, PopOu::out, IOdi::di, IOuo::uo) is det :-
			list.foldl(PredPrint, PopIn, IOdi, IO1),
			io.nl(OpinionStream, IO1, IO2),
			io.nl(UncertaintyStream, IO2, IOuo),
			debugStep(PopIn, PopOu)
		),
		list.foldl2(PredStep, 1..100, Population, _, !IO),
		io.close_output(OpinionStream, !IO),
		io.close_output(UncertaintyStream, !IO)
	else
		true
	)
	.

:- pred debugStep(list(ebea.player.selection.traits), list(ebea.player.selection.traits)).
:- mode debugStep(in, out) is det.

debugStep([], []).
debugStep([S | Rest], [SM | RestM]) :-
	debugStep(Rest, RestM),
	(if
		S = opinion(_, _)
	then
		Parameters = 'mu :='(ebea.player.selection.defaultParameters, 0.1),
		updateTrait(yes, Parameters, opinion(0.0, 0.8), S) = SM
	else
		throw("Exception")
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

/**
 * weight(Players, TraitPlayer, NeighbourKey) = Result
  
 * Return the weight of a neighbour.
  
 * This function is passed to the function {@code
 * ebea.population.neighbours.weightedRandomElements/6} that computes a
 * weighted random sample of neighbours
 */

:- func weight(ebea.population.players.players(C, T), ebea.player.selection.traits, ebea.population.players.key) = int.
:- mode weight(in, in(opinion), in) = out is det.

weight(Players, TraitPlayer, NeighbourKey) = Result :-
	 ebea.population.players.player(Players, NeighbourKey) = ANeighbour,
	 Result = weight(TraitPlayer, ANeighbour^traits^selectionTrait).

/**
 * Return the weight of a partner that is used in computing its probability
 * of being selected.
 */

:- func weight(ebea.player.selection.traits, ebea.player.selection.traits) = int.
:- mode weight(in(opinion), in) = out is det.

weight(TraitPlayer, TraitPartner) = Result :-
	TraitPartner = random,
	Result = 0
	;
	TraitPartner = partnerSelection(_, _),
	Result = 0
	;
	TraitPartner = opinion(_, _),
	(if
		overlap(TraitPlayer, TraitPartner) > TraitPlayer^uncertainty
	then
		DiffOpinion = float.abs(TraitPartner^opinionValue - TraitPlayer^opinionValue),
		Result = float.round_to_int(1000.0 * (2.0 - DiffOpinion))
	else
		Result = 0
	)
	.



% /**
%  * This function is used to reduce the neighbours to a sum of neighbours'
%  * weight.  The weight is given by function {@code weight/2}.

%  */
% :- func computeTotalWeight(ebea.player.selection.traits, ebea.population.players.players(C, T), ebea.population.players.key, float) = float.
% :- mode computeTotalWeight(in(opinion), in, in, in) = out is det.

% computeTotalWeight(TraitPlayer, Players, NeighbourID, AC) =
% 	AC +
% 	weight(TraitPlayer, ebea.population.players.player(Players, NeighbourID)^traits^selectionTrait)
% 	.




% :- func computeTotalWeight_v1(ebea.player.selection.traits, list(player(C, T))) = float.
% :- mode computeTotalWeight_v1(in(opinion), in) = out is det.

% computeTotalWeight_v1(TraitPlayer, Partners) = list.foldl(computeTotalWeight_v1(TraitPlayer), Partners, 0.0).

% :- func computeTotalWeight_v1(ebea.player.selection.traits, player(C, T), float) = float.
% :- mode computeTotalWeight_v1(in(opinion), in, in) = out is det.

% computeTotalWeight_v1(TraitPlayer, Partner, AC) = AC + weight_v1(TraitPlayer, Partner^traits^selectionTrait).

% /**
%  * Return the weight of a partner that is used in computing its probability of being selected.
%  */
% :- func weight_v1(ebea.player.selection.traits, ebea.player.selection.traits) = float.
% :- mode weight_v1(in(opinion), in) = out is det.

% weight_v1(TraitPlayer, TraitPartner) = Result :-
% 	TraitPartner = random,
% 	Result = 1.0
% 	;
% 	TraitPartner = partnerSelection(_),
% 	Result = 1.0
% 	;
% 	TraitPartner = opinion(_, _),
% 	Result = 10.0 * (2.0 - float.abs(TraitPartner^opinionValue - TraitPlayer^opinionValue) + 1.0)
% 	.

% /**
%  * pickPartner(TraitPlayer, Weight, PickedPartner, Partners, RemainingPartners)
  
%  * Pick a partner that corresponds to the randomly computed weight.
%  * Parameter {@code Partners} contains the available partners.  We select
%  * partner without substitution, therefore parameter {@code
%  * RemainingPartners} is unified with the partners that were not selected.

%  * <p><b>Pre-condition</b>: there must be at least one partner in {@code Partners}.
  
%  */
% :- pred pickPartner_v1(ebea.player.selection.traits, float, player(C, T), list(player(C, T)), list(player(C, T))).
% :- mode pickPartner_v1(in(opinion), in, out, in, out) is det.

% pickPartner_v1(TraitPlayer, TotalWeight, PickedPartner, Partners, RemainingPartners) :-
% 	(if
% 		Partners = [Partner | RestPartners]
% 	then
% 		Weight = weight_v1(TraitPlayer, Partner^traits^selectionTrait),
% 		(if
% 			TotalWeight < Weight
% 		then
% 			PickedPartner = Partner,
% 			RemainingPartners = RestPartners
% 		else
% 			RemainingPartners = [Partner | RestRemainingPartners],
% 			pickPartner_v1(
% 				TraitPlayer,
% 				TotalWeight - Weight,
% 				PickedPartner,
% 				RestPartners,
% 				RestRemainingPartners)
% 		)
% 	else
% 		throw("Error in computing a partner")
% 	).

% /**
%  * selectAPartner_v1(TraitPlayer, Index, !AvailablePartners, !Random)

%  * Select the {@code Index}th partner for the given player without
%  * replacement.  Partners are selected with a probability that is inversely
%  * proportional to the opinion difference between the focal player and
%  * potential partner.
  
%  */
% :- pred selectAPartner_v1(ebea.player.selection.traits, int, list(player(C, T)), list(player(C, T)),
% 	list(player(C, T)), list(player(C, T)),
% 	R, R)
% 	<= ePRNG(R).
% :- mode selectAPartner_v1(in(opinion), in, in, out, in, out, in, out) is det.

% selectAPartner_v1(TraitPlayer, _, !PickedPartners, !AvailablePartners, !Random) :-
% 	rng.nextFloat(Rnd, !Random),
% 	Weight = Rnd * computeTotalWeight_v1(TraitPlayer, !.AvailablePartners),
% 	pickPartner_v1(TraitPlayer, Weight, PickedPartner, !AvailablePartners),
% 	list.cons(PickedPartner, !PickedPartners).


% /**
%  * selectAPartner_high(WeightFunc, TraitPlayer, Index, !AvailablePartners, !Random)

%  * Select the {@code Index}th partner for the given player without
%  * replacement.  Partners are selected with a probability that is inversely
%  * proportional to the opinion difference between the focal player and
%  * potential partner.
  
%  */
% :- pred selectAPartner_high(ebea.player.selection.opinion.weightFunc, ebea.player.selection.traits, int, list(player(C, T)), list(player(C, T)),
% 	list(player(C, T)), list(player(C, T)),
% 	R, R)
% 	<= ePRNG(R).
% :- mode selectAPartner_high(in(weightFunc), in(opinion), in, in, out, in, out, in, out) is semidet.

% selectAPartner_high(WeightFunc, TraitPlayer, _, !PickedPartners, !AvailablePartners, !Random) :-
% 	rng.nextFloat(Rnd, !Random),
% 	TotalWeight = list.foldl(computeTotalWeight_high(WeightFunc, TraitPlayer), !.AvailablePartners, 0.0),
% 	TotalWeight > 0.0,
% 	Weight = Rnd * TotalWeight,
% 	pickPartner_high(WeightFunc, TraitPlayer, Weight, PickedPartner, !AvailablePartners),
% 	list.cons(PickedPartner, !PickedPartners).




% /**
%  * pickPartner_high(WeightFunc, TraitPlayer, Weight, PickedPartner, Partners, RemainingPartners)
  
%  * Pick a partner that corresponds to the randomly computed weight.
%  * Parameter {@code Partners} contains the available partners.  We select
%  * partner without substitution, therefore parameter {@code
%  * RemainingPartners} is unified with the partners that were not selected.

%  * <p><b>Pre-condition</b>: there must be at least one partner in {@code Partners}.
  
%  */
% :- pred pickPartner_high(ebea.player.selection.opinion.weightFunc, ebea.player.selection.traits, float, player(C, T), list(player(C, T)), list(player(C, T))).
% :- mode pickPartner_high(in(weightFunc), in(opinion), in, out, in, out) is det.
% %:- mode pickPartner_high(in((func(in(opinion), in) = out) is det), in(opinion), in, out, in, out) is det.

% pickPartner_high(WeightFunc, TraitPlayer, TotalWeight, PickedPartner, Partners, RemainingPartners) :-
% 	(if
% 		Partners = [Partner | RestPartners]
% 	then
% 		Weight = WeightFunc(TraitPlayer, Partner^traits^selectionTrait),
% 		(if
% 			TotalWeight < Weight
% 		then
% 			PickedPartner = Partner,
% 			RemainingPartners = RestPartners
% 		else
% 			RemainingPartners = [Partner | RestRemainingPartners],
% 			pickPartner_high(
% 				WeightFunc,
% 				TraitPlayer,
% 				TotalWeight - Weight,
% 				PickedPartner,
% 				RestPartners,
% 				RestRemainingPartners)
% 		)
% 	else
% 		throw("Error in computing a partner")
% 	).

% :- func computeTotalWeight_high(ebea.player.selection.opinion.weightFunc, ebea.player.selection.traits, player(C, T), float) = float.
% :- mode computeTotalWeight_high(in(weightFunc), in(opinion), in, in) = out is det.

% computeTotalWeight_high(WeightFunc, TraitPlayer, Partner, AC) = AC + WeightFunc(TraitPlayer, Partner^traits^selectionTrait).

% /**
%  * Return the weight of a partner that is used in computing its probability
%  * of being selected.
%  */

% :- func weight_v2(ebea.player.selection.traits, ebea.player.selection.traits) = float.
% :- mode weight_v2(in(opinion), in) = out is det.

% weight_v2(TraitPlayer, TraitPartner) = Result :-
% 	TraitPartner = random,
% 	Result = 0.0
% 	;
% 	TraitPartner = partnerSelection(_),
% 	Result = 0.0
% 	;
% 	TraitPartner = opinion(_, _),
% 	DiffOpinion = float.abs(TraitPartner^opinionValue - TraitPlayer^opinionValue),
% 	(if
% 		overlap(TraitPlayer, TraitPartner) > TraitPlayer^uncertainty
% 	then
% 		Result = 10.0 * (2.0 - DiffOpinion + 1.0)
% 	else
% 		Result = 0.0
% 	)
% 	.


%

/**
 * updateOpinion(Game, EnergyScaling, Payoffs, FocalPlayer, SelectedPartner, !Index, !Population)

 * Update the opinion of the given player which obtained the {@code !Index}
 * payoff.  The focal player may have selected partners that do not have an
 * opinion.  In this case the population remains unchanged.  Otherwise we
 * update the opinion.

 * <p> This predicate is used as a closure to reduce the list of selected
 * partners.

 * @param FocalPlayer the player that selected partners

 * @param SelectedPartner one of the partners selected by the player

 * @param !Index selected partner index in array {@code Payoffs}.

  
 */
:- pred updateOpinion(G, ebea.player.parameters(P), array(float), player(C, T), player(C, T), int, int, population(C, T), population(C, T))
	<= abstractGame(G).
:- mode updateOpinion(in, in, in, in, in, in, out, in, out) is det.

updateOpinion(Game, Parameters, Payoffs, FocalPlayer, SelectedPartner, Index, Index + 1, !Population) :-
	SelectionTrait_FP = FocalPlayer^traits^selectionTrait,
	SelectionTrait_SP = SelectedPartner^traits^selectionTrait,
	(if
		SelectionTrait_FP = opinion(_, _),
%		FocalPlayer^chromosome^selectionGenes = opinion(PayoffThreshold_FP, _),
		SelectionTrait_SP = opinion(_, _),
%		SelectedPartner^chromosome^selectionGenes = opinion(PayoffThreshold_SP, _),
		SelectionTrait_FP \= SelectionTrait_SP
	then
		PayoffThreshold = paretoPayoff(Game),
		(if
			scaledPayoffToThreshold(Game, Parameters^energyPar^energyScaling, array.lookup(Payoffs, 0)) >= PayoffThreshold
		then
			NextOpinion_FP = updateTrait(yes, Parameters^selectionPar, SelectionTrait_SP, SelectionTrait_FP),
			ebea.population.update(FocalPlayer^id, updatePlayerOpinion(NextOpinion_FP), !Population)
		else
			NextOpinion_FP = updateTrait(no, Parameters^selectionPar, SelectionTrait_SP, SelectionTrait_FP),
			ebea.population.update(FocalPlayer^id, updatePlayerOpinion(NextOpinion_FP), !Population)
		),
		(if
			scaledPayoffToThreshold(Game, Parameters^energyPar^energyScaling, array.lookup(Payoffs, Index)) >= PayoffThreshold
		then
			NextOpinion_SP = updateTrait(yes, Parameters^selectionPar, SelectionTrait_FP, SelectionTrait_SP),
			ebea.population.update(SelectedPartner^id, updatePlayerOpinion(NextOpinion_SP), !Population)
		else
			NextOpinion_SP = updateTrait(no, Parameters^selectionPar, SelectionTrait_FP, SelectionTrait_SP),
			ebea.population.update(SelectedPartner^id, updatePlayerOpinion(NextOpinion_SP), !Population)
		)
	else
		true
	).
		
	% TraitSelectedPartner = SelectedPartner^traits^selectionTrait,
	% (
	% 	TraitSelectedPartner = random
	% 	;
	% 	TraitSelectedPartner = partnerSelection(_)
	% 	;
	% 	TraitSelectedPartner = opinion(_, _),
	% 	ChromosomeSelectedPartner = SelectedPartner^chromosome^selectionGenes,
	% 	(
	% 		ChromosomeSelectedPartner = random,
	% 		throw("Invalid combination of selection chromosome and trait")
	% 		;
	% 		ChromosomeSelectedPartner = partnerSelection(_, _, _, _),
	% 		throw("Invalid combination of selection chromosome and trait")
	% 		;
	% 		ChromosomeSelectedPartner = opinion(PayoffThreshold),
	% 		PlayerPayoff = array.lookup(Payoffs, 0),
	% 		(if
	% 			scaledPayoffToThreshold(Game, EnergyScaling, PlayerPayoff) > PayoffThreshold
	% 		then
	% 			true
	% 		else
	% 			true
	% 		)
	% 	)
	% ).

:- func updatePlayerOpinion(ebea.player.selection.traits, player(C, T)) = player(C, T).
:- mode updatePlayerOpinion(in(opinion), in) = out is det.

updatePlayerOpinion(Opinion, Player) = Result :-
	PlayerTraits = 'selectionTrait :='(Player^traits, Opinion),
	Result = 'traits :='(Player, PlayerTraits).

%:- pred opinionChromosomeTrait(player(C, T), ebea.player.selection.chromosome, ebea.player.selection.traits).
%:- mode opinionChromosomeTrait(in, out, out(opinion)) is semidet.

		
/**
 * Compute the overlap between the opinions of two players.  The overlap is
 * represented by <i>h<sub>ij</sub></i>.
  
 */
:- func overlap(ebea.player.selection.traits, ebea.player.selection.traits) = float.
:- mode overlap(in(opinion), in(opinion)) = out is det.

overlap(Trait1, Trait2) =
	float.min(
		Trait1^opinionValue + Trait1^uncertainty,
		Trait2^opinionValue + Trait2^uncertainty)
	-
	float.max(
		Trait1^opinionValue - Trait1^uncertainty,
		Trait2^opinionValue - Trait2^uncertainty)
	.

/**
 * Compute the non-overlapping of player one.  The non-overlap is the
 * uncertainty length minus the overlap and is represented by
 * <i>2u<sub>i</sub>-h<sub>ij</sub></i>.
  
 */

:- func nonOverlapping(ebea.player.selection.traits, ebea.player.selection.traits) = float.
:- mode nonOverlapping(in(opinion), in(opinion)) = out is det.

nonOverlapping(Trait1, Trait2) =
	2.0 * Trait1^uncertainty - overlap(Trait1, Trait2).

:- func relativeAgreement(ebea.player.selection.traits, ebea.player.selection.traits) = float.
:- mode relativeAgreement(in(opinion), in(opinion)) = out is det.

relativeAgreement(Trait1, Trait2) =
	overlap(Trait1, Trait2) / Trait1^uncertainty - 1.0.

/**
 * Updates the opinion of player two after interacting with player one.
 */

:- func updateTrait(bool, ebea.player.selection.parameters, ebea.player.selection.traits, ebea.player.selection.traits) = ebea.player.selection.traits.
:- mode updateTrait(in, in, in(opinion), in(opinion)) = out(opinion) is det.

updateTrait(yes, Parameters, Trait1, Trait2) = Result :-
	(if
		overlap(Trait1, Trait2) > Trait1^uncertainty
	then
		Result^opinionValue =
			Trait2^opinionValue
			+ Parameters^mu
				* relativeAgreement(Trait1, Trait2)
				* (Trait1^opinionValue - Trait2^opinionValue),
		Result^uncertainty =
			Trait2^uncertainty
			+ Parameters^mu
				* relativeAgreement(Trait1, Trait2)
				* (Trait1^uncertainty - Trait2^uncertainty)
	else
		Result = Trait2
	)
	.
	
updateTrait(no, Parameters, Trait1, Trait2) = Result :-
	(if
		overlap(Trait1, Trait2) > Trait1^uncertainty
	then
		Result^opinionValue =
			float.max(-1.0,
				float.min(1.0,
					Trait2^opinionValue
					- Parameters^mu
						* relativeAgreement(Trait1, Trait2)
						* (Trait1^opinionValue - Trait2^opinionValue)
				)
			),
		Result^uncertainty =
			Trait2^uncertainty
	else
		Result = Trait2
	)
	.
	



% :- pred updateTrait(ebea.player.selection.parameters, ebea.player.selection.traits, ebea.player.selection.traits, ebea.player.selection.traits).
% :- mode updateTrait(in, in(opinion), in(opinion), out(opinion)) is det.

% updateTrait(Parameters, Trait1, Trait2, Result) :-
% 	overlap(Trait1, Trait2) > Trait1^uncertainty,
% 	(
% 		Trait1^opinionValue \= Trait2^opinionValue
% 		;
% 		Trait1^uncertainty \= Trait2^uncertainty
% 	),
% 	Result^opinionValue = Trait2^opinionValue +
% 		(if
% 			Trait1^opinionValue \= Trait2^opinionValue
% 		then
% 			Parameters^mu
% 			* relativeAgreement(Trait1, Trait2)
% 			* (Trait1^opinionValue - Trait2^opinionValue)
% 		else
% 			0.0
% 		),
% 	Result^uncertainty = Trait2^uncertainty +
% 		(if
% 			Trait1^uncertainty \= Trait2^uncertainty
% 		then
% 			Parameters^mu
% 			* relativeAgreement(Trait1, Trait2)
% 			* (Trait1^uncertainty - Trait2^uncertainty)
% 		else
% 			0.0
% 		)
% 	.

:- end_module ebea.player.selection.opinion.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
