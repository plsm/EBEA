/**
 * Provides the definition of the genes responsible for partner selection.
 * This module also contains predicates to define an user interface to edit
 * these genes.

 * @author Pedro Mariano
 * @version 1.0 2014/03/05
 */
:- module ebea.player.selection.chromosome.

:- interface.

:- import_module userInterface.
:- import_module parseable.

/**
 * The chromosome part that is responsible for partner selection.
 * Selection can be random, based on the partner selection algorithm
 * presented in Mariano 2009, or based on generic opinion.

 * TODO: opinion chromosome has average and deviation of initial uncertainty.

 * TODO2 : remove payoff threshold from opinion. Increases search space
 * unnecessarily.
  
 */
:- type chromosome --->
	random ;
	normalPartnerSelection(
		ebea.player.selection.pcv.chromosome
	) ;
	weightedPartnerSelection(
		ebea.player.selection.pcv.chromosome
	) ;
	opinion(
		uncertaintyIncreaseFactor :: float,
		mu                        :: float
	) ;
	opinion_old(
		payoffThreshold_O  :: float ,
		initialUncertainty :: float
	) ;
	opinion_old(
		initialAverageOpinion     :: float ,
		initialStdDevOpinion      :: float ,
		initialAverageUncertainty :: float ,
		initialStdDevUncertainty  :: float
	) .

:- inst random == bound(random).

:- inst normalPartnerSelection == bound(normalPartnerSelection(ground)).

:- inst weightedPartnerSelection == bound(weightedPartnerSelection(ground)).

:- inst opinion_old == bound(
	opinion_old(ground, ground) ;
	opinion_old(ground, ground, ground, ground)
	).

:- instance parseable(ebea.player.selection.chromosome.chromosome).

:- instance printable(ebea.player.selection.chromosome.chromosome).



/**
 * Return the number of genes in the given selection chromosome.
 */
:- func numberGenes(ebea.player.selection.chromosome.chromosome) = int.

/**
 * mutateGene(Parameters, Index, !Distribution, !Random, Chromosome, Result)

 * Mutate the {@code Index}th gene of {@code Chromosome} and the return the
 * result in {@code Result}.  Accumulator parameters {@code Distribution}
 * and {@code Random} are used to calculate the new gene value.
 */

:- pred mutateGene(
	ebea.player.selection.parameters :: in,
	int                              :: in,
	distribution :: in,  distribution :: out,
	R            :: in,  R            :: out,
	ebea.player.selection.chromosome.chromosome :: in,
		ebea.player.selection.chromosome.chromosome :: out
) is det
<= ePRNG(R).

/**
 * Given the selection part of an EBEA chromosome return the individual
 * that can develop from this chromosome.
 */
:- func born(ebea.player.selection.parameters, ebea.player.selection.chromosome.chromosome) = ebea.player.selection.traits.
:- mode born(in, in) = out is erroneous.



/**
 * Return a default value of {@code chromosome}.
 */
:- func default = ebea.player.selection.chromosome.chromosome.

:- func dialog = list(dialogItem(ebea.player.selection.chromosome.chromosome)).

:- pred parse(ebea.player.selection.chromosome.chromosome, list(int), list(int)).
:- mode parse(in, out, in) is det.
:- mode parse(out, in, out) is semidet.

/**
 * Print the selection part of an EBEA chromosome.
 */
:- pred print(io.output_stream, ebea.player.selection.chromosome.chromosome, io, io).
:- mode print(in, in, di, uo) is det.


:- implementation.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

:- instance parseable(ebea.player.selection.chromosome.chromosome) where
[
	pred(parse/3) is ebea.player.selection.chromosome.parse
].

:- instance printable(ebea.player.selection.chromosome.chromosome)
	where
[
	pred(print/4) is ebea.player.selection.chromosome.print
].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

numberGenes(Chromosome) = Result :-
	Chromosome = random,
	Result = 0
	;
	Chromosome = normalPartnerSelection(_),
	Result = 4
	;
	Chromosome = weightedPartnerSelection(_),
	Result = 4
	;
	Chromosome = opinion_old(_, _),
	Result = 1
	;
	Chromosome = opinion_old(_, _, _, _),
	Result = 0
	.

mutateGene(Parameters, Index, !Distribution, !Random, Chromosome, Result) :-
	Chromosome = random,
	throw("ebea.player.selection.mutateGene/8: No genes to mutate in chromosome random")
	;
	Chromosome = normalPartnerSelection(PS),
	mutateGenePartnerSelection(Parameters, Index, !Distribution, !Random, PS, NewPS),
	Result = normalPartnerSelection(NewPS)
	;
	Chromosome = weightedPartnerSelection(PS),
	mutateGenePartnerSelection(Parameters, Index, !Distribution, !Random, PS, NewPS),
	Result = weightedPartnerSelection(NewPS)
	;
	Chromosome = opinion_old(_, _),
	(if
		Index = 0
	then
		rng.distribution.unitGaussian(Perturb, !Distribution, !Random),
		Next = float.max(0.0, float.min(Chromosome^initialUncertainty + Perturb * 0.5, 2.0)),
		Result = 'initialUncertainty :='(Chromosome, Next)
	else
		throw("ebea.player.selection.mutateGene/8: invalid gene index for opinion chromosome")
	)
	;
	Chromosome = opinion_old(_, _, _, _),
	throw("ebea.player.selection.mutateGene/8: No genes to mutate in chromosome opinion/4")
	.

born(_, _) = throw("Not used").

default = random.

dialog =
	[di(label("selection genes"), selectOneOf(
		getCurrentChoice,
		setChoice,
		[
		 ci(label("random"),             []),
		 ci(label("normal partner selection"),
			[
			 di(label("pool size"),                  updateFieldInt(   get_normalPartnerSelectionField(pcv_get_poolSize),                 checkInt(   "pool size",                  bounded(0, yes),    unbound,            set_normalPartnerSelectionField(pcv_set_poolSize)))),
			 di(label("bits per probability"),       updateFieldInt(   get_normalPartnerSelectionField(pcv_get_bitsPerProbability),       checkInt(   "bits per probability",       bounded(1, yes),    unbound,            set_normalPartnerSelectionField(pcv_set_bitsPerProbability)))),
			 di(label("probability update factor"),  updateFieldFloat( get_normalPartnerSelectionField(pcv_get_probabilityUpdateFactor),  checkFloat( "probability update factor",  bounded(0.0, yes),  bounded(1.0, yes),  set_normalPartnerSelectionField(pcv_set_probabilityUpdateFactor)))),
			 di(label("payoff threshold"),           updateFieldFloat( get_normalPartnerSelectionField(pcv_get_payoffThreshold),          checkFloat( "payoff threshold",           bounded(-1.0, yes), bounded(1.0, yes),  set_normalPartnerSelectionField(pcv_set_payoffThreshold))))
			]),
		 ci(label("weighted partner selection"),
			[
			 di(label("pool size"),                  updateFieldInt(   get_weightedPartnerSelectionField(pcv_get_poolSize),                 checkInt(   "pool size",                  bounded(0, yes),    unbound,            set_weightedPartnerSelectionField(pcv_set_poolSize)))),
			 di(label("bits per probability"),       updateFieldInt(   get_weightedPartnerSelectionField(pcv_get_bitsPerProbability),       checkInt(   "bits per probability",       bounded(1, yes),    unbound,            set_weightedPartnerSelectionField(pcv_set_bitsPerProbability)))),
			 di(label("probability update factor"),  updateFieldFloat( get_weightedPartnerSelectionField(pcv_get_probabilityUpdateFactor),  checkFloat( "probability update factor",  bounded(0.0, yes),  bounded(1.0, yes),  set_weightedPartnerSelectionField(pcv_set_probabilityUpdateFactor)))),
			 di(label("payoff threshold"),           updateFieldFloat( get_weightedPartnerSelectionField(pcv_get_payoffThreshold),          checkFloat( "payoff threshold",           bounded(-1.0, yes), bounded(1.0, yes),  set_weightedPartnerSelectionField(pcv_set_payoffThreshold))))
			]),
		 ci(label("opinion"),
			[
			 di(label("payoff threshold (as fraction of game payoff)"),  updateFieldFloat( get_payoffThreshold_O,       checkFloat( "payoff threshold",     bounded(0.0, yes),  bounded(1.0, yes), set_payoffThreshold_O))),
			 di(label("initial uncertainty"),                            updateFieldFloat( get_initialUncertainty,      checkFloat( "initial uncertainty",  bounded(-1.0, yes), bounded(1.0, yes), set_initialUncertainty)))
			]),
		 ci(label("opinion"),
			 [
			  di(label("initialAverageOpinion"),      updateFieldFloat(    get_initialAverageOpinion,      checkFloat( "initialAverageOpinion",      unbound, unbound, set_initialAverageOpinion))),
			  di(label("initialStdDevOpinion"),       updateFieldFloat(    get_initialStdDevOpinion,       checkFloat( "initialStdDevOpinion",       unbound, unbound, set_initialStdDevOpinion))),
			  di(label("initialAverageUncertainty"),  updateFieldFloat(    get_initialAverageUncertainty,  checkFloat( "initialAverageUncertainty",  unbound, unbound, set_initialAverageUncertainty))),
			  di(label("initialStdDevUncertainty"),   updateFieldFloat(    get_initialStdDevUncertainty,   checkFloat( "initialStdDevUncertainty",   unbound, unbound, set_initialStdDevUncertainty)))
			 ])
		]))].

parse(P) -->
	{P = random},
	[0]
	.

parse(P) -->
	{P = normalPartnerSelection(PS)},
	[1],
	parsePartnerSelection(PS)
	.

parse(P) -->
	{P = opinion_old(_, _)},
	[2],
	parseable.float32(P^payoffThreshold_O),
	parseable.float32(P^initialUncertainty)
	.

parse(P) -->
	{P = opinion_old(_, _, _, _)},
	[3],
	parseable.float32(P^initialAverageOpinion),
	parseable.float32(P^initialStdDevOpinion),
	parseable.float32(P^initialAverageUncertainty),
	parseable.float32(P^initialStdDevUncertainty)
	.

parse(P) -->
	{P = weightedPartnerSelection(PS)},
	[4],
	parsePartnerSelection(PS)
	.


print(Stream, Chromosome, !IO) :-
	Chromosome = random,
	io.print(Stream, "random", !IO)
	;
	Chromosome = normalPartnerSelection(PS),
	printPartnerSelection(Stream, PS, !IO)
	;
	Chromosome = weightedPartnerSelection(PS),
	printPartnerSelection(Stream, PS, !IO)
	;
	Chromosome = opinion_old(_, _),
	io.print(Stream, Chromosome^payoffThreshold_O, !IO),
	io.print(Stream, ' ', !IO),
	io.print(Stream, Chromosome^initialUncertainty, !IO)
	;
	Chromosome = opinion_old(_, _, _, _),
	io.print(Stream, Chromosome^initialAverageOpinion, !IO),
	io.print(Stream, ' ', !IO),
	io.print(Stream, Chromosome^initialStdDevOpinion, !IO),
	io.print(Stream, ' ', !IO),
	io.print(Stream, Chromosome^initialAverageUncertainty, !IO),
	io.print(Stream, ' ', !IO),
	io.print(Stream, Chromosome^initialStdDevUncertainty, !IO)
	.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions


:- pred mutateGenePartnerSelection(
	ebea.player.selection.parameters :: in,
	int                              :: in,
	distribution                         :: in,  distribution                         :: out,
	R                                    :: in,  R                                    :: out,
	ebea.player.selection.pcv.chromosome :: in,  ebea.player.selection.pcv.chromosome :: out
) is det
<= ePRNG(R).

mutateGenePartnerSelection(Parameters, Index, !Distribution, !Random, !Chromosome) :-
	(if
		Index = 0
	then
		rng.distribution.unitGaussian(Perturb0, !Distribution, !Random),
		Perturb = Perturb0 * Parameters^poolSizeStdDev,
		NextPoolSize = int.max(0, !.Chromosome^poolSize + float.round_to_int(Perturb)),
		!:Chromosome = 'poolSize :='(!.Chromosome, NextPoolSize)
	else if
		Index = 1
	then
		rng.distribution.unitGaussian(Perturb0, !Distribution, !Random),
		Perturb = Perturb0 * Parameters^bitsPerProbabilityStdDev,
		NextBitsPerProbability = int.max(0, int.min(!.Chromosome^bitsPerProbability + float.round_to_int(Perturb), 30)),
		!:Chromosome = 'bitsPerProbability :='(!.Chromosome, NextBitsPerProbability)
	else if
		Index = 2
	then
		rng.distribution.unitGaussian(Perturb0, !Distribution, !Random),
		Perturb = Perturb0 * Parameters^probabilityUpdateFactorStdDev,
		NextProbabilityUpdateFactor = float.max(0.0, float.min(!.Chromosome^probabilityUpdateFactor + Perturb, 1.0)),
		!:Chromosome = 'probabilityUpdateFactor :='(!.Chromosome, NextProbabilityUpdateFactor)
	else if
		Index = 3
	then
		rng.distribution.unitGaussian(Perturb0, !Distribution, !Random),
		Perturb = Perturb0 * Parameters^payoffThresholdStdDev,
		NextPayoffThreshold = float.max(0.0, float.min(!.Chromosome^payoffThreshold + Perturb, 1.0)),
		!:Chromosome = 'payoffThreshold :='(!.Chromosome, NextPayoffThreshold)
	else
		throw("ebea.player.selection.mutateGenePartnerSelection/8: invalid gene index for partnerSelection chromosome")
	).





:- pred parsePartnerSelection(ebea.player.selection.pcv.chromosome, list(int), list(int)).
:- mode parsePartnerSelection(in, out, in) is det.
:- mode parsePartnerSelection(out, in, out) is semidet.

parsePartnerSelection(PS) -->
	parseable.int32(PS^poolSize),
	parseable.int32(PS^bitsPerProbability),
	parseable.float32(PS^probabilityUpdateFactor),
	parseable.float32(PS^payoffThreshold)
	.


:- pred printPartnerSelection(io.output_stream, ebea.player.selection.pcv.chromosome, io, io).
:- mode printPartnerSelection(in, in, di, uo) is det.

printPartnerSelection(Stream, PS, !IO) :-
	io.print(Stream, PS^poolSize, !IO),
	io.print(Stream, " ", !IO),
	io.print(Stream, PS^bitsPerProbability, !IO),
	io.print(Stream, " ", !IO),
	io.print(Stream, PS^probabilityUpdateFactor, !IO),
	io.print(Stream, " ", !IO),
	io.print(Stream, PS^payoffThreshold, !IO)
	.

:- func getCurrentChoice(ebea.player.selection.chromosome.chromosome) = maybe(int).

getCurrentChoice(random)                      = yes(0).
getCurrentChoice(normalPartnerSelection(_))   = yes(1).
getCurrentChoice(weightedPartnerSelection(_)) = yes(2).
getCurrentChoice(opinion_old(_, _))           = yes(3).
getCurrentChoice(opinion_old(_, _, _, _))     = yes(4).

:- func setChoice(ebea.player.selection.chromosome.chromosome, int) = setResult(ebea.player.selection.chromosome.chromosome).

setChoice(Chromosome, Index) = ok(Result) :-
	(if
		Index = 0
	then
		Chromosome = random,                       Result = Chromosome  ;
		Chromosome = normalPartnerSelection(_),    Result = random  ;
		Chromosome = weightedPartnerSelection(_),  Result = random  ;
		Chromosome = opinion_old(_, _),            Result = random  ;
		Chromosome = opinion_old(_, _, _, _),      Result = random
	else if
		Index = 1
	then
		Chromosome = random,                       Result = normalPartnerSelection(pcv_default_partnerSelection)  ;
		Chromosome = normalPartnerSelection(_),    Result = Chromosome  ;
		Chromosome = weightedPartnerSelection(_),  Result = normalPartnerSelection(pcv_default_partnerSelection)  ;
		Chromosome = opinion_old(_, _),            Result = normalPartnerSelection(pcv_default_partnerSelection)  ;
		Chromosome = opinion_old(_, _, _, _),      Result = normalPartnerSelection(pcv_default_partnerSelection)
	else if
		Index = 2
	then
		Chromosome = random,                       Result = weightedPartnerSelection(pcv_default_partnerSelection)  ;
		Chromosome = normalPartnerSelection(_),    Result = weightedPartnerSelection(pcv_default_partnerSelection)  ;
		Chromosome = weightedPartnerSelection(_),  Result = Chromosome  ;
		Chromosome = opinion_old(_, _),            Result = weightedPartnerSelection(pcv_default_partnerSelection)  ;
		Chromosome = opinion_old(_, _, _, _),      Result = weightedPartnerSelection(pcv_default_partnerSelection)
	else if
		Index = 3
	then
		Chromosome = random,                       Result = opinion_old(default_payoffThreshold_O, default_initialUncertainty)  ;
		Chromosome = normalPartnerSelection(_),    Result = opinion_old(default_payoffThreshold_O, default_initialUncertainty)  ;
		Chromosome = weightedPartnerSelection(_),  Result = opinion_old(default_payoffThreshold_O, default_initialUncertainty)  ;
		Chromosome = opinion_old(_, _),            Result = Chromosome  ;
		Chromosome = opinion_old(_, _, _, _),      Result = opinion_old(default_payoffThreshold_O, default_initialUncertainty)
	else if
		Index = 4
	then
		Chromosome = random,                       Result = opinion_old(default_initialAverageOpinion, default_initialStdDevOpinion, default_initialAverageUncertainty, default_initialStdDevUncertainty)  ;
		Chromosome = normalPartnerSelection(_),    Result = opinion_old(default_initialAverageOpinion, default_initialStdDevOpinion, default_initialAverageUncertainty, default_initialStdDevUncertainty)  ;
		Chromosome = weightedPartnerSelection(_),  Result = opinion_old(default_initialAverageOpinion, default_initialStdDevOpinion, default_initialAverageUncertainty, default_initialStdDevUncertainty)  ;
		Chromosome = opinion_old(_, _),            Result = opinion_old(default_initialAverageOpinion, default_initialStdDevOpinion, default_initialAverageUncertainty, default_initialStdDevUncertainty)  ;
		Chromosome = opinion_old(_, _, _, _),      Result = Chromosome
	else
		throw("ebea.player.selecion.chromosome.setChoice/2: invalid index")
	)
	.

:- func pcv_default_poolSize = int.

pcv_default_poolSize = 0.

:- func pcv_default_bitsPerProbability = int.

pcv_default_bitsPerProbability = 1.

:- func pcv_default_probabilityUpdateFactor = float.

pcv_default_probabilityUpdateFactor = 0.0.

:- func pcv_default_payoffThreshold = float.

pcv_default_payoffThreshold = 0.0.



:- func pcv_get_poolSize(ebea.player.selection.pcv.chromosome) = int.

pcv_get_poolSize(P) = P^poolSize.

:- func pcv_set_poolSize(ebea.player.selection.pcv.chromosome, int) = ebea.player.selection.pcv.chromosome.

pcv_set_poolSize(P, V) = 'poolSize :='(P, V).


:- func pcv_get_bitsPerProbability(ebea.player.selection.pcv.chromosome) = int.

pcv_get_bitsPerProbability(P) = P^bitsPerProbability.

:- func pcv_set_bitsPerProbability(ebea.player.selection.pcv.chromosome, int) = ebea.player.selection.pcv.chromosome.

pcv_set_bitsPerProbability(P, V) = 'bitsPerProbability :='(P, V).


:- func pcv_get_probabilityUpdateFactor(ebea.player.selection.pcv.chromosome) = float.

pcv_get_probabilityUpdateFactor(P) = P^probabilityUpdateFactor.

:- func pcv_set_probabilityUpdateFactor(ebea.player.selection.pcv.chromosome, float) = ebea.player.selection.pcv.chromosome.

pcv_set_probabilityUpdateFactor(P, V) = 'probabilityUpdateFactor :='(P, V).


:- func pcv_get_payoffThreshold(ebea.player.selection.pcv.chromosome) = float.

pcv_get_payoffThreshold(P) = P^payoffThreshold.

:- func pcv_set_payoffThreshold(ebea.player.selection.pcv.chromosome, float) = ebea.player.selection.pcv.chromosome.

pcv_set_payoffThreshold(P, V) = 'payoffThreshold :='(P, V).









:- func pcv_default_partnerSelection = ebea.player.selection.pcv.chromosome.

pcv_default_partnerSelection = partnerSelection(
	pcv_default_poolSize,
	pcv_default_bitsPerProbability,
	pcv_default_probabilityUpdateFactor,
	pcv_default_payoffThreshold
	).



:- func default_payoffThreshold_O = float.

default_payoffThreshold_O = 0.0.

:- func default_initialUncertainty = float.

default_initialUncertainty = 1.0.

:- func default_initialAverageOpinion = float.

default_initialAverageOpinion = 0.0.

:- func default_initialStdDevOpinion = float.

default_initialStdDevOpinion = 0.0.

:- func default_initialAverageUncertainty = float.

default_initialAverageUncertainty = 0.0.

:- func default_initialStdDevUncertainty = float.

default_initialStdDevUncertainty = 0.0.





:- func get_normalPartnerSelectionField(
	func(ebea.player.selection.pcv.chromosome) = T,
	ebea.player.selection.chromosome.chromosome
	) = T.

get_normalPartnerSelectionField(GetFunc, P) = R :-
	(	%
		P = random,                        I = pcv_default_partnerSelection  ;
		P = normalPartnerSelection(PS),    I = PS  ;
		P = weightedPartnerSelection(PS),  I = PS  ;
		P = opinion_old(_, _),             I = pcv_default_partnerSelection  ;
		P = opinion_old(_, _, _, _),       I = pcv_default_partnerSelection
	),
	R = GetFunc(I)
	.


:- func set_normalPartnerSelectionField(
	func(ebea.player.selection.pcv.chromosome, T) = ebea.player.selection.pcv.chromosome,
	ebea.player.selection.chromosome.chromosome,
	T
	) = ebea.player.selection.chromosome.chromosome.

set_normalPartnerSelectionField(SetFunc, P, V) = R :-
	(	%
		P = random,                        I = pcv_default_partnerSelection  ;
		P = normalPartnerSelection(PS),    I = PS  ;
		P = weightedPartnerSelection(PS),  I = PS  ;
		P = opinion_old(_, _),             I = pcv_default_partnerSelection  ;
		P = opinion_old(_, _, _, _),       I = pcv_default_partnerSelection
	),
	R = normalPartnerSelection(SetFunc(I, V))
	.



:- func get_weightedPartnerSelectionField(
	func(
		ebea.player.selection.pcv.chromosome
		) = T,
	ebea.player.selection.chromosome.chromosome
	) = T.

get_weightedPartnerSelectionField(GetFunc, P) = R :-
	(	%
		P = random,                        I = pcv_default_partnerSelection  ;
		P = normalPartnerSelection(PS),    I = PS  ;
		P = weightedPartnerSelection(PS),  I = PS  ;
		P = opinion_old(_, _),             I = pcv_default_partnerSelection  ;
		P = opinion_old(_, _, _, _),       I = pcv_default_partnerSelection
	),
	R = GetFunc(I)
	.


:- func set_weightedPartnerSelectionField(
	func(
		ebea.player.selection.pcv.chromosome,
		T
		) = ebea.player.selection.pcv.chromosome,
	ebea.player.selection.chromosome.chromosome,
	T
	) = ebea.player.selection.chromosome.chromosome.

set_weightedPartnerSelectionField(SetFunc, P, V) = R :-
	(	%
		P = random,                        I = pcv_default_partnerSelection  ;
		P = normalPartnerSelection(PS),    I = PS  ;
		P = weightedPartnerSelection(PS),  I = PS  ;
		P = opinion_old(_, _),             I = pcv_default_partnerSelection  ;
		P = opinion_old(_, _, _, _),       I = pcv_default_partnerSelection
	),
	R = weightedPartnerSelection(SetFunc(I, V))
	.



:- func get_payoffThreshold_O(ebea.player.selection.chromosome.chromosome) = float.

get_payoffThreshold_O(P) = R :-
	P = random,
	R = default_payoffThreshold_O
	;
	P = normalPartnerSelection(_),
	R = default_payoffThreshold_O
	;
	P = weightedPartnerSelection(_),
	R = default_payoffThreshold_O
	;
	P = opinion_old(_, _),
	R = P^payoffThreshold_O
	;
	P = opinion_old(_, _, _, _),
	R = default_payoffThreshold_O
	.

:- func set_payoffThreshold_O(ebea.player.selection.chromosome.chromosome, float) = ebea.player.selection.chromosome.chromosome.

set_payoffThreshold_O(P, V) = R :-
	P = random,
	R = opinion_old(V, default_initialUncertainty)
	;
	P = normalPartnerSelection(_),
	R = opinion_old(V, default_initialUncertainty)
	;
	P = weightedPartnerSelection(_),
	R = opinion_old(V, default_initialUncertainty)
	;
	P = opinion_old(_, _),
	R = 'payoffThreshold_O :='(P, V)
	;
	P = opinion_old(_, _, _, _),
	R = opinion_old(V, default_initialUncertainty)
	.


:- func get_initialUncertainty(ebea.player.selection.chromosome.chromosome) = float.

get_initialUncertainty(P) = R :-
	P = random,
	R = default_initialUncertainty
	;
	P = normalPartnerSelection(_),
	R = default_initialUncertainty
	;
	P = weightedPartnerSelection(_),
	R = default_initialUncertainty
	;
	P = opinion_old(_, _),
	R = P^initialUncertainty
	;
	P = opinion_old(_, _, _, _),
	R = default_initialUncertainty
	.

:- func set_initialUncertainty(ebea.player.selection.chromosome.chromosome, float) = ebea.player.selection.chromosome.chromosome.

set_initialUncertainty(P, V) = R :-
	P = random,
	R = opinion_old(default_payoffThreshold_O, V)
	;
	P = normalPartnerSelection(_),
	R = opinion_old(default_payoffThreshold_O, V)
	;
	P = weightedPartnerSelection(__),
	R = opinion_old(default_payoffThreshold_O, V)
	;
	P = opinion_old(_, _),
	R = 'initialUncertainty :='(P, V)
	;
	P = opinion_old(_, _, _, _),
	R = opinion_old(default_payoffThreshold_O, V)
	.


:- func get_initialAverageOpinion(ebea.player.selection.chromosome.chromosome) = float.

get_initialAverageOpinion(P) = R :-
	P = random,
	R = default_initialAverageOpinion
	;
	P = normalPartnerSelection(_),
	R = default_initialAverageOpinion
	;
	P = weightedPartnerSelection(_),
	R = default_initialAverageOpinion
	;
	P = opinion_old(_, _),
	R = default_initialAverageOpinion
	;
	P = opinion_old(_, _, _, _),
	R = P^initialAverageOpinion
	.

:- func set_initialAverageOpinion(ebea.player.selection.chromosome.chromosome, float) = ebea.player.selection.chromosome.chromosome.

set_initialAverageOpinion(P, V) = R :-
	P = random,
	R = opinion_old(V, default_initialStdDevOpinion, default_initialAverageUncertainty, default_initialStdDevUncertainty)
	;
	P = normalPartnerSelection(_),
	R = opinion_old(V, default_initialStdDevOpinion, default_initialAverageUncertainty, default_initialStdDevUncertainty)
	;
	P = weightedPartnerSelection(_),
	R = opinion_old(V, default_initialStdDevOpinion, default_initialAverageUncertainty, default_initialStdDevUncertainty)
	;
	P = opinion_old(_, _),
	R = opinion_old(V, default_initialStdDevOpinion, default_initialAverageUncertainty, default_initialStdDevUncertainty)
	;
	P = opinion_old(_, _, _, _),
	R = 'initialAverageOpinion :='(P, V)
	.


:- func get_initialStdDevOpinion(ebea.player.selection.chromosome.chromosome) = float.

get_initialStdDevOpinion(P) = R :-
	P = random,
	R = default_initialStdDevOpinion
	;
	P = normalPartnerSelection(_),
	R = default_initialStdDevOpinion
	;
	P = weightedPartnerSelection(_),
	R = default_initialStdDevOpinion
	;
	P = opinion_old(_, _),
	R = default_initialStdDevOpinion
	;
	P = opinion_old(_, _, _, _),
	R = P^initialStdDevOpinion
	.

:- func set_initialStdDevOpinion(ebea.player.selection.chromosome.chromosome, float) = ebea.player.selection.chromosome.chromosome.

set_initialStdDevOpinion(P, V) = R :-
	P = random,
	R = opinion_old(default_initialAverageOpinion, V, default_initialAverageUncertainty, default_initialStdDevUncertainty)
	;
	P = normalPartnerSelection(_),
	R = opinion_old(default_initialAverageOpinion, V, default_initialAverageUncertainty, default_initialStdDevUncertainty)
	;
	P = weightedPartnerSelection(_),
	R = opinion_old(default_initialAverageOpinion, V, default_initialAverageUncertainty, default_initialStdDevUncertainty)
	;
	P = opinion_old(_, _),
	R = opinion_old(default_initialAverageOpinion, V, default_initialAverageUncertainty, default_initialStdDevUncertainty)
	;
	P = opinion_old(_, _, _, _),
	R = 'initialStdDevOpinion :='(P, V)
	.


:- func get_initialAverageUncertainty(ebea.player.selection.chromosome.chromosome) = float.

get_initialAverageUncertainty(P) = R :-
	P = random,
	R = default_initialAverageUncertainty
	;
	P = normalPartnerSelection(_),
	R = default_initialAverageUncertainty
	;
	P = weightedPartnerSelection(_),
	R = default_initialAverageUncertainty
	;
	P = opinion_old(_, _),
	R = default_initialAverageUncertainty
	;
	P = opinion_old(_, _, _, _),
	R = P^initialAverageUncertainty
	.

:- func set_initialAverageUncertainty(ebea.player.selection.chromosome.chromosome, float) = ebea.player.selection.chromosome.chromosome.

set_initialAverageUncertainty(P, V) = R :-
	P = random,
	R = opinion_old(default_initialAverageOpinion, default_initialStdDevOpinion, V, default_initialStdDevUncertainty)
	;
	P = normalPartnerSelection(_),
	R = opinion_old(default_initialAverageOpinion, default_initialStdDevOpinion, V, default_initialStdDevUncertainty)
	;
	P = weightedPartnerSelection(_),
	R = opinion_old(default_initialAverageOpinion, default_initialStdDevOpinion, V, default_initialStdDevUncertainty)
	;
	P = opinion_old(_, _),
	R = opinion_old(default_initialAverageOpinion, default_initialStdDevOpinion, V, default_initialStdDevUncertainty)
	;
	P = opinion_old(_, _, _, _),
	R = 'initialAverageUncertainty :='(P, V)
	.


:- func get_initialStdDevUncertainty(ebea.player.selection.chromosome.chromosome) = float.

get_initialStdDevUncertainty(P) = R :-
	P = random,
	R = default_initialStdDevUncertainty
	;
	P = normalPartnerSelection(_),
	R = default_initialStdDevUncertainty
	;
	P = weightedPartnerSelection(_),
	R = default_initialStdDevUncertainty
	;
	P = opinion_old(_, _),
	R = default_initialStdDevUncertainty
	;
	P = opinion_old(_, _, _, _),
	R = P^initialStdDevUncertainty
	.

:- func set_initialStdDevUncertainty(ebea.player.selection.chromosome.chromosome, float) = ebea.player.selection.chromosome.chromosome.

set_initialStdDevUncertainty(P, V) = R :-
	P = random,
	R = opinion_old(default_initialAverageOpinion, default_initialStdDevOpinion, default_initialAverageUncertainty, V)
	;
	P = normalPartnerSelection(_),
	R = opinion_old(default_initialAverageOpinion, default_initialStdDevOpinion, default_initialAverageUncertainty, V)
	;
	P = weightedPartnerSelection(_),
	R = opinion_old(default_initialAverageOpinion, default_initialStdDevOpinion, default_initialAverageUncertainty, V)
	;
	P = opinion_old(_, _),
	R = opinion_old(default_initialAverageOpinion, default_initialStdDevOpinion, default_initialAverageUncertainty, V)
	;
	P = opinion_old(_, _, _, _),
	R = 'initialStdDevUncertainty :='(P, V)
	.



:- end_module ebea.player.selection.chromosome.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:

