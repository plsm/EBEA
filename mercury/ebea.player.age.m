/**
 * This module provides the types, predicates and functions to handle an
 * EBEA player's age.  The player's age increments in each algorithm
 * iteration.  The player may die of old age if EBEA is run in that mode.

 * @author Pedro Mariano
 * @version 1.0 2013/06/04
 */
:- module ebea.player.age.

:- interface.

:- import_module chromosome, foldable, parseable, rng.
:- import_module userInterface.

/**
 * Genes that control the ageing process of a player and death by old age.
 */

:- type chromosome.

/**
 * The phenotypic trait is the player's age.
 */
:- type trait == int.

/**
 * Parameters that control the death by old age process.
 */
:- type parameters --->
	noDeath ;
	deathByOldAge(
		oldAge         :: int,
		deathSuaveness :: float
	).

/**
 * Accumulator of age genes.
 */
:- type ac.

/**
 * defaultParameters = Result

 * Return a default set of parameters that control ageing process.
 */
:- func defaultParameters = ebea.player.age.parameters.


/**
 * initParameters(Parameters)

 * Unify {@code Parameters} with a default set of parameters that control ageing
 * process.
 */
:- pred initParameters(ebea.player.age.parameters).
:- mode initParameters(out) is det.

/**
 * initParameters = Result

 * Return a default set of parameters that control ageing process.
 */
:- func initParameters = ebea.player.age.parameters.

/**
 * initParameters(OldAge, DeathSuaveness, Parameters)

 * Unify {@code Parameters} with the given values of {@code OldAge} and {@code
 * DeathSuaveness}.  With these parameters, in each iteration of EBEA all
 * players go through a death by old age process.  If the parameters are
 * invalid, the predicate fails.


 * @param OldAge

 * @param DeathSuaveness
 */
:- pred initParameters(int, float, ebea.player.age.parameters).
:- mode initParameters(in, in, out) is semidet.

/**
 * initParameters(OldAge, DeathSuaveness) = Result

 * Return ageing process parameters initialised with the given values of {@code
 * OldAge} and {@code DeathSuaveness}.  With these parameters, in each iteration
 * of EBEA all players go through a death by old age process.  If the parameters
 * are invalid, the function throws an exception.

 * @param OldAge

 * @param DeathSuaveness
 */
:- func initParameters(int, float) = ebea.player.age.parameters.

/**
 * dialogParameters = Result

 * Returns a dialog to edit age parameters.
 */
:- func dialogParameters = list(dialogItem(ebea.player.age.parameters)).

/**
 * defaultChromosome = Result

 * Returns the value {@code plain}.  Currently there are no genes that control
 * the ageing process.
 */
:- func defaultChromosome = ebea.player.age.chromosome.

/**
 * Updates player age.  This function is called every iteration of EBEA.
 */
:- func stepClockTick(player(C, T)) = player(C, T).

/**
 * stepSurvive(Parameters, Player, !Random, MDeath)
  
 * Parameter is unified with {@code yes(oldAge)} if the given player dies
 * of old age.

 * <p>Parameter {@code Parameters} controls if there is a death by old age event.
  
 */
:- pred stepSurvive(
	ebea.player.age.parameters, player(C, T),
	R, R,
	bool)
	<= ePRNG(R).
:- mode stepSurvive(in, in, in, out, out) is det.


/**
 * Parse a chromosome to and from a list of bytes.
 */
:- pred parseChromosome(ebea.player.age.chromosome, list(int), list(int)).
:- mode parseChromosome(in, out, in) is det.
:- mode parseChromosome(out, in, out) is det.

/**
 * Parse the parameters that control ageing process to and from a list of bytes.
 */
:- pred parseParameters(ebea.player.age.parameters, list(int), list(int)).
:- mode parseParameters(in, out, in) is det.
:- mode parseParameters(out, in, out) is semidet.

/**
 * Parse an age phenotypic trait to and from a list of bytes.
 */
:- pred parseTrait(ebea.player.age.trait, list(int), list(int)).
:- mode parseTrait(in, out, in) is det.
:- mode parseTrait(out, in, out) is semidet.


:- instance chromosome(ebea.player.age.chromosome, ebea.player.age.trait, ebea.player.age.parameters).

:- instance foldable(ebea.player.age.chromosome, ebea.player.age.ac).

:- instance parseable(ebea.player.age.parameters).

:- instance printable(ebea.player.age.ac).

:- implementation.

:- import_module ebea.player.energy.
:- import_module rng, rng.distribution.
:- import_module exception, float, math.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

:- type chromosome --->
	plain.

:- type ac ---> noac.

:- instance parseable(ebea.player.age.parameters)
where
[
	pred(parse/3) is ebea.player.age.parseParameters
 ].

:- instance chromosome(ebea.player.age.chromosome, ebea.player.age.trait, ebea.player.age.parameters) where
[
	func(numberGenes/1) is ebea.player.age.numberGenes,
	pred(mutateGene/8)  is ebea.player.age.mutateGene,
	func(born/2)        is ebea.player.age.born
].

:- instance foldable(ebea.player.age.chromosome, ebea.player.age.ac)
	where
[
	func(fold/2)   is ebea.player.age.fold,
	func(initAC/0) is ebea.player.age.initAC
].

:- instance printable(ebea.player.age.ac)
	where
[
	pred(print/4) is ebea.player.age.printAc
].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

initParameters = noDeath.

initParameters(noDeath).

initParameters(OldAge, DeathSuaveness) = Result :-
	(if
		initParameters(OldAge, DeathSuaveness, P)
	then
		Result = P
	else
		throw("ebea.player.age.initParameters/2: invalid age parameters")
	).

initParameters(OldAge, DeathSuaveness, Parameters) :-
	OldAge > 0,
	DeathSuaveness >= 1.0,
	Parameters^oldAge = OldAge,
	Parameters^deathSuaveness = DeathSuaveness.

defaultParameters = deathByOldAge(150, 1.0).

dialogParameters =
	[
	di(label("age parameters"),  selectOneOf( selectedParameters, setParameters,
		[
		ci(label("no death"),    []),
		ci(label("death by old age"),
			[
			di(label("old age"),          updateFieldInt(   get_oldAge,          checkInt(   "old age",          bounded(0, no),   unbound, set_oldAge))),
			di(label("death suaveness"),  updateFieldFloat( get_deathSuaveness,  checkFloat( "death suaveness",  bounded(0.0, no), unbound, set_deathSuaveness)))
			])
		]))
	].

defaultChromosome = plain.

parseChromosome(plain) -->
	{true}.

parseParameters(noDeath) --> [0].
parseParameters(P) -->
	{P = deathByOldAge(_, _)},
	[1],
	parseable.int32(P^oldAge),
	parseable.float32(P^deathSuaveness).

parseTrait(Age) -->
	parseable.int32(Age).

stepClockTick(Player) = Result :-
	Result = 'traits :='(
		Player,
			'ageTrait :='(
			Player^traits,
			Player^traits^ageTrait + 1
		)
	).


stepSurvive(Parameters, Player, !Random, Dies) :-
	Parameters = noDeath,
	Dies = no
	;
	Parameters = deathByOldAge(_, _),
	deathProbability(Parameters^oldAge, Parameters^deathSuaveness, Player^traits^ageTrait) = Value,
	rng.flipCoin(Value, Dies, !Random)
	.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

/**
 * Given the parameters of the population dynamic and the population size
 * calculate the probability that a player dies.
 */

:- func deathProbability(int, float, int) = float.

%:- pragma memo(deathProbability/3).

deathProbability(OldAge, DeathSuaveness, PlayerAge) = Result :-
	Base = (float(OldAge - PlayerAge)) / DeathSuaveness,
	Result = 1.0 / (1.0 + math.exp(Base)).


/**
 * There are no age chromosome genes, so this function returns zero.

 * <p> This function is part of type class {@code
 * chromosome(ebea.player.age.chromosome, ebea.player.age.trait,
 * ebea.player.age.parameters, ebea.player.age.ac)}.

 */
:- func numberGenes(ebea.player.age.chromosome) = int.

numberGenes(plain) = 0.

/**
 * Mutate an EBEA age chromosome gene.

 * <p> This predicate is part of type class {@code
 * chromosome(ebea.player.age.chromosome, ebea.player.age.trait,
 * ebea.player.age.parameters, ebea.player.age.ac)}.
  
 */
:- pred mutateGene(ebea.player.age.parameters, int, distribution, distribution, R, R, ebea.player.age.chromosome, ebea.player.age.chromosome)
	<= ePRNG(R).
:- mode mutateGene(in, in, in, out, in, out, in, out) is erroneous.

mutateGene(Parameters, _Index, !Distribution, !Random, Chromosome, _Result) :-
	Chromosome = plain,
	(
		Parameters = noDeath
		;
		Parameters = deathByOldAge(_, _)
	),
	throw("ebea.player.age.mutateGene/8: No genes to mutate in age chromosome").


/**
 * Given an EBEA age chromosome return the age trait trait that can
 * develop from this chromosome.

 * <p> This function is part of type class {@code
 * chromosome(ebea.player.age.chromosome, ebea.player.age.trait,
 * ebea.player.age.parameters, ebea.player.age.ac)}.

 */
:- func born(ebea.player.age.parameters, ebea.player.age.chromosome) = ebea.player.age.trait.

born(Parameters, Chromosome) = Result :-
	Chromosome = plain,
	(
		Parameters = noDeath
		;
		Parameters = deathByOldAge(_, _)
	),
	Result = 0.

:- func initAC = ebea.player.age.ac.

initAC = noac.

:- func fold(ebea.player.age.chromosome, ebea.player.age.ac) = ebea.player.age.ac.

fold(plain, _AC) = noac.

:- pred printTrait(io.output_stream, ebea.player.age.trait, io.state, io.state).
:- mode printTrait(in, in, di, uo) is det.

printTrait(Stream, Trait, !IO) :-
	io.print(Stream, Trait, !IO).

:- pred printAc(io.output_stream, ebea.player.age.ac, io.state, io.state).
:- mode printAc(in, in, di, uo) is det.

printAc(_Stream, noac, !IO).


:- func selectedParameters(ebea.player.age.parameters) = maybe(int).

selectedParameters(noDeath)             = yes(0).
selectedParameters(deathByOldAge(_, _)) = yes(1).

:- func setParameters(ebea.player.age.parameters, int) = userInterface.setResult(ebea.player.age.parameters).

setParameters(OldValue, Index) = ok(NewValue) :-
	OldValue = noDeath,
	(if     Index = 0 then NewValue = OldValue
	else if Index = 1 then NewValue = deathByOldAge(default_oldAge, default_deathSuaveness)
	else throw("ebea.player.age.setParameters/2: invalid index")
	)
	;
	OldValue = deathByOldAge(_, _),
	(if     Index = 0 then NewValue = noDeath
	else if Index = 1 then NewValue = OldValue
	else throw("ebea.player.age.setParameters/2: invalid index")
	)
	.

:- func get_oldAge(ebea.player.age.parameters) = int.

get_oldAge(P) = R :-
	P = noDeath,
	R = default_oldAge
	;
	P = deathByOldAge(_, _),
	R = P^oldAge
	.

:- func set_oldAge(ebea.player.age.parameters, int) = ebea.player.age.parameters.

set_oldAge(P, V) = R :-
	P = noDeath,
	R = deathByOldAge(V, default_deathSuaveness)
	;
	P = deathByOldAge(_, _),
	R = 'oldAge :='(P, V)
	.


:- func get_deathSuaveness(ebea.player.age.parameters) = float.

get_deathSuaveness(P) = R :-
	P = noDeath,
	R = default_deathSuaveness
	;
	P = deathByOldAge(_, _),
	R = P^deathSuaveness
	.

:- func set_deathSuaveness(ebea.player.age.parameters, float) = ebea.player.age.parameters.

set_deathSuaveness(P, V) = R :-
	P = noDeath,
	R = deathByOldAge(default_oldAge, V)
	;
	P = deathByOldAge(_, _),
	R = 'deathSuaveness :='(P, V)
	.


:- func default_oldAge = int.

default_oldAge = 100.

:- func default_deathSuaveness = float.

default_deathSuaveness = 1.0.

:- end_module ebea.player.age.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
