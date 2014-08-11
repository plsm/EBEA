/**
 * This module provides the energy functionality of the Energy Based
 * Evolutionary Algorithm.  It provides a type that represents the genes
 * controlling how energy is handled by a player.  This type is an instance
 * of type class {@code chromosomes}.
 * 

 * @author Pedro Mariano
 * @version 2.0 2013/12/14
 */
:- module ebea.player.energy.

:- interface.

:- import_module ebea.population.
:- import_module userInterface, game.
:- import_module array.

/**
 * Represents the genes that are responsible for the energy dynamics of a
 * player in an EBEA.
 */

:- type chromosome --->
	plain.


/**
 * Represents the phenotype trait (behaviour) of the energy dynamics of a
 * player in an EBEA.
 */

:- type trait == float.

/**
 * Provides the parameters that control EBEA's energy dynamics of
 * reproducing behaviour.  A player produces an offspring when its energy
 * reaches the reproduction threshold represented by field {@code
 * energyReproduce}.  The parent always looses a fixed and variable amount of
 * energy.  The fixed amount is given by the sum of fields {@code
 * energyNewBorn} and {@code energyLostBirth}.  The values of these fields
 * are interpreted as fractions of {@code energyReproduce}.  The variable
 * amount is given by the root of two linear polynomials where the
 * denominator has the form <i>x</i> and the numerator has the form
 * <i>x+b</i>.  This last coefficient is the product of field {@code
 * coefficient} and the longevity parameter.
 */

:- type parameters --->
	parameters(
		energyScaling   :: energyScaling,
		energyReproduce :: float,
		energyNewborn   :: float,
		energyLostBirth :: float,
		coefficient     :: float
	).


/**
 * Represents how the payoff obtained in a game by a player is used to
 * update a player's energy.

 * @cons unscaled The payoff is added as is to the energy.  In this process
 * the player can die due to starvation when its energy drops below zero.
 * When a player reproduces its energy and the offspring's energy are set
 * to the birth energy.

 * @cons scaled The payoff is added but scaled to fall in the interval
 * {@code [-1,1]}.  In this process the player can also die due to
 * starvation.  Also, when a player reproduces its energy and the
 * offspring's energy are set to parameter <i>energy birth</i>.

 * @cons scaledPositive The payoff is added but scaled to fall in the
 * interval {@code [0,1]}.  In this process there is no death by starvation
 * as the player's energy never drops below zero.  When a player reproduces
 * its energy is decreased by the reproduction threshold.  The offspring's
 * energy is set to zero.
 */

:- type energyScaling --->
	unscaled ;
	scaled ;
	scaledPositive.

/**
 * The accumulator used to reduce a collection of energy chromosomes.
 */
:- type ac --->
	noac.

/**
 * defaultParameters = Result

 * Return a default value of {@code parameters} that can be used when
 * constructing new values of this type.
 */
:- func defaultParameters = ebea.player.energy.parameters.

/**
 * dialogParameters = Result

 * Return a dialog to display and edit values of {@code parameters} type.
 */
:- func dialogParameters = list(dialogItem(ebea.player.energy.parameters)).

/**
 * defaultChromosome = Result

 * Return a default value of {@code chromosome} that can be used when
 * constructing new values of this type.
 */
:- func defaultChromosome = ebea.player.energy.chromosome.

/**
 * canReproduce(Player, Parameters, NextTrait)
  
 * If this predicate succeeds then the corresponding player can reproduce
 * and parameter {@code NextEnergy} is unified with his remainder energy.
 */

:- pred canReproduce(ebea.player.player(C, T), ebea.player.parameters(P), ebea.player.energy.trait).
:- mode canReproduce(in, in, out) is semidet.


/**
 * stepPlayGame(EnergyParameters, Player, Partners, Payoffs, !NextRoundPopulation, !PlayerProfile, !Random)

 * Plays a game between {@code Player} and {@code Partners} and updates
 * their energy in {@code NextRoundPopulation}.  Parameters {@code Player}
 * and {@code Partners} are used to create the strategy profile.  We assume
 * the game is symmetric.

 */
:- pred stepPlayGame(
	ebea.player.energy.parameters, G,
	player(C, T), list(player(C, T)),
	maybe(array(float)),
	population(C, T), population(C, T),
	list(list(key)), list(list(key)),
	R, R)
	<= (asymmetricGame(G, C), ePRNG(R)).
:- mode stepPlayGame(in, in, in, in, out, in, out, in, out, in, out) is det.

%% ************************************************************************
%% stepPlayGame3(EnergyParameters, Player, Partners, Payoffs, !NextRoundPopulation, !PlayerProfile, !SiteActionAccumulator, !Random)
%%
%% Plays a game between {@code Player} and {@code Partners} and updates
%% their energy in {@code NextRoundPopulation} using the {@code game/3}
%% type-class.  Parameters {@code Player} and {@code Partners} are used to
%% create the strategy profile.  We assume the game is symmetric.
%%
%% <p>The {@code game/3} type-class returns an array with the actions
%% performed by the players.  This array is used to update the action
%% accumulator of the selecting player site.
%%
%%
:- pred stepPlayGame3(
	ebea.player.energy.parameters :: in,
	G                             :: in,
	player(CS, T)                 :: in,
	list(player(CS, T))           :: in,
	maybe(array(float)) :: out,
	population(CS, T)                       :: in,  population(CS, T)                       :: out,
	list(list(ebea.population.players.key)) :: in,  list(list(ebea.population.players.key)) :: out,
	R                                       :: in,  R                                       :: out,
	array(AA) :: di,  array(AA) :: uo
) is det
	<= (
	asymmetricGame(G, CS, A),
	ePRNG(R),
	foldable(A, AA)
).

/**
 * stepSurvive(Parameters, Player, !Random, Dies)
  
 * Unifies {@code Dies} with {@code yes} if the player does not have
 * sufficient energy.
 */

:- pred stepSurvive(
	ebea.player.energy.parameters, player(C, T),
	R, R,
	bool)
	<= ePRNG(R).
:- mode stepSurvive(in, in, in, out, out) is det.


/**
 * scalePayoff(Game, EnergyScaling, Payoff) = Result
  
 * Compute the scaled payoff in the given game context.
 */
:- func scalePayoff(G, energyScaling, float) = float
	<= abstractGame(G).


:- pred parseChromosome(ebea.player.energy.chromosome, list(int), list(int)).
:- mode parseChromosome(in, out, in) is det.
:- mode parseChromosome(out, in, out) is det.

:- pred parseParameters(ebea.player.energy.parameters, list(int), list(int)).
:- mode parseParameters(in, out, in) is det.
:- mode parseParameters(out, in, out) is semidet.

:- pred parseTrait(ebea.player.energy.trait, list(int), list(int)).
:- mode parseTrait(in, out, in) is det.
:- mode parseTrait(out, in, out) is semidet.

:- pred test(io.state, io.state).
:- mode test(di, uo) is det.

:- instance chromosome(ebea.player.energy.chromosome, ebea.player.energy.trait, ebea.player.energy.parameters).

:- instance foldable(ebea.player.energy.chromosome, ebea.player.energy.ac).

:- instance printable(ebea.player.energy.chromosome).

%:- instance printable(ebea.player.energy.trait).

:- instance printable(ebea.player.energy.ac).

:- implementation.

:- import_module array.
:- import_module parseable.
:- import_module util.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

:- instance chromosome(ebea.player.energy.chromosome, ebea.player.energy.trait, ebea.player.energy.parameters)
	where
[
	func(numberGenes/1)  is ebea.player.energy.numberGenes,
	pred(mutateGene/8)   is ebea.player.energy.mutateGene,
	func(born/2)         is ebea.player.energy.born
].

:- instance foldable(ebea.player.energy.chromosome, ebea.player.energy.ac)
	where
[
	func(fold/2)   is ebea.player.energy.fold,
	func(initAC/0) is ebea.player.energy.fold
].

:- instance printable(ebea.player.energy.chromosome)
	where
[
	pred(print/4) is ebea.player.energy.printChromosome
].

% :- instance printable(ebea.player.energy.trait)
% 	where
% [
% 	pred(print/4) is ebea.player.energy.printTrait
% ].

:- instance printable(ebea.player.energy.ac)
	where
[
	pred(print/4) is ebea.player.energy.printAccumulator
].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

defaultParameters = Result :-
	Result^energyScaling   = default_energyScaling,
	Result^energyReproduce = default_energyReproduce,
	Result^energyNewborn   = default_energyNewborn,
	Result^energyLostBirth = default_energyLostBirth,
	Result^coefficient     = default_coefficient.



dialogParameters =
	[
	di(label("energy scaling"),    'new editField'(  get_energyScaling,    set(set_energyScaling),
		[
		di(label("unscaled"),       newValue(unscaled)),
		di(label("scaled"),         newValue(scaled)),
		di(label("scaledPositive"), newValue(scaledPositive))
		])),
	di(label("energy reproduce"),  updateFieldFloat( get_energyReproduce,  checkFloat( "energyReproduce",  bounded(0.0, no), unbound, set_energyReproduce))),
	di(label("energy newborn (as % of energy reproduce)"),    updateFieldFloat( get_energyNewborn,    set_energyNewborn)),
	di(label("energy lost birth (as % of energy reproduce)"), updateFieldFloat( get_energyLostBirth,  set_energyLostBirth)),
	di(label("coefficient"),       updateFieldFloat( get_coefficient,      checkFloat( "coefficient",      bounded(0.0, yes), unbound, set_coefficient)))
	].

defaultChromosome = plain.

canReproduce(Player, Parameters, NextTraits) :-
	Player^chromosome^energyGenes = plain,
	Player^traits^energyTrait >= Parameters^energyPar^energyReproduce,
	NextEnergy = Player^traits^energyTrait - energyLostGivingBirth(Parameters, float(Player^traits^ageTrait)),
	NextEnergy >= 0.0,
	NextTraits = NextEnergy
	.

stepPlayGame(
	EnergyParameters, Game, ForPlayer, Partners, MPayoffs,
	!NextRoundPopulation,
	!PlayerProfiles,
	!Random) :-
%	trace [io(!IO)] (io.format("ebea.player.energy.round/9:\n\tplayer: %s\n\tpartners: %s\n", [s(string(ForPlayer)), s(string(Partners))], !IO)),
	
	NumberPlayers = game.numberPlayers(Game),
	InitProfile =
	(func(Index) = Element :-
		(if
			Index = 0
		then
			Element = ForPlayer^chromosome^strategyGenes
		else
			Partner = list.det_index0(Partners, Index - 1),
			Element = Partner^chromosome^strategyGenes
		)
	),
	Profile = array.generate(NumberPlayers, InitProfile),
	PlayerProfile = [ForPlayer^id | list.map(ebea.player.'ID', Partners)],
	!:PlayerProfiles = [PlayerProfile | !.PlayerProfiles],
	game.playAsymmetric(Game, Profile, !Random, MPayoffs),
	(
		MPayoffs = yes(Payoffs),
		UpdateEnergies =
		(pred(Player::in, Payoff::in, NRPin::in, NRPout::out) is det :-
			ebea.population.update(
				Player^id,
				ebea.player.energy.updateEnergy(
					EnergyParameters^energyScaling,
					Game,
					Payoff
				),
				NRPin
			) = NRPout
		),
		list.foldl_corresponding(UpdateEnergies, [ForPlayer | Partners], array.to_list(Payoffs), !NextRoundPopulation)
		;
		MPayoffs = no
	).

stepPlayGame3(	
	EnergyParameters,
	Game,
	ForPlayer,
	Partners,
	MPayoffs,
	!NextRoundPopulation,
	!PlayerProfiles,
	!Random,
	!SiteActionAccumulator
) :-
	
	NumberPlayers = game.numberPlayers(Game),
	InitProfile =
	(func(Index) = Element :-
		(if
			Index = 0
		then
			Element = ForPlayer^chromosome^strategyGenes
		else
			Partner = list.det_index0(Partners, Index - 1),
			Element = Partner^chromosome^strategyGenes
		)
	),
	Profile = array.generate(NumberPlayers, InitProfile),
	PlayerProfile = [ForPlayer^id | list.map(ebea.player.'ID', Partners)],
	!:PlayerProfiles = [PlayerProfile | !.PlayerProfiles],
	game.playAsymmetric(Game, Profile, !Random, MPayoffs, MActions),
	(	%
		MPayoffs = yes(Payoffs),
		MActions = yes(Actions),

		UpdateEnergies =
		(pred(Player::in, Payoff::in, NRPin::in, NRPout::out) is det :-
			ebea.population.update(
				Player^id,
				ebea.player.energy.updateEnergy(
					EnergyParameters^energyScaling,
					Game,
					Payoff
				),
				NRPin
			) = NRPout
		),
		list.foldl_corresponding(UpdateEnergies, [ForPlayer | Partners], array.to_list(Payoffs), !NextRoundPopulation),
		PreviousActionAccumulator = util.arrayUnsafeLookup(!.SiteActionAccumulator, ForPlayer^siteIndex),
		array.foldl(foldable.fold, Actions, PreviousActionAccumulator) = NextActionAccumulator,
		util.arrayUnsafeSet(ForPlayer^siteIndex, NextActionAccumulator, !SiteActionAccumulator)
	;
		MPayoffs = no,
		MActions = no
	;
		MPayoffs = yes(_),
		MActions = no,
		throw("stepPlayGame3/13: invalid combination of MPayoffs and MActions")
	;	
		MPayoffs = no,
		MActions = yes(_),
		throw("stepPlayGame3/13: invalid combination of MPayoffs and MActions")
	).

stepSurvive(Parameters, Player, !Random, Death) :-
	Parameters^energyScaling = scaled,
	(if
		Player^traits^energyTrait < 0.0
	then
		Death = yes
	else
		Death = no
	)
	;
	Parameters^energyScaling = unscaled,
	(if
		Player^traits^energyTrait < 0.0
	then
		Death = yes
	else
		Death = no
	)
	;
	Parameters^energyScaling = scaledPositive,
	Death = no
	.

%:- pragma memo(scalePayoff/3).

scalePayoff(_, unscaled, Payoff) = Payoff.

scalePayoff(Game, scaled, Payoff) = Payoff / Scale :-
	Scale = float.max(float.abs(game.lowestPayoff(Game)), float.abs(game.highestPayoff(Game))).

scalePayoff(Game, scaledPositive, Payoff) = Result :-
	Scale = game.highestPayoff(Game) - game.lowestPayoff(Game),
	Result = (Payoff - game.lowestPayoff(Game)) / Scale.

parseChromosome(plain) -->
	{true}.

parseParameters(P) -->
	(
		{P^energyScaling = unscaled},       [0] ;
		{P^energyScaling = scaled},         [1] ;
		{P^energyScaling = scaledPositive}, [2]
	),
	parseable.float32(P^energyReproduce),
	parseable.float32(P^energyNewborn),
	parseable.float32(P^energyLostBirth),
	parseable.float32(P^coefficient).

parseTrait(Energy) -->
	parseable.float32(Energy).

test(!IO) :-
	io.print("Enter energy reproduce, energy newborn (as % e_R), energy lost birth (as % e_R) and coefficient:\n", !IO),
	io.read_line_as_string(IParEne, !IO),
	io.print("Enter old age:\n", !IO),
	io.read_line_as_string(IParAge, !IO),
	(if
		IParEne = ok(ParEne),
		string.words(ParEne) = [SEnergyReproduce, SEnergyNewborn, SEnergyLostBirth, SCoefficient],
		string.to_float(SEnergyReproduce, PE^energyReproduce),
		string.to_float(SEnergyNewborn,   PE^energyNewborn),
		string.to_float(SEnergyLostBirth, PE^energyLostBirth),
		string.to_float(SCoefficient,     PE^coefficient),
		PE^energyScaling = scaled,
		IParAge = ok(ParAge),
		string.words(ParAge) = [SOldAge],
		string.to_int(SOldAge, PA^oldAge),
		PA^deathSuaveness = 1.0
	then
		P^mutationProbability = 0.0,
		P^agePar = PA,
		P^energyPar = PE,
		P^selectionPar = defaultParameters,
		P^gamePar = 1,
		PredPrintELGB =
		(pred(Age::in, IOdi::di, IOuo::uo) is det :-
			io.format(" %5.1f", [f(energyLostGivingBirth(P, float(Age)))], IOdi, IOuo)
		),
		int.fold_up(PredPrintELGB, 0, 3 * PA^oldAge / 2, !IO),
		io.nl(!IO),
		test(!IO)
	else
		true
	). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

/**
 * Return the energy lost by the given player if he gives birth to a new player.
 */
:- func energyLostGivingBirth(ebea.player.parameters(P), float) = float.

energyLostGivingBirth(Parameters, Age) =
	(if
		Parameters^energyPar^coefficient > 0.0,
		Parameters^agePar^oldAge = OldAge
	then
		Age
		/ (
			  Age
			+ Parameters^energyPar^coefficient * float(OldAge)
		  )
		* (
			  1.0
			- Parameters^energyPar^energyNewborn
			- Parameters^energyPar^energyLostBirth
		  )
		* Parameters^energyPar^energyReproduce
	else
		0.0
	)
	+
	Parameters^energyPar^energyReproduce
	* (
		  Parameters^energyPar^energyNewborn
		+ Parameters^energyPar^energyLostBirth
	  )
	.

/**
 * updateEnergy(EnergyGainProcess, Game, Payoff, Player) = Result
  
 * Updates the energy of a player after he has played a game.  Parameter
 * {@code EnergyGainProcess} specifies how this is done.
  
 */
:- func updateEnergy(energyScaling, G, float, ebea.player.player(C, T)) = ebea.player.player(C, T)
	<= abstractGame(G).

updateEnergy(unscaled, _, Payoff, Player) = Result :-
	Result = 'traits :='(
		Player,
		'energyTrait :='(
			Player^traits,
			Player^traits^energyTrait + Payoff
		)
	).

updateEnergy(scaled, Game, Payoff, Player) = Result :-
	Scale = float.max(float.abs(game.lowestPayoff(Game)), float.abs(game.highestPayoff(Game))),
	Result = 'traits :='(
		Player,
		'energyTrait :='(
			Player^traits,
			Player^traits^energyTrait + Payoff / Scale
		)
	).

updateEnergy(scaledPositive, Game, Payoff, Player) = Result :-
	Scale = game.highestPayoff(Game) - game.lowestPayoff(Game),
	Result = 'traits :='(
		Player,
		'energyTrait :='(
			Player^traits,
			Player^traits^energyTrait + (Payoff - game.lowestPayoff(Game)) / Scale
		)
	).





/**
 * The number of energy genes of a EBEA chromosome is zero.
 */
:- func numberGenes(ebea.player.energy.chromosome) = int.

numberGenes(Chromosome) = Result :-
	Chromosome = plain,
	Result = 0.

/**
 * Mutate an EBEA chromosome gene.

 * <p> This predicate is part of type class {@code
 * chromosome(ebea.player.energy.chromosome, ebea.player.energy.traits,
 * ebea.player.energy.parameters, ebea.player.energy.ac)}.
  
 */
:- pred mutateGene(ebea.player.energy.parameters, int, distribution, distribution, R, R, ebea.player.energy.chromosome, ebea.player.energy.chromosome)
	<= ePRNG(R).
:- mode mutateGene(in, in, in, out, in, out, in, out) is erroneous.

mutateGene(_Parameters, _Index, !Distribution, !Random, Chromosome, _Result) :-
	Chromosome = plain,
	throw("ebea.player.energy.mutateGene/8: No genes to mutate in chromosome group plain").

/**
 * Given an EBEA chromosome return the individual that can develop from
 * this chromosome.

 * <p> This function is part of type class {@code
 * chromosome(ebea.player.energy.chromosome, ebea.player.energy.trait,
 * ebea.player.energy.parameters, ebea.player.energy.ac)}.

 */
:- func born(ebea.player.energy.parameters, ebea.player.energy.chromosome) = ebea.player.energy.trait.

born(Parameters, Chromosome) = Result :-
	Chromosome = plain,
	Result = Parameters^energyNewborn * Parameters^energyReproduce
	.




/**
 * Return the initial value of the accumulator.

 * <p> This function is part of type class {@code
 * chromosome(ebea.player.energy.chromosome, ebea.player.energy.traits,
 * ebea.player.energy.parameters, ebea.player.energy.ac)}.

 */
:- func fold = ebea.player.energy.ac.

fold = Result :-
	Result = noac.

/**

 * <p> This function is part of type class {@code
 * chromosome(ebea.player.energy.chromosome, ebea.player.energy.traits,
 * ebea.player.energy.parameters, ebea.player.energy.ac)}.

 */
:- func fold(ebea.player.energy.chromosome, ebea.player.energy.ac) = ebea.player.energy.ac.

fold(Chromosome, _) = noac :-
	Chromosome = plain.




/**
 * Print an EBEA chromosome.

 * <p> This predicate is part of type class {@code
 * chromosome(ebea.player.energy.chromosome, ebea.player.energy.traits,
 * ebea.player.energy.parameters, ebea.player.energy.ac)}.
  
 */
:- pred printChromosome(io.output_stream, ebea.player.energy.chromosome, io, io).
:- mode printChromosome(in, in, di, uo) is det.

printChromosome(Stream, Chromosome, !IO) :-
	Chromosome = plain,
	io.print(Stream, "plain", !IO).



:- pred printAccumulator(io.output_stream, ebea.player.energy.ac, io, io).
:- mode printAccumulator(in, in, di, uo) is det.

printAccumulator(_Stream, AC, !IO) :-
	AC = noac
	.


:- pred printTrait(io.output_stream, ebea.player.energy.trait, io, io).
:- mode printTrait(in, in, di, uo) is det.

printTrait(Stream, Trait, !IO) :-
	io.print(Stream, Trait, !IO).



:- func get_energyScaling(ebea.player.energy.parameters) = energyScaling.

get_energyScaling(P) = P^energyScaling.


:- func set_energyScaling(ebea.player.energy.parameters, energyScaling) = ebea.player.energy.parameters.

set_energyScaling(P, V) = 'energyScaling :='(P, V).



:- func get_energyReproduce(ebea.player.energy.parameters) = float.

get_energyReproduce(P) = P^energyReproduce.


:- func set_energyReproduce(ebea.player.energy.parameters, float) = ebea.player.energy.parameters.

set_energyReproduce(P, V) = 'energyReproduce :='(P, V).



:- func get_energyNewborn(ebea.player.energy.parameters) = float.

get_energyNewborn(P) = P^energyNewborn.


:- func set_energyNewborn(ebea.player.energy.parameters, float) = setResult(ebea.player.energy.parameters).

set_energyNewborn(P, V) =
	(if
		V + P^energyLostBirth =< 1.0,
		V + P^energyLostBirth > 0.0
	then
		ok('energyNewborn :='(P, V))
	else
		error("The sum of energy new born and energy lost birth must be greater than zero and less than or equal to one")
	).



:- func get_energyLostBirth(ebea.player.energy.parameters) = float.

get_energyLostBirth(P) = P^energyLostBirth.


:- func set_energyLostBirth(ebea.player.energy.parameters, float) = setResult(ebea.player.energy.parameters).

set_energyLostBirth(P, V) =
	(if
		V + P^energyNewborn =< 1.0,
		V + P^energyNewborn > 0.0
	then
		ok('energyLostBirth :='(P, V))
	else
		error("The sum of 'energy lost birth' and 'energy new born' must be greater than zero and less than or equal to one")
	).



:- func get_coefficient(ebea.player.energy.parameters) = float.

get_coefficient(P) = P^coefficient.


:- func set_coefficient(ebea.player.energy.parameters, float) = ebea.player.energy.parameters.

set_coefficient(P, V) = 'coefficient :='(P, V).



:- func default_energyScaling = energyScaling.

default_energyScaling = scaled.

:- func default_energyReproduce = float.

default_energyReproduce = 50.0.

:- func default_energyNewborn = float.

default_energyNewborn = 0.1.

:- func default_energyLostBirth = float.

default_energyLostBirth = 0.1.

:- func default_coefficient = float.

default_coefficient = 1.0.

:- end_module ebea.player.energy.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
