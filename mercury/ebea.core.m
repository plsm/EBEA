/**
 * The core module with the implementation of the main algorithm of the
 * Energy Based Evolutionary Algorithm (EBEA).  The main predicate of this
 * module performs an iteration of the EBEA.

  
 * @author Pedro Mariano
 * @version 1.0 2012/09/20
 */
:- module ebea.core.

:- interface.

:- import_module ebea.population, ebea.streams, rng.distribution.
:- import_module rng, game, chromosome, printable, foldable, parseable.
:- import_module bool, io.

/**
 * Parameters required to run an Energy Based Evolutionary Algorithm.  It
 * contains the game players are going to play, parameters that control
 * players' behaviour and streams where statistical data is recorded.
 */

:- type data(G, P).

:- type data(G, P, AA).

/**
 * Statistics about the events in an EBEA run.
 */
:- type stats(A).

/**
 * Modes that EBEA can be run.
 */
:- type runMode(C, T) --->
	background ;
	interactively(
		first :: processPred(C, T),
		itera :: interactivePred(C, T),
		final :: processPred(C, T)
	)
	.

:- inst runMode ==
	bound(
		background ;
		interactively(processPred, interactivePred, processPred)
	).

:- type interactivePred(C, T) == pred(population(C, T), int, bool, io.state, io.state).

:- inst interactivePred == (pred(in, in, out, di, uo) is det).

:- type processPred(C, T) == pred(population(C, T), io.state, io.state).

:- inst processPred == (pred(in, di, uo) is det).

:- type pop --->
	some [C, T] pop(population(C, T)).

/**
 * initData2(Game, Parameters, Streams, Population, Data, Stats)
  
 * Initialise data and statistics necessary to interactively run EBEA.  The
 * data contains the game, population parameters and output streams.  This
 * data controls all aspects of an EBEA run bar the number of iterations.
 * The statistics are used when we only write a run summary.
  
 */
:- pred initData2(
	G                             :: in,
	ebea.population.parameters(P) :: in,
	ebea.streams.outStreams       :: in,
	population(CS, T)             :: in,
	data(G, P) :: out,
	stats(AS)  :: out
) is det
	<= (
	chromosome(CS, T, P),
	asymmetricGame(G, CS),
	foldable(CS, AS)
).

%% ************************************************************************
%% initData3(Game, Parameters, Streams, Population, Data, Stats)
%%
%% Initialise data and statistics necessary to interactively run EBEA.  The
%% data contains the game, population parameters and output streams.  This
%% data controls all aspects of an EBEA run bar the number of iterations.
%% The statistics are used when we only write a run summary.
%%
%% @param G The game used by players (values of the payoff matrix, number
%% of players...).
%%
%% @param P Other parameters of the game (mutation operator parameters...).
%%
%% @param AA The game actions accumulator.
%%
%% @param CS Players' strategy genes and game strategies.
%%
%% @param T Players' phenotype that result from strategy genes.
%%
%% @param A Game actions.
%%
%% @param AS The strategy accumulator used to reduce the strategy genes in
%% every iteration.
%%
:- pred initData3(
	G                                 :: in,
	ebea.population.parameters(P, AA) :: in,
	ebea.streams.outStreams           :: in,
	population(CS, T)                 :: in,
	data(G, P, AA) :: out,
	stats(AS)      :: out
) is det
	<= (
	chromosome(CS, T, P),
	asymmetricGame(G, CS, A),
	foldable(CS, AS),
	foldable(A, AA)
).

/**
 * Perform an iteration of EBEA and update the current population and
 * statistics.

 * This predicate can be used to interactively run EBEA.
  
 */
:- pred iterationData2(
	ebea.core.data(G, P), int,
	population(C, T), population(C, T),
	stats(A), stats(A),
	distribution, distribution,
	R, R,
	io.state, io.state)
	<= (ePRNG(R), asymmetricGame(G, C), chromosome(C, T, P), foldable(C, A), parseable(C), printable(C), printable(T), printable(A)).
:- mode iterationData2(in, in, in, out, in, out, in, out, in, out, di, uo) is det.


%% ************************************************************************
%% iterationData3(Data, IterationNumber, !Population, !Stats, !Distribution, !Random, !IO)
%%
%% Perform an iteration of EBEA and update the current population and
%% statistics using type {@code data/3}.
%%
%% This predicate can be used to interactively run EBEA.
%%
%% @param G The game used by players (values of the payoff matrix, number
%% of players...).
%%
%% @param P Other parameters of the game (mutation operator parameters...).
%%
%% @param A Game actions
%%
%% @param CS Players' strategy genes and game strategies.
%%
%% @param T Players' phenotype that result from strategy genes.
%%
%% @param AS The strategy accumulator used to reduce the strategy genes in
%% every iteration.
%%
%% @param AA The game actions accumulator.
%%
:- pred iterationData3(
	ebea.core.data(G, P, AA) :: in,
	int                      :: in,
	population(CS, T) :: in,  population(CS, T) :: out,
	stats(ACS)        :: in,  stats(ACS)        :: out,
	distribution      :: in,  distribution      :: out,
	R                 :: in,  R                 :: out,
	io.state          :: di,  io.state          :: uo
) is det
	<= (
	ePRNG(R),
	asymmetricGame(G, CS, A),
	chromosome(CS, T, P),
	foldable(CS, ACS),
	foldable(A, AA),
	parseable(CS),
	printable(CS),
	printable(T),
	printable(ACS)
).


/**
 * run(Mode, Game, Parameters, Streams, NumberIterations, Population, !Distribution, !Random, !IO)

 * Perform the given number of iterations of EBEA for the given game,
 * parameters and initial population.  Parameters {@code !Distribution} and
 * {@code !Random} are the source of pseudo-random numbers and random
 * distributions.
  
 */
:- pred runGame2(runMode(C, T), G, ebea.population.parameters(P), ebea.streams.outStreams, int, population(C, T), distribution, distribution, R, R, io.state, io.state)
	<= (ePRNG(R), asymmetricGame(G, C), chromosome(C, T, P), foldable(C, A), parseable(C), printable(C), printable(T), printable(A)).
:- mode runGame2(in(runMode), in, in, in, in, in,  in, out, in, out, di, uo) is det.


%% ************************************************************************
%% runGame3(Mode, Game, Parameters, Streams, NumberIterations, Population, !Distribution, !Random, !IO)
%%
%% Perform a run of EBEA using a game that conforms to the {@code
%% asymmetricGame/3} type-class.  
%%
%% @param G The game used by players (values of the payoff matrix, number
%% of players...).
%%
%% @param P Other parameters of the game (mutation operator parameters...).
%%
%% @param AA The game actions accumulator.
%%
%% @param CS Players' strategy genes and game strategies.
%%
%% @param T Players' phenotype that result from strategy genes.
%%
%% @param A Game actions.
%%
%% @param ACS The strategy accumulator used to reduce the strategy genes in
%% every iteration.
%%
:- pred runGame3(
	runMode(CS, T)                    :: in(runMode),
	G                                 :: in,
	ebea.population.parameters(P, AA) :: in,
	ebea.streams.outStreams           :: in,
	int                               :: in,
	population(CS, T)                 :: in,
	distribution :: in,  distribution :: out,
	R            :: in,  R            :: out,
	io.state     :: di,  io.state     :: uo
) is det
	<= (
	ePRNG(R),
	asymmetricGame(G, CS, A),
	chromosome(CS, T, P),
	foldable(CS, ACS),
	foldable(A, AA),
	parseable(CS),
	printable(CS),
	printable(T),
	printable(ACS)
).


:- implementation.

:- import_module ebea.player, ebea.player.chromosome, ebea.player.age,
ebea.player.selection, ebea.population.players, ebea.population.site,
ebea.population.neighbours,
ebea.streams.birth, ebea.streams.death, ebea.streams.phenotype,
ebea.streams.siteState,
ebea.streams.playerProfile.

:- import_module util.
:- import_module array, benchmarking, char, int, list, maybe, solutions, string.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

:- type data(G, P) --->
	data(
		game  :: G,
		gp    :: ebea.population.parameters(P),
		s     :: ebea.streams.outStreams
	).

:- type data(G, P, AA) --->
	data(
		game3       :: G,
		parameters3 :: ebea.population.parameters(P, AA),
		streams3    :: ebea.streams.outStreams
	).

:- type stats(A) --->
	stats(
		reduceEvolution        :: ebea.player.ac(A),
		deathsCarryingCapacity :: int,
		deathsOldAge           :: int,
		deathsStarvation       :: int,
		births                 :: int
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

initData2(Game, Parameters, Streams, Population, data(Game, Parameters, Streams), Stats) :-
	Stats^reduceEvolution = ebea.population.fold_players(ebea.player.foldChromosome, Population, ebea.player.initAc),
	Stats^deathsCarryingCapacity = 0,
	Stats^deathsOldAge = 0,
	Stats^deathsStarvation = 0,
	Stats^births = 0.

initData3(Game, Parameters, Streams, Population, data(Game, Parameters, Streams), Stats) :-
	Stats^reduceEvolution = ebea.population.fold_players(ebea.player.foldChromosome, Population, ebea.player.initAc),
	Stats^deathsCarryingCapacity = 0,
	Stats^deathsOldAge = 0,
	Stats^deathsStarvation = 0,
	Stats^births = 0.

runGame2(Mode, Game, Parameters, Streams, NumberIterations, Population, !Distribution, !Random, !IO) :-
	initData2(Game, Parameters, Streams, Population, Data, Stats),
	printInitialDataToStreams(Data^s, Population, !IO),
	(
		Mode = background
		;
		Mode = interactively(_, _, _),
		(Mode^first)(Population, !IO)
	),
	runLoopData2(Mode, Data, NumberIterations, 0, Population, FinalPopulation, Stats, _, !Distribution, !Random, !IO),
	(
		Mode = background
		;
		Mode = interactively(_, _, _),
		(Mode^final)(FinalPopulation, !IO)
	).

runGame3(Mode, Game, Parameters, Streams, NumberIterations, Population, !Distribution, !Random, !IO) :-
	initData3(Game, Parameters, Streams, Population, Data, Stats),
	printInitialDataToStreams(Data^streams3, Population, !IO),
	(
		Mode = background
		;
		Mode = interactively(_, _, _),
		(Mode^first)(Population, !IO)
	),
	runLoopData3(Mode, Data, NumberIterations, 0, Population, FinalPopulation, Stats, _, !Distribution, !Random, !IO),
	(
		Mode = background
		;
		Mode = interactively(_, _, _),
		(Mode^final)(FinalPopulation, !IO)
	).

iterationData2(Data, IterationNumber, !Population, ThisStats, NextStats, !Distribution, !Random, !IO) :-
	% io.print(!.Population, !IO),
	% io.nl(!IO),
	% io.print('\r', !IO), io.print(IterationNumber, !IO), io.print(' ', !IO), io.print('a', !IO), io.flush_output(io.stdout_stream, !IO),
	ebea.population.fold3_PlayerNeighbour(
		ebea.player.selection.stepSelectPartnersPlayGame2(Data^gp^playerParameters, Data^game),
		!.Population,
		!Population,
		[], PlayerProfiles,
		!Random),
	% io.print('\r', !IO), io.print(IterationNumber, !IO), io.print(' ', !IO), io.print('b', !IO), io.flush_output(io.stdout_stream, !IO),
	ebea.population.map_players(ebea.player.age.stepClockTick, !Population),
	% io.print('\r', !IO), io.print(IterationNumber, !IO), io.print(' ', !IO), io.print('c', !IO), io.flush_output(io.stdout_stream, !IO),
	ebea.population.stepBirthDeath(
		Data^gp,
		!Distribution,
		!Random,
		!Population,
		Births,
		CemeteryCarryingCapacity, CemeteryOldAge, CemeteryStarvation),
	% io.print('\r', !IO), io.print(IterationNumber, !IO), io.print(' ', !IO), io.print('d', !IO), io.flush_output(io.stdout_stream, !IO),
	DeathIDs = list.append(CemeteryCarryingCapacity, list.append(CemeteryOldAge, CemeteryStarvation)),
	BirthIDs = list.delete_elems(list.map(ebea.player.'ID', Births), DeathIDs),
	ebea.population.mapfold_PlayerNeighbour_sv(
		ebea.player.selection.stepProcessBornPlayersCheckForDeadPlayers(
			Data^game,
			DeathIDs,
			BirthIDs),
		!Population,
		!Random),
	list.foldl(
		initSelectionTraits(Data^game),
		BirthIDs,
		!Population),
	% io.print('\r', !IO), io.print(IterationNumber, !IO), io.print(' ', !IO), io.print('e', !IO), io.flush_output(io.stdout_stream, !IO),
	NextStats = stats(
		%NextStats^reduceEvolution =
		ebea.population.fold_players(
			ebea.player.foldChromosome,
			!.Population,
			ThisStats^reduceEvolution
		),
		%NextStats^deathsCarryingCapacity =
		ThisStats^deathsCarryingCapacity + list.length(CemeteryCarryingCapacity),
		%NextStats^deathsOldAge =
		ThisStats^deathsOldAge + list.length(CemeteryOldAge),
		%NextStats^deathsStarvation =
		ThisStats^deathsStarvation + list.length(CemeteryStarvation),
		%NextStats^births =
		ThisStats^births + list.length(Births)
	),
	% io.print('\r', !IO), io.print(IterationNumber, !IO), io.print(' ', !IO), io.print('f', !IO), io.flush_output(io.stdout_stream, !IO),
	ebea.core.printIterationDataToStreams(Data^s, Data^game, IterationNumber, !.Population, PlayerProfiles, Births, CemeteryCarryingCapacity, CemeteryOldAge, CemeteryStarvation, !IO)
	.

iterationData3(Data, IterationNumber, !Population, !Stats, !Distribution, !Random, !IO) :-
	% io.print(!.Population, !IO),
	% io.nl(!IO),
	Data^parameters3^siteDynamics = SiteDynamics,
	(
		SiteDynamics = static,
		iterationData2(data(Data^game3, Data^parameters3^base, Data^streams3), IterationNumber, !Population, !Stats, !Distribution, !Random, !IO)
	;
		SiteDynamics = dynamic(_),
		ebea.population.fold4_PlayerNeighbour(
			ebea.player.selection.stepSelectPartnersPlayGame3(
				Data^parameters3^base^playerParameters,
				Data^game3
			),
			!.Population,
			!Population,
			[], PlayerProfiles,
			!Random,
			util.arrayInitUnique(array.size(!.Population^sites), foldable.initAC), SiteActionAccumulator
		),
		ebea.population.map_players(ebea.player.age.stepClockTick, !Population),
		ebea.population.stepBirthDeath(
			Data^parameters3^base,
			!Distribution,
			!Random,
			!Population,
			Births,
			CemeteryCarryingCapacity, CemeteryOldAge, CemeteryStarvation),
		DeathIDs = list.append(CemeteryCarryingCapacity, list.append(CemeteryOldAge, CemeteryStarvation)),
		BirthIDs = list.delete_elems(list.map(ebea.player.'ID', Births), DeathIDs),
		ebea.population.mapfold_PlayerNeighbour_sv(
			ebea.player.selection.stepProcessBornPlayersCheckForDeadPlayers(
				Data^game3,
				DeathIDs,
				BirthIDs),
			!Population,
			!Random),
		list.foldl(
			initSelectionTraits(Data^game3),
			BirthIDs,
			!Population),
		ebea.population.stepUpdateSitesState(SiteDynamics, SiteActionAccumulator, !Population),
		!:Stats = stats(
			ebea.population.fold_players(
				ebea.player.foldChromosome,
				!.Population,
				!.Stats^reduceEvolution
			),
			!.Stats^deathsCarryingCapacity + list.length(CemeteryCarryingCapacity),
			!.Stats^deathsOldAge + list.length(CemeteryOldAge),
			!.Stats^deathsStarvation + list.length(CemeteryStarvation),
			!.Stats^births + list.length(Births)
		),
		ebea.core.printIterationDataToStreams(Data^streams3, Data^game3, IterationNumber, !.Population, PlayerProfiles, Births, CemeteryCarryingCapacity, CemeteryOldAge, CemeteryStarvation, !IO)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions


%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Initialise selection traits of newborn players

:- pred initSelectionTraits(
	G   :: in,
	ebea.population.players.key :: in,
	ebea.population.population(C, T) :: in,  ebea.population.population(C, T) :: out
) is det
<= abstractGame(G).

initSelectionTraits(Game, NewBornKey, !Population) :-
	UpdatePlayerFunc =
	(func(TheNewBorn) = Result :-
		ebea.population.neighbours.init(!.Population^sites, TheNewBorn) = Neighbours,
		ebea.player.selection.initTraits(Game, TheNewBorn, Neighbours) = Result
	),
	ebea.population.players.update(NewBornKey, UpdatePlayerFunc, !.Population^players, NewPlayers),
	!:Population = 'players :='(!.Population, NewPlayers)
	.
	

:- pred runLoopData2(
	runMode(C, T),
	ebea.core.data(G, P), int, int,
	population(C, T), population(C, T),
	stats(A), stats(A),
	distribution, distribution,
	R, R,
	io, io)
	<= (ePRNG(R), asymmetricGame(G, C), chromosome(C, T, P), foldable(C, A), parseable(C), printable(C), printable(T), printable(A)).
:- mode runLoopData2(in(runMode), in, in, in, in, out, in, out, in, out, in, out, di, uo) is det.

runLoopData2(Mode, Data, TimeLeft, Iteration, !Population, !Stats, !Distribution, !Random, !IO) :-
	NumberPlayers = game.numberPlayers(Data^game),
	iterationData2(Data, Iteration, !Population, !Stats, !Distribution, !Random, !IO),
	(
		Mode = background,
		Stop = no
		;
		Mode = interactively(_, _, _),
		(Mode^itera)(!.Population, Iteration, Stop, !IO)
	),
	(if
		ebea.population.size(!.Population) < NumberPlayers
		;
		TimeLeft = 0
		;
		Stop = yes
	then
		ebea.core.printLastIterationDataToStreams(!.Stats, Data^s, Iteration, !.Population, !IO)
	else
		runLoopData2(Mode, Data, TimeLeft - 1, Iteration + 1, !Population, !Stats, !Distribution, !Random, !IO)
	).

:- pred runLoopData3(
	runMode(CS, T)           :: in(runMode),
	ebea.core.data(G, P, AA) :: in,
	int                      :: in,
	int                      :: in,
	population(CS, T) :: in,  population(CS, T) :: out,
	stats(ACS)        :: in,  stats(ACS)        :: out,
	distribution      :: in,  distribution      :: out,
	R                 :: in,  R                 :: out,
	io.state          :: di,  io.state          :: uo
) is det
	<= (
	ePRNG(R),
	asymmetricGame(G, CS, A),
	chromosome(CS, T, P),
	foldable(CS, ACS),
	foldable(A, AA),
	parseable(CS),
	printable(CS),
	printable(T),
	printable(ACS)
).

runLoopData3(background, Data, TimeLeft, Iteration, !Population, !Stats, !Distribution, !Random, !IO) :-
	NumberPlayers = game.numberPlayers(Data^game3),
	iterationData3(Data, Iteration, !Population, !Stats, !Distribution, !Random, !IO),
	(if
		ebea.population.size(!.Population) < NumberPlayers
		;
		TimeLeft = 0
	then
		ebea.core.printLastIterationDataToStreams(!.Stats, Data^streams3, Iteration, !.Population, !IO)
	else
		runLoopData3(background, Data, TimeLeft - 1, Iteration + 1, !Population, !Stats, !Distribution, !Random, !IO)
	).

runLoopData3(Mode, Data, TimeLeft, Iteration, !Population, !Stats, !Distribution, !Random, !IO) :-
	Mode = interactively(_, _, _),
	NumberPlayers = game.numberPlayers(Data^game3),
	iterationData3(Data, Iteration, !Population, !Stats, !Distribution, !Random, !IO),
	(Mode^itera)(!.Population, Iteration, Stop, !IO),
	(if
		ebea.population.size(!.Population) < NumberPlayers
		;
		TimeLeft = 0
		;
		Stop = yes
	then
		ebea.core.printLastIterationDataToStreams(!.Stats, Data^streams3, Iteration, !.Population, !IO)
	else
		runLoopData3(Mode, Data, TimeLeft - 1, Iteration + 1, !Population, !Stats, !Distribution, !Random, !IO)
	).

/**
 * Prints information about the initial state of an EBEA run on the
 * selected output streams.

 * <p> Parameter {@code Streams} specifies which output streams and
 * therefore which data is written.  A value of {@code detailed/4} means
 * information about births, deaths, player phenotypes and games played are
 * printed.  With this value we write the most detailed account of what
 * happened in an iteration.  A value {@code dynamics/1} means we write
 * population size, deaths and a reduction of the current population.
 */

:- pred printInitialDataToStreams(
		ebea.streams.outStreams,
		ebea.population.population(C, T),
		io, io)
	<= (
		  parseable(C),
		  foldable(C, A),
		  printable(C),
		  printable(A)
	).
:- mode printInitialDataToStreams(in, in, di, uo) is det.

printInitialDataToStreams(Streams, Population, !IO) :-
	Streams = detailedTxt(_, _, _, _),
	ebea.population.fold_players(printBirth(Streams^tosBirth, -1), Population, !IO)
	;
	Streams = detailedBin(_, _, _, _, _),
	ebea.streams.siteState.write(Streams^bosSiteState, -1, Population, !IO),
	ebea.population.fold_players(ebea.streams.birth.foldInit, Population, []) = Births,
	ebea.streams.birth.writeInit(Streams^bosBirth, Births, !IO)
	;
	Streams = dynamics(_),
	io.print(Streams^sopopulation, "-1 ", !IO),
	io.print(Streams^sopopulation, ebea.population.size(Population), !IO),
	io.print(Streams^sopopulation, " 0 0 0 0 ", !IO),
	ebea.population.fold_players(ebea.player.foldChromosome, Population, ebea.player.initAc) = Reduce,
	printable.print(Streams^sopopulation, Reduce, !IO),
	io.nl(Streams^sopopulation, !IO)
	;
	Streams = summary(_)
	.
	
/**
 * Prints information about an EBEA iteration depending on the selected
 * output streams.  Parameter {@code Streams} specifies which output
 * streams and therefore which data is written.  A value of {@code
 * detailed/4} means information about births, deaths, player phenotypes
 * and games played are printed.  With this value we write the most
 * detailed account of what happened in an iteration.  A value {@code
 * dynamics/1} means we write population size, deaths and a reduction of
 * the current population.
 */

:- pred printIterationDataToStreams(
		ebea.streams.outStreams,
		G,
		int,
		ebea.population.population(C, T),
		list(list(key)),
		list(ebea.player.player(C, T)),
		list(key), list(key), list(key),
		io, io)
	<= (
		  asymmetricGame(G, C),
		  chromosome(C, T, P), foldable(C, A),
		  parseable(C),
		  printable(C), printable(T), printable(A)).
:- mode printIterationDataToStreams(in, in, in, in, in, in, in, in, in, di, uo) is det.

printIterationDataToStreams(Streams, Game, Iteration, Population, PlayerProfiles, Births,
	CemeteryCarryingCapacity, CemeteryOldAge, CemeteryStarvation,
	!IO)
:-
	Streams = detailedTxt(_, _, _, _),
	list.foldl(printBirth(Streams^tosBirth, Iteration), Births, !IO),
	list.foldl(printPlayerProfile(Streams^tosPlayerProfile, Iteration), PlayerProfiles, !IO),
	list.foldl(printDeathDataToStream(Streams^tosDeath, Iteration, carryingCapacity), CemeteryCarryingCapacity, !IO),
	list.foldl(printDeathDataToStream(Streams^tosDeath, Iteration, oldAge), CemeteryOldAge, !IO),
	list.foldl(printDeathDataToStream(Streams^tosDeath, Iteration, starvation), CemeteryStarvation, !IO),
	ebea.population.fold_players(printPhenotypeDataToStream(Streams^tosPhenotype, Iteration), Population, !IO)
	;
	Streams = detailedBin(_, _, _, _, _),
	ebea.streams.siteState.write(Streams^bosSiteState, Iteration, Population, !IO),
	ebea.streams.birth.write(Streams^bosBirth, Iteration, Births, !IO),
	ebea.streams.death.write(Streams^bosDeath, Iteration, CemeteryCarryingCapacity, CemeteryOldAge, CemeteryStarvation, !IO),
	ebea.streams.phenotype.write(Streams^bosPhenotype, Iteration, Population, !IO),
	ebea.streams.playerProfile.write(Streams^bosPlayerProfile, Game, Iteration, PlayerProfiles, !IO)
	;
	Streams = dynamics(_),
	io.print(Streams^sopopulation, Iteration, !IO),
	io.print(Streams^sopopulation, " ", !IO),
	io.print(Streams^sopopulation, ebea.population.size(Population), !IO),
	io.print(Streams^sopopulation, " ", !IO),
	io.print(Streams^sopopulation, list.length(Births) `with_type` int, !IO),
	io.print(Streams^sopopulation, " ", !IO),
	io.print(Streams^sopopulation, list.length(CemeteryCarryingCapacity) `with_type` int, !IO),
	io.print(Streams^sopopulation, " ", !IO),
	io.print(Streams^sopopulation, list.length(CemeteryOldAge) `with_type` int, !IO),
	io.print(Streams^sopopulation, " ", !IO),
	io.print(Streams^sopopulation, list.length(CemeteryStarvation) `with_type` int, !IO),
	io.print(Streams^sopopulation, " ", !IO),
	ebea.population.fold_players(ebea.player.foldChromosome, Population, ebea.player.initAc) = Reduce,
	printable.print(Streams^sopopulation, Reduce, !IO),
	io.nl(Streams^sopopulation, !IO)
	;
	Streams = summary(_)
	.


:- pred printLastIterationDataToStreams(
	stats(A),
	ebea.streams.outStreams,
	int,
	ebea.population.population(C, T),
	io, io)
<= (
	chromosome(C, T, P),
	foldable(C, A),
	printable(C),
	printable(T),
	printable(A)
).
:- mode printLastIterationDataToStreams(in, in, in, in, di, uo) is det.

printLastIterationDataToStreams(Stats, Streams, Iteration, Population, !IO)
:-
	Streams = detailedTxt(_, _, _, _)
	;
	Streams = detailedBin(_, _, _, _, _)
	;
	Streams = dynamics(_)
	;
	Streams = summary(_),
	ebea.population.fold_players(ebea.player.foldChromosome, Population, ebea.player.initAc) = Reduce,
	io.print(Streams^sosummary, Iteration, !IO),
	io.print(Streams^sosummary, " ", !IO),
	io.print(Streams^sosummary, ebea.population.size(Population), !IO),
	io.print(Streams^sosummary, " ", !IO),
	io.print(Streams^sosummary, ebea.population.lastID(Population), !IO),
	io.print(Streams^sosummary, " ", !IO),
	io.print(Streams^sosummary, Stats^deathsCarryingCapacity, !IO),
	io.print(Streams^sosummary, " ", !IO),
	io.print(Streams^sosummary, Stats^deathsOldAge, !IO),
	io.print(Streams^sosummary, " ", !IO),
	io.print(Streams^sosummary, Stats^deathsStarvation, !IO),
	io.print(Streams^sosummary, " ", !IO),
	printable.print(Streams^sosummary, Reduce, !IO),
	io.print(Streams^sosummary, " ", !IO),
	printable.print(Streams^sosummary, Stats^reduceEvolution, !IO),
	io.nl(Streams^sosummary, !IO)
	.

/**
 * After players played the game and the next iteration is calculated, we
 * write the results to streams.  The identification and chromosome of
 * players that were born, the identification of players that died, the
 * phenotype of players alive for the next iteration.
 */

:- pred printDataToStreams(
		G,
		maybe(stats(A)),
		ebea.streams.outStreams,
		int,
		ebea.population.population(C, T),
		list(list(key)),
		list(ebea.player.player(C, T)),
		list(key), list(key), list(key),
		io, io)
	<= (asymmetricGame(G, C),
			chromosome(C, T, P), foldable(C, A), parseable(C),
		  printable(C), printable(T), printable(A)).
:- mode printDataToStreams(in, in, in, in, in, in, in, in, in, in, di, uo) is det.

printDataToStreams(Game, Last, Streams, Iteration, Population, PlayerProfiles, Births,
	CemeteryCarryingCapacity, CemeteryOldAge, CemeteryStarvation,
	!IO)
:-
	Streams = detailedTxt(_, _, _, _),
	list.foldl(printBirth(Streams^tosBirth, Iteration), Births, !IO),
	list.foldl(printPlayerProfile(Streams^tosPlayerProfile, Iteration), PlayerProfiles, !IO),
	list.foldl(printDeathDataToStream(Streams^tosDeath, Iteration, carryingCapacity), CemeteryCarryingCapacity, !IO),
	list.foldl(printDeathDataToStream(Streams^tosDeath, Iteration, oldAge), CemeteryOldAge, !IO),
	list.foldl(printDeathDataToStream(Streams^tosDeath, Iteration, starvation), CemeteryStarvation, !IO),
	ebea.population.fold_players(printPhenotypeDataToStream(Streams^tosPhenotype, Iteration), Population, !IO)
	;
	Streams = detailedBin(_, _, _, _, _),
	ebea.streams.siteState.write(Streams^bosSiteState, Iteration, Population, !IO),
	ebea.streams.birth.write(Streams^bosBirth, Iteration, Births, !IO),
	ebea.streams.death.write(Streams^bosDeath, Iteration, CemeteryCarryingCapacity, CemeteryOldAge, CemeteryStarvation, !IO),
	ebea.streams.phenotype.write(Streams^bosPhenotype, Iteration, Population, !IO),
	ebea.streams.playerProfile.write(Streams^bosPlayerProfile, Game, Iteration, PlayerProfiles, !IO)
	;
	Streams = dynamics(_),
	io.print(Streams^sopopulation, Iteration, !IO),
	io.print(Streams^sopopulation, " ", !IO),
	io.print(Streams^sopopulation, ebea.population.size(Population), !IO),
	io.print(Streams^sopopulation, " ", !IO),
	io.print(Streams^sopopulation, list.length(Births) `with_type` int, !IO),
	io.print(Streams^sopopulation, " ", !IO),
	io.print(Streams^sopopulation, list.length(CemeteryCarryingCapacity) `with_type` int, !IO),
	io.print(Streams^sopopulation, " ", !IO),
	io.print(Streams^sopopulation, list.length(CemeteryOldAge) `with_type` int, !IO),
	io.print(Streams^sopopulation, " ", !IO),
	io.print(Streams^sopopulation, list.length(CemeteryStarvation) `with_type` int, !IO),
	io.print(Streams^sopopulation, " ", !IO),
	ebea.population.fold_players(ebea.player.foldChromosome, Population, ebea.player.initAc) = Reduce,
	printable.print(Streams^sopopulation, Reduce, !IO),
	io.nl(Streams^sopopulation, !IO)
	;
	Last = no,
	Streams = summary(_)
	;
	Last = yes(Stats),
	Streams = summary(_),
	ebea.population.fold_players(ebea.player.foldChromosome, Population, ebea.player.initAc) = Reduce,
	io.print(Streams^sosummary, Iteration, !IO),
	io.print(Streams^sosummary, " ", !IO),
	io.print(Streams^sosummary, ebea.population.size(Population), !IO),
	io.print(Streams^sosummary, " ", !IO),
	io.print(Streams^sosummary, ebea.population.lastID(Population), !IO),
	io.print(Streams^sosummary, " ", !IO),
	io.print(Streams^sosummary, Stats^deathsCarryingCapacity, !IO),
	io.print(Streams^sosummary, " ", !IO),
	io.print(Streams^sosummary, Stats^deathsOldAge, !IO),
	io.print(Streams^sosummary, " ", !IO),
	io.print(Streams^sosummary, Stats^deathsStarvation, !IO),
	io.print(Streams^sosummary, " ", !IO),
	printable.print(Streams^sosummary, Reduce, !IO),
	io.print(Streams^sosummary, " ", !IO),
	printable.print(Streams^sosummary, Stats^reduceEvolution, !IO),
	io.nl(Streams^sosummary, !IO)
	.

:- pred printPlayerProfile(io.output_stream, int, list(key), io.state, io.state).
:- mode printPlayerProfile(in, in, in, di, uo) is det.

printPlayerProfile(Stream, Iteration, PlayerProfile, !IO) :-
	io.print(Stream, Iteration, !IO),
	PredPrint =
	(pred(I::in, IOdi::di, IOuo::uo) is det :-
		io.print(Stream, ' ', IOdi, IO1),
		io.print(Stream, I, IO1, IOuo)
	),
	list.foldl(PredPrint, PlayerProfile, !IO),
	io.nl(Stream, !IO)
	.

:- pred printBirth(io.output_stream, int, player(C, T), io, io)
	<= printable(C).
:- mode printBirth(in, in, in, di, uo) is det.

printBirth(Stream, Iteration, Player, !IO) :-
	io.print(Stream, Iteration, !IO),
	io.print(Stream, " ", !IO),
	io.print(Stream, Player^id, !IO),
	io.print(Stream, " ", !IO),
	io.print(Stream, Player^siteIndex, !IO),
	io.print(Stream, " ", !IO),
	ebea.player.chromosome.print(Stream, Player^chromosome, !IO),
	io.nl(Stream, !IO)
	.

:- pred printDeathDataToStream(io.output_stream, int, ebea.population.death, key, io, io).
:- mode printDeathDataToStream(in, in, in, in, di, uo) is det.

printDeathDataToStream(Stream, Iteration, Death, ID, !IO) :-
	io.print(Stream, Iteration, !IO),
	io.print(Stream, " ", !IO),
	io.print(Stream, ID, !IO),
	io.print(Stream, ' ', !IO),
	io.print(Stream, ebea.population.deathChar(Death) `with_type` char, !IO),
	io.nl(Stream, !IO).

:- pred printPhenotypeDataToStream(io.output_stream, int, player(C, T), io, io)
	<= (chromosome(C, T, P)).
:- mode printPhenotypeDataToStream(in, in, in, di, uo) is det.

printPhenotypeDataToStream(Stream, Iteration, Player, !IO) :-
	io.print(Stream, Iteration, !IO),
	io.print(Stream, " ", !IO),
	io.print(Stream, Player^id, !IO),
	io.print(Stream, " ", !IO),
	ebea.player.printTraits(Stream, Player, !IO),
	io.nl(Stream, !IO)
	.

:- end_module ebea.core.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
