/**
 * The population in an Energy Based Evolutionary Algorithm is a collection
 * of phenotypes subject to a carrying capacity and life expectancy.  The
 * population determines the potential partners of a player.  This module
 * is responsible for calculating the population in the next round of the
 * EBEA.

 * @author Pedro Mariano
 * @version 1.0 2012/07/03
 */
:- module ebea.population.

:- interface.

:- include_module neighbours, configuration, players, site.

:- import_module ebea.population.neighbours, ebea.population.configuration,
ebea.population.players, ebea.population.site.
:- import_module chromosome, ebea.player, rng, rng.distribution, parseable.
:- import_module array, char, io, list, maybe.

/**
 * The players in a population are stored in a list.  An
 * integer is used to generate the next player's identification.  The
 * population structure is represented by an array of sites.  Each site
 * contains a list of players' identification and an array of integers
 * representing neighbour sites indexes.

 * <p> When players select partners to play game we use the population
 * structure to retrieve tuples with player and his potential partners.
 * After playing the game we update the list of players.

 * <p> In the birth and death step we process the players to obtain a list
 * of newborn players and a list of players' identification corresponding
 * to the dead ones.  The list of newborn players and the list with
 * players' identification are used to update the sites.

 * <p> When we need to process the players we fold the list of players.

 * <p> When we need to process player neighbours tuples we fold the array
 * of sites using the list of players to map ids to players.

 * <p> 
  
 */
:- type population(C, P) --->
	pop(
		sites   :: array(site),
		players :: ebea.population.players.players(C, P),
		nextID  :: ebea.population.players.key
	).

%% ************************************************************************
%% Parameters that govern population dynamics.  These include the behaviour
%% of players meaning ageing, reproduction and partner selection.
%%
%% @param P Parameters the govern the game played by players
%%
:- type parameters(P) --->
	p(
	  migrationProbability :: float,
	  dynamic              :: dynamic,
	  playerParameters     :: ebea.player.parameters(P)
	).

%% ************************************************************************
%% Parameters that govern population dynamics.  These include the behaviour
%% of players meaning ageing, reproduction and partner selection.
%%
%% @param P Parameters the govern the game played by players
%%
%% @param A The action accumulator.
%%
:- type parameters(P, A) --->
	p(
		base         :: ebea.population.parameters(P),
		siteDynamics :: ebea.population.site.dynamics(A)
	).


/**
 * Represents the round dynamic.  How the next population is calculated
 * using the two processes: death and birth.

 * @cons birthPlusDeath The population of the current round is subject
 * simultaneous to the death and birth process.  The next population is the
 * sum of the survivals plus the new offspring.

 * @cons birthThenDeath The population of the current round reproduces.
 * Everybody including the new offspring is subject to the death process.

 * @cons deathThenBirth The population of the current round is subject to
 * the death process.  The survivals are then subject to the new birth
 * process.  The population of the next round is thus the survivals plus
 * new offspring.
  
 */
:- type dynamic --->
	birthPlusDeath ;
	birthThenDeath ;
	deathThenBirth.

:- type death --->
	carryingCapacity ;
	oldAge ;
	starvation.

:- inst playerDeath == bound(oldAge ; starvation).


:- pred createInitialPopulation(ebea.player.parameters(P), ebea.population.configuration.configuration(C, A), population(C, T), R, R)
	<= (chromosome(C, T, P), ePRNG(R)).
:- mode createInitialPopulation(in, in, out, in, out) is det.


/**
 * Return the number of players in the population.
 */
:- func size(population(C, P)) = int.

/**
 * Return the player's identification last used.
 */
:- func lastID(population(C, P)) = ebea.population.players.key.


%:- func players(population(C, P)) = list(player(C, P)).



/**
 * stepBirthDeath(Parameters, !Random, !Population, Nursery, Cemetery)
 
 * Calculate the population for the next round.

 * <p> Players that have accumulated enough energy can reproduce.  They
 * produce one offspring subject to mutation.  The population is also
 * subject to a carrying capacity process: every player in the population
 * is subject to a death random event.  Death probability depends on
 * population size.

 * @param Nursery List with the players that born in the current round.

 * @param Cemetery List with the identification of players that died in the
 * current round.  This information is written in a stream.
 */

:- pred stepBirthDeath(
	ebea.population.parameters(P),
	distribution, distribution,
	R, R,
	population(C, T), population(C, T),
	list(player(C, T)),
	list(key), list(key), list(key))
	<= (ePRNG(R), chromosome(C, T, P)).
:- mode stepBirthDeath(in, in, out, in, out, in, out, out, out, out, out) is det.

%% ************************************************************************
%% stepUpdateSitesState(SiteDynamics, SiteActionAccumulator, !Population)
%%
%% For each site update its site using the given game actions accumulator
%% and closure.
%%
%% @param AA The game actions accumulator.
%%
:- pred stepUpdateSitesState(
	ebea.population.site.dynamics(AA) :: in,
	array(AA)                         :: in,
	population(C, T) :: in,  population(C, T) :: out
) is det.

/**
 * Return the player with the given identification.  Throws an exception if
 * the player does not exist.

 * TODO: migrate to new population type
 */

% :- func player(int, population(C, P)) = player(C, P).

/**
 * update(ID, Player, Population) = Result

 * Update the player with identification {@code ID} in the population.  If
 * the player does not exist, return the population unchanged.
  
 * TODO: migrate to new population type
 */
:- func update(key, func(player(C, T)) = player(C, T), population(C, T)) = population(C, T).

/**
 * update(ID, Player, !Population)

 * Update the player with identification {@code ID} in the population.  If
 * the player does not exist, return the population unchanged.
  
 * TODO: migrate to new population type
 */
:- pred update(key, func(player(C, T)) = player(C, T), population(C, T), population(C, T)).
:- mode update(in, in, in, out) is det.

/**
 * fold(Closure, Population, AC) = Result
 *
 * Apply the given closure to all players in the population and reduce them
 * to {@code Result}.

*/
:- func fold_players(func(player(C, P), A) = A, population(C, P), A) = A.

:- pred fold_players(pred(player(C, P), A, A), population(C, P), A, A).
:- mode fold_players(in(pred(in, in, out) is det), in, in, out) is det.
:- mode fold_players(in(pred(in, di, uo) is det), in, di, uo) is det.

:- pred fold_sites(func(player(C, P), A) = A, population(C, P), A, array(A)).
:- mode fold_sites(in, in, in, out) is det.





/**
 * Applies the given closure to every players in the population in order to
 * update their state.
  
 */
:- pred map_players(func(player(C, P)) = player(C, P), population(C, P), population(C, P)).
:- mode map_players(in, in, out) is det.

/**
 * Apply the given closure and transform all the players in the population.
 */
:- func transform_player(func(player(C, P)) = T, population(C, P)) = list(T).

% /**
%  * fold2_Player(Pred, Population, !Accumulator1, !Accumulator2)

%  * Calls {@code Pred} for every player in the population with the two
%  * accumulators.
%  */

% :- pred fold2_Player(pred(player(C, P), A, A, B, B), population(C, P), A, A, B, B).
% :- mode fold2_Player(in(pred(in, in, out, in, out) is det), in, in, out, in, out) is det.
% :- mode fold2_Player(in(pred(in, in, out, di, uo) is det), in, in, out, di, uo) is det.

% /**
%  * fold2_PlayerNeighbour(Pred, Population, !Accumulator1, !Accumulator2)
  
%  * Iterate through all pairs where a pair is a player and his neighbours
%  * calling {@code Pred} with the two accumulators.
  
%  */
% :- pred fold2_PlayerNeighbour(pred(player(C, T), list(player(C, T)), A, A, B, B), population(C, T), A, A, B, B).
% :- mode fold2_PlayerNeighbour(in(pred(in, in, in, out, in, out) is det), in, in, out, in, out) is det.

/**
 * fold3_PlayerNeighbour(Pred, Population, !Accumulator1, !Accumulator2, !Accumulator3)
  
 * Iterate through all pairs where a pair is a player and his neighbours
 * calling {@code Pred} with the two accumulators.
  
 */
:- pred fold3_PlayerNeighbour(pred(player(C, T), neighbours, A1, A1, A2, A2, A3, A3), population(C, T), A1, A1, A2, A2, A3, A3).
:- mode fold3_PlayerNeighbour(in(pred(in, in, in, out, in, out, in, out) is det), in, in, out, in, out, in, out) is det.

:- pred fold4_PlayerNeighbour(pred(player(C, T), neighbours, A1, A1, A2, A2, A3, A3, A4, A4), population(C, T), A1, A1, A2, A2, A3, A3, A4, A4).
:- mode fold4_PlayerNeighbour(in(pred(in, in, in, out, in, out, in, out, di, uo) is det), in, in, out, in, out, in, out, di, uo) is det.


/**
 * mapfold_PlayerNeighbour(Pred, !Population, !Accumulator1)
  
 * Use {@code Pred} to map players in the population using their neighbours
 * and the accumulator.
  
 */
:- pred mapfold_PlayerNeighbour(pred(player(C, T), ebea.population.neighbours.neighbours, player(C, T), A, A), population(C, T), population(C, T), A, A).
:- mode mapfold_PlayerNeighbour(in(pred(in, in, out, in, out) is det), in, out, in, out) is det.

/**
 * Convert a population dynamic value to a string a vice-versa.  The string
 * is one that must be present in a text stream.
 */
:- pred stringDynamic(string, dynamic).
:- mode stringDynamic(in, out) is semidet.
:- mode stringDynamic(out, in) is det.
:- mode stringDynamic(out, out) is multi.

/**
 * Convert a death instance to a character and vice-versa.
 */
:- pred deathChar(death, char).
:- mode deathChar(in, out) is det.
:- mode deathChar(out, in) is semidet.

:- func deathChar(death) = char.

% /**
%  * readParameters(Stream, MParameters, !IO)
  
%  * Read the parameters that govern the population dynamics from the given
%  * text stream.  If there is some IO error, value {@code no} is returned
%  * and a message is printed in the standard error stream.
%  */

% :- pred readParameters(io.input_stream, ebea.player.parameters(P), maybe(ebea.population.parameters(P)), io, io).
% :- mode readParameters(in, in, out, di, uo) is det.

/**
 * printCarryingCapacityFunction(Stream, Parameters, MaxX, !IO)
  
 * Print the function used to compute the probability of a player dying due
 * to the carrying capacity.  This is a sigmoid function that depends on
 * population parameters.  In the horizontal axis is population size while
 * in the vertical axis is the probability.
 */

%:- pred printCarryingCapacityFunction(io.output_stream, ebea.population.parameters(P), int, io, io).
:- pred printCarryingCapacityFunction(io.output_stream, float, int, io, io).
:- mode printCarryingCapacityFunction(in, in, in, di, uo) is det.	

:- pred debug(population(C, P), io.state, io.state).
:- mode debug(in, di, uo) is det.

:- pred parseDynamic(dynamic, list(int), list(int)).
:- mode parseDynamic(in, out, in) is det.
:- mode parseDynamic(out, in, out) is semidet.

:- pred parseDeath(death, list(int), list(int)).
:- mode parseDeath(in, out, in) is det.
:- mode parseDeath(out, in, out) is semidet.

:- implementation.


:- import_module ebea.population.players, ebea.population.site, ebea.population.site.parameters.
:- import_module chromosome, ebea.player.energy, ebea.player.selection.
:- import_module array, bitmap, bool, exception, float, int, math, solutions, string.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

createInitialPopulation(PlayerParameters, Parameters, Population, !Random) :-
	Parameters^geometry = Geometry,
	(
	Geometry = wellmixed,
	(
		Parameters^sites = [],
		throw("ebea.population.createInitialPopulation/5: no sites")
		;
		Parameters^sites = [Site | _],
		ebea.population.players.init(EmptyPlayers, InitialKey),
		list.foldl4(
			ebea.population.site.initialisePlayers(PlayerParameters, 0),
			Site^chromosomes,
			InitialKey, NextKey,
			[], SitePlayerKeys,
			EmptyPlayers, PopulationPlayers,
			!Random),
		SiteState^carryingCapacity = float(Site^carryingCapacity),
		SingleSite = site(SiteState, SitePlayerKeys, array.init(0, -1)),
		Population = pop(array.init(1, SingleSite), PopulationPlayers, NextKey)
	)
	;
	Geometry = lattice(_, _, _, _),
	SiteIndexes = 0..(Geometry^xSize * Geometry^ySize - 1),
	ebea.population.players.init(EmptyPlayers, InitialKey),
	list.map_foldl4(
		ebea.population.site.createLatticeInitialSite(
			float(Parameters^defaultCarryingCapacity),
			Geometry,
			PlayerParameters),
		SiteIndexes,      InitialSites,
		Parameters^sites, _,
		InitialKey,       NextKey,
		EmptyPlayers,     PopulationPlayers,
		!Random),
	Population = pop(array.from_list(InitialSites), PopulationPlayers, NextKey)
	).




size(Population) = ebea.population.players.size(Population^players).

lastID(Population) = Population^nextID.

stepBirthDeath(
	Parameters,
	!Distribution,
	!Random,
	Population, pop(NextSites, NextPlayers, NextID),
	Nursery,
	CemeteryIDsCarryingCapacity, CemeteryIDsOldAge, CemeteryIDsStarvation)
:-
	Parameters^dynamic = birthPlusDeath,
	ebea.population.players.mapFold4(
		ebea.population.site.stepBirth(Parameters^playerParameters),
		Population^players, MappedPlayers,
		!Distribution,
		!Random,
		Population^nextID, NextID,
		[], Nursery),
	ebea.population.players.filterFold4(
		ebea.population.site.stepDeath(Parameters, Population),
		MappedPlayers, SurvivingPlayers,
		!Random,
		[], CemeteryCarryingCapacity,
		[], CemeteryOldAge,
		[], CemeteryStarvation),
%	list.foldl(placePlayer, Nursery, Population^sites) = NextSites,
	ebea.population.site.placePlayers(Parameters^migrationProbability, Nursery, Newborn, Population^sites, Sites0, !Random),
	ebea.population.site.removePlayers(CemeteryCarryingCapacity, [], CemeteryIDsCarryingCapacity, Sites0, Sites1),
	ebea.population.site.removePlayers(CemeteryOldAge, [], CemeteryIDsOldAge, Sites1, Sites2),
	ebea.population.site.removePlayers(CemeteryStarvation, [], CemeteryIDsStarvation, Sites2, NextSites),
	NextPlayers = ebea.population.players.append(SurvivingPlayers, Newborn)
	;
	Parameters^dynamic = birthThenDeath,
	ebea.population.players.mapFold4(
		ebea.population.site.stepBirth(Parameters^playerParameters),
		Population^players, MappedPlayers,
		!Distribution,
		!Random,
		Population^nextID, NextID,
		[], Nursery),
	ebea.population.site.placePlayers(Parameters^migrationProbability, Nursery, Newborn, Population^sites, Sites0, !Random),
	NewPlayers = ebea.population.players.append(MappedPlayers, Newborn),
	ebea.population.players.filterFold4(
		ebea.population.site.stepDeath(Parameters, pop(Sites0, NewPlayers, NextID)),
		NewPlayers, SurvivingPlayers,
		!Random,
		[], CemeteryCarryingCapacity,
		[], CemeteryOldAge,
		[], CemeteryStarvation),
	ebea.population.site.removePlayers(CemeteryCarryingCapacity, [], CemeteryIDsCarryingCapacity, Sites0, Sites1),
	ebea.population.site.removePlayers(CemeteryOldAge, [], CemeteryIDsOldAge, Sites1, Sites2),
	ebea.population.site.removePlayers(CemeteryStarvation, [], CemeteryIDsStarvation, Sites2, NextSites),
	NextPlayers = SurvivingPlayers
	;
	Parameters^dynamic = deathThenBirth,
	ebea.population.players.filterFold4(
		ebea.population.site.stepDeath(Parameters, Population),
		Population^players, SurvivingPlayers,
		!Random,
		[], CemeteryCarryingCapacity,
		[], CemeteryOldAge,
		[], CemeteryStarvation),
	ebea.population.players.mapFold4(
		ebea.population.site.stepBirth(Parameters^playerParameters),
		SurvivingPlayers, MappedPlayers,
		!Distribution,
		!Random,
		Population^nextID, NextID,
		[], Nursery),
	ebea.population.site.placePlayers(Parameters^migrationProbability, Nursery, Newborn, Population^sites, Sites0, !Random),
	ebea.population.site.removePlayers(CemeteryCarryingCapacity, [], CemeteryIDsCarryingCapacity, Sites0, Sites1),
	ebea.population.site.removePlayers(CemeteryOldAge, [], CemeteryIDsOldAge, Sites1, Sites2),
	ebea.population.site.removePlayers(CemeteryStarvation, [], CemeteryIDsStarvation, Sites2, NextSites),
	NextPlayers = ebea.population.players.append(MappedPlayers, Newborn)
	.

stepUpdateSitesState(SiteDynamics, SiteActionAccumulator, !Population) :-
	SiteDynamics = static
	;
	SiteDynamics = dynamic(UpdateFunc),
	UpdateSite =
	(pred(OldSite::in, NewSite::out, Index::in, NextIndex::out) is det :-
		NextIndex = Index + 1,
		array.lookup(SiteActionAccumulator, Index) = AA,
		NewSite = 'state :='(OldSite, UpdateFunc(AA, OldSite))
	),
	array.map_foldl(UpdateSite, !.Population^sites, NewSites, 0, _),
	!:Population = 'sites :='(!.Population, NewSites)
	.

update(ID, PlayerFunc, Population) = Result :-
	Result = 'players :='(Population, NextPlayers),
	ebea.population.players.update(ID, PlayerFunc, Population^players, NextPlayers).

update(ID, PlayerFunc, Population, Result) :-
	update(ID, PlayerFunc, Population) = Result.

fold_players(Func, Population, AC) = Result :-
	ebea.population.players.fold(Func, Population^players, AC) = Result.

fold_players(Pred, Population, !AC) :-
	ebea.population.players.fold(Pred, Population^players, !AC).

fold_sites(Func, Population, AC, array.from_list(Result)) :-
	array.to_list(Population^sites) = LSites,
	list.map(Apply, LSites) = Result,
	Apply =
	(func(S) = R :-
		ebea.population.site.fold_player(Population, S, Func, AC) = R
	)
%	array.init(array.size(Population^sites), AC) = Result
	.

map_players(Closure, !Population) :-
	ebea.population.players.map(Closure, !.Population^players) = NextPlayers,
	!:Population = 'players :='(!.Population, NextPlayers).

transform_player(Closure, Population) = Result :-
	ebea.population.players.transform(Closure, Population^players) = Result.

% fold2_PlayerNeighbour(Pred, Population, !Accumulator1, !Accumulator2) :-
% 	promise_equivalent_solutions [!:Accumulator1, !:Accumulator2]
% 		solutions.unsorted_aggregate2(ebea.population.neighbours(Population), Fold2_PlayerNeighbour, !Accumulator1, !Accumulator2),
% 	Fold2_PlayerNeighbour =
% 	(pred({Player, Neighbours}::in, Acc1::in, NextAcc1::out, Acc2::in, NextAcc2::out) is det :-
% 		Pred(Player, Neighbours, Acc1, NextAcc1, Acc2, NextAcc2)
% 	).

fold3_PlayerNeighbour(Pred, Population, !Accumulator1, !Accumulator2, !Accumulator3) :-
	ebea.population.players.fold3(fold3_real_PlayerNeighbour(Pred, Population), Population^players, !Accumulator1, !Accumulator2, !Accumulator3).

fold4_PlayerNeighbour(Pred, Population, !Accumulator1, !Accumulator2, !Accumulator3, !Accumulator4) :-
	ebea.population.players.fold4(fold4_real_PlayerNeighbour(Pred, Population), Population^players, !Accumulator1, !Accumulator2, !Accumulator3, !Accumulator4).

mapfold_PlayerNeighbour(Pred, PopulationIn, PopulationOut, !Accumulator) :-
	ebea.population.players.mapFold(mapfold_real_PlayerNeighbour(Pred, PopulationIn), PopulationIn^players, MappedPlayers, !Accumulator),
	PopulationOut = 'players :='(PopulationIn, MappedPlayers).

stringDynamic("birth+death", birthPlusDeath).
stringDynamic("birth>death", birthThenDeath).
stringDynamic("death>birth", deathThenBirth).

deathChar(carryingCapacity, c).
deathChar(oldAge, a).
deathChar(starvation, s).

deathChar(Death) = Char :-
	deathChar(Death, Char).

% readParameters(Stream, PlayerParameters, MParameters, !IO) :-
% 	io.read_line_as_string(Stream, RLine, !IO),
% 	(if
% 		RLine = ok(Line)
% 	then
% 		(if
% 			string.words(Line) = [SCarryingCapacity, SDynamic | Rest],
% 			util.comment(Rest)
% 		then
% 			(if
% 				string.to_float(SCarryingCapacity, CarryingCapacity),
% 				CarryingCapacity >= 0.0,
% 				stringDynamic(SDynamic, Dynamic)
% 			then
% 				Parameters^carryingCapacity = CarryingCapacity,
% 				Parameters^dynamic = Dynamic,
% 				Parameters^pla = PlayerParameters,
% 				MParameters = yes(Parameters)
% 			else
% 				(if
% 					not stringDynamic(SDynamic, _)
% 				then
% 					io.format(io.stderr_stream, "Invalid population dynamics: %s.\n Expecting one of:", [s(SDynamic)], !IO),
% 					High =
% 					(pred(D::out) is multi :-
% 						stringDynamic(D, _)
% 					),
% 					solutions.aggregate(High, util.spacePrint(io.stderr_stream), !IO),
% 					io.nl(io.stderr_stream, !IO)
% 				else
% 					true
% 				),
% 				(if
% 					string.to_float(SCarryingCapacity, CarryingCapacity),
% 					CarryingCapacity < 0.0
% 				then
% 					io.print(io.stderr_stream, "Carrying capacity must be a positive value\n", !IO)
% 				else
% 					true
% 				),
% 				(if
% 					not string.to_float(SCarryingCapacity, _)
% 				then
% 					io.print(io.stderr_stream, "Invalid carrying capacity value.\n", !IO)
% 				else
% 					true
% 				),
% 				MParameters = no
% 			)
% 		else
% 			io.print(io.stderr_stream, "Invalid number of parameters for population: required carrying capacity and dynamics\n", !IO),
% 			MParameters = no
% 		)
% 	else
% 		io.print(io.stderr_stream, "Error reading population parameters from stream.\n", !IO),
% 		MParameters = no
% 	).

printCarryingCapacityFunction(Stream, CarryingCapacity, MaxPopSize, !IO) :-
	PrintPointPred =
	(pred(PopSize::in, IOdi::di, IOuo::uo) is det :-
		io.print(Stream, PopSize, IOdi, IO1),
		io.print(Stream, " ", IO1, IO2),
		io.print(Stream, deathProbability(CarryingCapacity, PopSize), IO2, IO3),
		io.nl(Stream, IO3, IOuo)
	),
	(if
		MaxPopSize =< 0
	then
		int.fold_up(PrintPointPred, 0, float.round_to_int(2.0 * CarryingCapacity), !IO)
	else
		int.fold_up(PrintPointPred, 0, MaxPopSize, !IO)
	).

parseDynamic(birthPlusDeath) --> [0].
parseDynamic(birthThenDeath) --> [1].
parseDynamic(deathThenBirth) --> [2].

parseDeath(carryingCapacity) --> [0].
parseDeath(oldAge)           --> [1].
parseDeath(starvation)       --> [2].

debug(_Population, !IO) :-
	true.
	% Population = pop(Sites, _Players, _NextID),
	% PredPrintSites =
	% (pred(Site::in, Idxin::in, Idxout::out, IOdi::di, IOuo::uo) is det :-
	% 	io.format("Site #%d\n  This site has a carrying capacity of %f\n  Players in this site:%s\n  Neighbouring sites:%s\n",
	% 		[i(Idxin), f(Site^carryingCapacity), s(PlayersSite), s(NeighbouringSites)], IOdi, IOuo),
	% 	list.foldl(FuncJoinInt, Site^playerIDs, "") = PlayersSite,
	% 	array.foldl(FuncJoinInt, Site^neighbourSiteIdxs, "") = NeighbouringSites,
	% 	Idxout = Idxin + 1
	% ),
	% array.foldl2(PredPrintSites, Sites, 0, _, !IO),
	% PredPrintPlayerNeighbours =
	% (pred(PlayerNeighbours::in, IOdi::di, IOuo::uo) is det :-
	% 	PlayerNeighbours = {Player, Neighbours},
	% 	list.map(ebea.player.'ID', Neighbours) = ListIDs,
	% 	io.format("Player %s\n has neighbours%s\n", [s(string(Player)), s(list.foldl(FuncJoinInt, ListIDs, ""))], IOdi, IOuo)
	% ),
	% solutions.aggregate(neighbours(Population), PredPrintPlayerNeighbours, !IO),
	% io.print("\n END DEBUG\n", !IO),
	% true,
	% FuncJoinInt =
	% (func(Int, Str) = Result :-
	% 	Result = string.format(" %d%s", [i(Int), s(Str)])
	% ).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- func playerSite(population(C, T), player(C, T)) = site.

playerSite(Population, Player) = array.lookup(Population^sites, Player^siteIndex).

% /**
%  * Update the players in the given population using the player and its site.
%  */
% :- pred mapfold4_PlayerSite(
% 	pred(player(C, T), site, player(C, T), T1, T1, T2, T2, T3, T3, T4, T4),
% 	population(C, T), list(player(C, T)),
% 	T1, T1, T2, T2, T3, T3, T4, T4).
% :- mode mapfold4_PlayerSite(in(pred(in, in, out, in, out, in, out, in, out, in, out) is det), in, out, in, out, in, out, in, out, in, out) is det.

% mapfold4_PlayerSite(Pred, Population, MappedPlayers, !Accumulator1, !Accumulator2, !Accumulator3, !Accumulator4) :-
% 	list.map_foldl4(MapFold, Population^players, MappedPlayers, !Accumulator1, !Accumulator2, !Accumulator3, !Accumulator4),
% 	MapFold =
% 	(pred(Pla::in, Alp::out, Acc1::in, Cca1::out, Acc2::in, Cca2::out, Acc3::in, Cca3::out, Acc4::in, Cca4::out) is det :-
% 		Pred(Pla, playerSite(Population, Pla), Alp, Acc1, Cca1, Acc2, Cca2, Acc3, Cca3, Acc4, Cca4)
% 	).

% :- pred fold5_PlayerSite(
% 	pred(player(C, T), site, T1, T1, T2, T2, T3, T3, T4, T4, T5, T5),
% 	population(C, T),
% 	T1, T1,
% 	T2, T2,
% 	T3, T3,
% 	T4, T4,
% 	T5, T5).
% :- mode fold5_PlayerSite(in(pred(in, in, in, out, in, out, in, out, in, out, in, out) is det), in, in, out, in, out, in, out, in, out, in, out) is det.

% fold5_PlayerSite(Pred, Population, !Accumulator1, !Accumulator2, !Accumulator3, !Accumulator4, !Accumulator5) :-
% 	list.foldl5(Fold, Population^players, !Accumulator1, !Accumulator2, !Accumulator3, !Accumulator4, !Accumulator5),
% 	Fold =
% 	(pred(P::in, Acc1::in, Cca1::out, Acc2::in, Cca2::out, Acc3::in, Cca3::out, Acc4::in, Cca4::out, Acc5::in, Cca5::out) is det :-
% 		Pred(P, playerSite(Population, P), Acc1, Cca1, Acc2, Cca2, Acc3, Cca3, Acc4, Cca4, Acc5, Cca5)
% 	).

% /**
%  * For each player in the population return a list with the neighbours.
%  * These neighbours are the potential game partners or if reproduction is
%  * sexual they can be used in a genetic crossover operator.

%  * TODO move to module ebea.population.neighbours
%  */
% :- pred neighbours(population(C, P), player(C, P), list(player(C, P))).
% :- mode neighbours(in, out, out) is nondet.

% neighbours(pop(Structure, Players, _), Player, Neighbours) :-
% 	ebea.population.site.neighbours(Structure, Players, Player, Neighbours).

% /**
%  * neighbours(Population, PlayerNeighbours)
  
%  * For each player in the population return a list with the neighbours.
%  * This predicate is similar to {@code neighbours/3} but is amenable to
%  * predicates from module {@code solutions}.

%  * TODO move to module ebea.population.neighbours
%  */
% :- pred neighbours(population(C, P), {player(C, P), list(player(C, P))}).
% :- mode neighbours(in, out) is nondet.

% neighbours(Population, {Player, Neighbours}) :-
% 	neighbours(Population, Player, Neighbours).




/**
 * fold3_real_PlayerNeighbour(Pred, Population, Player, !Accumulator1, !Accumulator2, !Accumulator3)
  
 * Iterate through all pairs where a pair is a player and his neighbours
 * calling {@code Pred} with the three accumulators.
  
 */
:- pred fold3_real_PlayerNeighbour(
	pred(player(C, T), neighbours, A1, A1, A2, A2, A3, A3),
	population(C, T),
	player(C, T),
	A1, A1,
	A2, A2,
	A3, A3).
:- mode fold3_real_PlayerNeighbour(
	in(pred(in, in, in, out, in, out, in, out) is det),
	in, in,
	in, out,
	in, out,
	in, out) is det.

fold3_real_PlayerNeighbour(Pred, Population, Player, !Accumulator1, !Accumulator2, !Accumulator3) :-
	ebea.population.neighbours.init(Population^sites, Player) = Neighbours,
	Pred(Player, Neighbours, !Accumulator1, !Accumulator2, !Accumulator3).

%% ************************************************************************
%% fold4_real_PlayerNeighbour(Pred, Population, Player, !Accumulator1, !Accumulator2, !Accumulator3, !Accumulator4)
%%
%% Iterate through all pairs where a pair is a player and his neighbours
%% calling {@code Pred} with the four accumulators.
%%
:- pred fold4_real_PlayerNeighbour(
	pred(player(C, T), neighbours, A1, A1, A2, A2, A3, A3, A4, A4)
		:: in(pred(in, in, in, out, in, out, in, out, di, uo) is det),
	population(C, T) :: in,
	player(C, T)     :: in,
	A1 :: in,  A1 :: out,
	A2 :: in,  A2 :: out,
	A3 :: in,  A3 :: out,
	A4 :: di,  A4 :: uo
) is det.

fold4_real_PlayerNeighbour(Pred, Population, Player, !Accumulator1, !Accumulator2, !Accumulator3, !Accumulator4) :-
	ebea.population.neighbours.init(Population^sites, Player) = Neighbours,
	Pred(Player, Neighbours, !Accumulator1, !Accumulator2, !Accumulator3, !Accumulator4).



/**
 * mapfold_real_PlayerNeighbour(Pred, !Population, !Accumulator1)
  
 * Use {@code Pred} to map players in the population using their neighbours
 * and the accumulator.
  
 */
:- pred mapfold_real_PlayerNeighbour(pred(player(C, T), ebea.population.neighbours.neighbours, player(C, T), A, A), population(C, T), player(C, T), player(C, T), A, A).
:- mode mapfold_real_PlayerNeighbour(in(pred(in, in, out, in, out) is det), in, in, out, in, out) is det.

mapfold_real_PlayerNeighbour(Pred, Population, Player, MappedPlayer, !Accumulator) :-
%	ebea.population.site.neighbours(Population^sites, Population^players, Player) = Neighbours,
	ebea.population.neighbours.init(Population^sites, Player) = Neighbours,
	Pred(Player, Neighbours, MappedPlayer, !Accumulator).
	



/*
:- pred foldSite(pred(site(C, P), A, A), site(C, P), A, A).
:- mode foldSite(in(pred(in, in, out) is det), in, in, out) is det.
:- mode foldSite(in(pred(in, di, uo) is det), in, di, uo) is det.

foldSite(Pred, Site, !AC) :-
	list.foldl(Pred, Site^sitePlayers, !AC).
*/

% /**
%  * death(Parameters, !Random, !Population, Deaths)
  
%  * Apply the death algorithm to all players in the population.  The
%  * probability that a player dies is given by a sigmoid function that
%  * depends on the population size and a "carrying capacity" parameter.
%  * Points of this sigmoid function are calculated by {@code
%  * carryingCapacity/2}.
%  */

% :- pred death(
% 	ebea.population.parameters(P),
% 	R, R,
% 	population(C, T), population(C, T),
% 	list(int), list(int), list(int))
% 	<= (ePRNG(R), chromosome(C, T, P)).
% :- mode death(in, in, out, in, out, out, out, out) is det.

% death(
% 	Parameters,
% 	!Random,
% 	wellMixed(PopulationIn, ID), wellMixed(PopulationOut, ID),
% 	CemeteryCarryingCapacity, CemeteryOldAge, CemeteryStarvation)
% :-
% 	list.foldl5(
% 		survives(Parameters, list.length(PopulationIn)),
% 		PopulationIn,
% 		[], PopulationOut,
% 		!Random,
% 		[], CemeteryCarryingCapacity,
% 		[], CemeteryOldAge,
% 		[], CemeteryStarvation).

% death(
% 	Parameters,
% 	!Random,
% 	network(SitesIn, ID), network(array.from_list(SitesOut), ID),
% 	CemeteryCarryingCapacity, CemeteryOldAge, CemeteryStarvation)
% :-
% 	list.map_foldl4(
% 		deathSite(Parameters),
% 		array.to_list(SitesIn), SitesOut,
% 		!Random,
% 		[], CemeteryCarryingCapacity,
% 		[], CemeteryOldAge,
% 		[], CemeteryStarvation).

% deathSite(Parameters, !Site, !Random, !CemeteryCarryingCapacity, !CemeteryOldAge, !CemeteryStarvation) :-
% 	list.foldl5(
% 		survives(Parameters, list.length(Site^sitePlayers)),
% 		!.Site^sitePlayers,
% 		[], !:Site^sitePlayers,
% 		!Random,
% 		!CemeteryCarryingCapacity,
% 		!CemeteryOldAge,
% 		!CemeteryStarvation).

% /**
%  * survives(Parameters, PopulationSize, Player, MPlayer, !Random, !Cemetery)

%  * Calculate if a player can survive in a population with the given
%  * conditions.  Parameter {@code MPlayer} is unified with {@code yes(P)} if
%  * it survives, or with {@code no} otherwise.

%  * <p> The probability that a player dies is given by a sigmoid function
%  * that depends on the population size and a "carrying capacity" parameter.
%  * Points of this sigmoid function are calculated by {@code
%  * carryingCapacity/2}.
%  */

% :- pred survives(ebea.population.parameters(P), int, player(C, T),
% 		list(player(C, T)), list(player(C, T)),
% 		R, R,
% 		list(int), list(int),
% 		list(int), list(int),
% 		list(int), list(int))
% 	<= ePRNG(R).
% :- mode survives(in, in, in, in, out, in, out, in, out, in, out, in, out) is det.

% survives(Parameters, PopulationSize, Player, !Population, !Random,
% 	!CemeteryCarryingCapacity, !CemeteryOldAge, !CemeteryStarvation)
% :-
% 	deathProbability(Parameters^carryingCapacity, PopulationSize) = Value,
% 	rng.flipCoin(Value, DiesCC, !Random),
% 	(if
% 		DiesCC = yes
% 	then
% 		Dies = yes(carryingCapacity)
% 	else
% 		ebea.energy.survives(Parameters^pla^energyPar, Player, !Random, Dies)
% 	),
% 	(
% 		Dies = no,
% 		list.cons(Player, !Population)
% 		;
% 		Dies = yes(carryingCapacity),
% 		list.cons(Player^id, !CemeteryCarryingCapacity)
% 		;
% 		Dies = yes(oldAge),
% 		list.cons(Player^id, !CemeteryOldAge)
% 		;
% 		Dies = yes(starvation),
% 		list.cons(Player^id, !CemeteryStarvation)
% 	).

% /**
%  * Apply the birth algorithm of the EBEA.  Players can reproduce when their
%  * energy reaches the reproduction threshold.  They produce an offspring,
%  * and their energy goes back to zero.  The predicate returns the number of
%  * births.
%  */

% :- pred birth(ebea.population.parameters(P), distribution, distribution, R, R, population(C, T), population(C, T), population(C, T), int)
% 	<= (ePRNG(R), chromosome(C, T, P)).
% :- mode birth(in, in, out, in, out, in, out, out, out) is det.

% birth(Parameters, !Distribution, !Random, PopulationIn, PopulationOut, Nursery, Births) :-
% 	list.map_foldl4(
% 		reproduces(Parameters^pla),
% 		PopulationIn^pop, PopulationOut^pop,
% 		[], Nursery^pop,
% 		PopulationIn^id, NextID,
% 		!Distribution, !Random),
% 	Nursery^id = NextID,
% 	PopulationOut^id = NextID,
% 	Births = list.length(Nursery^pop).

% :- pred reproduces(ebea.player.parameters(P), player(C, T), player(C, T), list(player(C, T)), list(player(C, T)), int, int, distribution, distribution, R, R)
% 	<= (ePRNG(R), chromosome(C, T, P)).
% :- mode reproduces(in, in, out, in, out, in, out, in, out, in, out) is det.

% reproduces(Parameters, Player, MapPlayer, Nursery, NextNursery, ID, NextID, !Distribution, !Random) :-
% 	player.reproduce(Parameters, Player, MapPlayer, ID + 1, MOffspring, !Distribution, !Random),
% 	(
% 		MOffspring = no,
% 		NextID = ID,
% 		NextNursery = Nursery
% 		;
% 		MOffspring = yes(Offspring),
% 		NextID = ID + 1,
% 		NextNursery = [Offspring | Nursery]
% 	)
% 	.
% 	% NextPhenotype1 = 'age :='(Player^p, Player^p^age + 1),
% 	% (if
% 	% 	ebea.player.canReproduce(Parameters^pla, Player, NextEnergy)
% 	% then
% 	% 	NextPhenotype2 = 'energy :='(NextPhenotype1, NextEnergy),
% 	% 	NextPlayer = 'p :='(Player, NextPhenotype2),
% 	% 	NextID = ID + 1,
% 	% 	MapPlayer = NextPlayer,
% 	% 	ebea.player.reproduce(Parameters^pla, Player, NextID, Offspring, !Distribution, !Random),
% 	% 	NextNursery = [Offspring | Nursery]
% 	% else
% 	% 	NextPlayer = 'p :='(Player, NextPhenotype1),
% 	% 	NextID = ID,
% 	% 	MapPlayer = NextPlayer,
% 	% 	NextNursery = Nursery
% 	% ).


/**
 * Given the parameters of the population dynamic and the population size
 * calculate the probability that a player dies.
 */

:- func deathProbability(float, int) = float.

:- pragma memo(deathProbability/2).

deathProbability(CarryingCapacity, PopulationSize) = Result :-
	Base = 6.0 * (CarryingCapacity - float(PopulationSize)) / CarryingCapacity,
	Result = 1.0 / (1.0 + math.exp(Base)).



% /**
%  * playerPop(ID, Population) = Result.
  
%  * Auxiliary function that returns the player in list {@code Population}
%  * with identification {@code ID}.  Throws an exception if there is no such
%  * player.
%  */

% :- func playerPop(int, list(player(C, P))) = player(C, P).

% playerPop(ID, Population) = Result :-
% 	Population = [Head | Tail],
% 	(if
% 		Head^id = ID
% 	then
% 		Result = Head
% 	else
% 		Result = playerPop(ID, Tail)
% 	)
% 	;
% 	Population = [],
% 	throw("ebea.population.playerPop/2: population does not have player id").

% :- func getPlayer(list(player(C, P)), int) = player(C, P).

% getPlayer(Population, ID) = Result :-
% 	Population = [Head | Tail],
% 	(if
% 		Head^id = ID
% 	then
% 		Result = Head
% 	else
% 		Result = getPlayer(Tail, ID)
% 	)
% 	;
% 	Population = [],
% 	throw(string.format("ebea.population.getPlayer/2: population does not have player id %d", [i(ID)]) `with_type` string).

% /**
%  * updatePop(ID, Player, Population) = Result
  
%  * Auxiliary function that updates the player in list {@code Population}
%  * with identification {@code ID}.  The new value of the player is {@code
%  * Player}.
  
%  */
% :- func updatePop(int, player(C, P), list(player(C, P))) = list(player(C, P)).

% updatePop(ID, Player, Population) = Result :-
% 	Population = [],
% 	Result = []
% 	;
% 	Population = [Head | Rest],
% 	(if
% 		Head^id = ID
% 	then
% 		Result = [Player | Rest]
% 	else
% 		Result = [Head | updatePop(ID, Player, Rest)]
% 	).

% :- func join(list(player(C, T)), list(player(C, T))) = list(player(C, T))
% 	<= chromosome(C, T, P).

% join([], A) = A.
% join([P | R], L) = [P | join(R, L)].


:- end_module ebea.population.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
