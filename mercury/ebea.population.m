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
:- import_module
	chromosome,
	game,
	ebea.player,
	rng,
	rng.distribution,
	parseable.
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
%% @param MU The action accumulator.
%%
:- type parameters(P, MU) --->
	p(
		base         :: ebea.population.parameters(P),
		siteDynamics :: ebea.population.site.dynamics(MU)
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


:- pred createInitialPopulation(
	G                                                 :: in,
	ebea.player.parameters(P)                         :: in,
	ebea.population.configuration.configuration(C, _) :: in,
	population(C, T) :: out,
	R :: in,  R :: out
) is det
	<= (
	abstractGame(G),
	chromosome(C, T, P),
	ePRNG(R)
).


/**
 * Return the number of players in the population.
 */
:- func size(population(C, P)) = int.

/**
 * Return the player's identification last used.
 */
:- func lastID(population(C, P)) = ebea.population.players.key.

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

/**
 * fold3_PlayerNeighbour(Pred, Population, !Accumulator1, !Accumulator2, !Accumulator3)
  
 * Iterate through all pairs where a pair is a player and his neighbours
 * calling {@code Pred} with the three accumulators.
  
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

:- pred mapfold_PlayerNeighbour_sv(pred(ebea.population.neighbours.neighbours, player(C, T), player(C, T), A, A), population(C, T), population(C, T), A, A).
:- mode mapfold_PlayerNeighbour_sv(in(pred(in, in, out, in, out) is det), in, out, in, out) is det.

:- func map_PlayerNeighbour(
	func(player(C, T), ebea.population.neighbours.neighbours) = player(C, T),
	population(C, T)
	) = population(C, T).

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

/**
 * printCarryingCapacityFunction(Stream, Parameters, MaxX, !IO)
  
 * Print the function used to compute the probability of a player dying due
 * to the carrying capacity.  This is a sigmoid function that depends on
 * population parameters.  In the horizontal axis is population size while
 * in the vertical axis is the probability.
 */

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

createInitialPopulation(Game, PlayerParameters, Configuration, Population, !Random) :-
	Configuration^geometry = Geometry,
	(	%
		Geometry = wellmixed,
		(
			Configuration^sites = [],
			throw("ebea.population.createInitialPopulation/5: no sites")
			;
			Configuration^sites = [Site | _],
			ebea.population.players.init(EmptyPlayers, InitialKey),
			list.foldl4(
				ebea.population.site.initialisePlayers(PlayerParameters, 0),
				Site^chromosomes,
				InitialKey, NextKey,
				[], SitePlayerKeys,
				EmptyPlayers, PopulationPlayers,
				!Random),
			SiteState^carryingCapacity = float(Site^carryingCapacity),
			SingleSite = site(SiteState, SiteState, SitePlayerKeys, array.init(0, -1)),
			Population0 = pop(array.init(1, SingleSite), PopulationPlayers, NextKey)
		)
	;
		Geometry = lattice(_, _, _, _),
		SiteIndexes = 0..(Geometry^xSize * Geometry^ySize - 1),
		ebea.population.players.init(EmptyPlayers, InitialKey),
		list.map_foldl4(
			ebea.population.site.createLatticeInitialSite(
				float(Configuration^defaultCarryingCapacity),
				Geometry,
				PlayerParameters),
			SiteIndexes,      InitialSites,
			list.sort(Configuration^sites),  _,
			InitialKey,                      NextKey,
			EmptyPlayers,                    PopulationPlayers,
			!Random),
		Population0 = pop(array.from_list(InitialSites), PopulationPlayers, NextKey)
	),
	Population = map_PlayerNeighbour(ebea.player.selection.initTraits(Game), Population0)
	.




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
		NewSite = 'currentState :='(OldSite, UpdateFunc(AA, OldSite))
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
	.

map_players(Closure, !Population) :-
	ebea.population.players.map(Closure, !.Population^players) = NextPlayers,
	!:Population = 'players :='(!.Population, NextPlayers).

transform_player(Closure, Population) = Result :-
	ebea.population.players.transform(Closure, Population^players) = Result.

fold3_PlayerNeighbour(Pred, Population, !Accumulator1, !Accumulator2, !Accumulator3) :-
	ebea.population.players.fold3(fold3_real_PlayerNeighbour(Pred, Population), Population^players, !Accumulator1, !Accumulator2, !Accumulator3).

fold4_PlayerNeighbour(Pred, Population, !Accumulator1, !Accumulator2, !Accumulator3, !Accumulator4) :-
	ebea.population.players.fold4(fold4_real_PlayerNeighbour(Pred, Population), Population^players, !Accumulator1, !Accumulator2, !Accumulator3, !Accumulator4).

mapfold_PlayerNeighbour(Pred, PopulationIn, PopulationOut, !Accumulator) :-
	ebea.population.players.mapFold(mapfold_real_PlayerNeighbour(Pred, PopulationIn), PopulationIn^players, MappedPlayers, !Accumulator),
	PopulationOut = 'players :='(PopulationIn, MappedPlayers).

mapfold_PlayerNeighbour_sv(Pred, PopulationIn, PopulationOut, !Accumulator) :-
	ebea.population.players.mapFold(mapfold_real_PlayerNeighbour_sv(Pred, PopulationIn), PopulationIn^players, MappedPlayers, !Accumulator),
	PopulationOut = 'players :='(PopulationIn, MappedPlayers).


map_PlayerNeighbour(MapPlayerNeighbourFunc, Population) = Result :-
	MapPlayerFunc =
	(func(OldPlayer) = NewPlayer :-
		ebea.population.neighbours.init(Population^sites, OldPlayer) = Neighbours,
		NewPlayer = MapPlayerNeighbourFunc(OldPlayer, Neighbours)
	),
	Result = 'players :='(Population, ebea.population.players.map(MapPlayerFunc, Population^players))
	.

stringDynamic("birth+death", birthPlusDeath).
stringDynamic("birth>death", birthThenDeath).
stringDynamic("death>birth", deathThenBirth).

deathChar(carryingCapacity, c).
deathChar(oldAge, a).
deathChar(starvation, s).

deathChar(Death) = Char :-
	deathChar(Death, Char).

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
	ebea.population.neighbours.init(Population^sites, Player) = Neighbours,
	Pred(Player, Neighbours, MappedPlayer, !Accumulator).
	
/**
 * mapfold_real_PlayerNeighbour_sv(Pred, Population, !Player, !Accumulator1)
  
 * Use {@code Pred} to map a player in the population using its neighbours
 * and the accumulator.
  
 */
:- pred mapfold_real_PlayerNeighbour_sv(
	pred(ebea.population.neighbours.neighbours, player(C, T), player(C, T), A, A)
		:: in(pred(in, in, out, in, out) is det),
	population(C, T) :: in,
	player(C, T) :: in,  player(C, T) :: out,
	A            :: in,  A            :: out
) is det.

mapfold_real_PlayerNeighbour_sv(Pred, Population, !Player, !Accumulator) :-
	ebea.population.neighbours.init(Population^sites, !.Player) = Neighbours,
	Pred(Neighbours, !Player, !Accumulator).

/**
 * Given the parameters of the population dynamic and the population size
 * calculate the probability that a player dies.
 */

:- func deathProbability(float, int) = float.

:- pragma memo(deathProbability/2).

deathProbability(CarryingCapacity, PopulationSize) = Result :-
	Base = 6.0 * (CarryingCapacity - float(PopulationSize)) / CarryingCapacity,
	Result = 1.0 / (1.0 + math.exp(Base)).

:- end_module ebea.population.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
