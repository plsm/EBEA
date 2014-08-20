/**
 * A site is a geographical location where players can be and where every
 * player can interact with every other player without restrictions.

 * @author Pedro Mariano
 * @version 1.0 2013/06/24
 */
:- module ebea.population.site.

:- interface.

:- include_module parameters.
:- import_module ebea.population.site.parameters.

%% ************************************************************************
%% Represents a site in an EBEA population.  A site has a state, the list
%% with current players, and an array with adjacent sites.  A site state
%% may change during an evolutionary run depending on the dynamics.  The
%% list of players contains players' keys.  This list changes from
%% iteration to iteration as players reproduce and die.  The array with
%% adjacent sites contains site indexes.  These refer to the site array in
%% type {@code population}.  This array does not change during an
%% evolutionary run.
%%
:- type site --->
	site(
		state             :: ebea.population.site.state,
		playerIDs         :: list(ebea.population.players.key),
		neighbourSiteIdxs :: array(int)
	).

%% ************************************************************************
%% The state of site which contains the data that can change during an
%% iteration depending on a value of type {@code dynamics(A)}.
%%
%% @cons state(CarryingCapacity)
%%
:- type state --->
	state(
		carryingCapacity  :: float
	).

%% ************************************************************************
%% Represents site dynamics.
%%
%% @cons static Site state does not change during an evolutionary run.
%%
%% @cons dynamic(F) Site state changes is given by function {@code F}.
%% Given a site, it returns its next state.
%%
%% @param AA The game actions accumulator.
%%
:- type dynamics(AA) --->
	static ;
	dynamic(func(AA, ebea.population.site.site) = ebea.population.site.state)
	.

:- type updateState(AA) == (func(AA, ebea.population.site.site) = ebea.population.site.state).

/**
 * Initialise the players in some site given the initial site state parameters.
 */
:- pred initialisePlayers(
	ebea.player.parameters(P) :: in,
	int                       :: in,
	initialPlayers(C)         :: in,
	ebea.population.players.key           :: in, ebea.population.players.key           :: out,
	list(ebea.population.players.key)     :: in, list(ebea.population.players.key)     :: out,
	ebea.population.players.players(C, T) :: in, ebea.population.players.players(C, T) :: out,
	R                                     :: in, R                                     :: out
)
	is det
	<= (chromosome(C, T, P), ePRNG(R))
.

/**
 * Create the initial site given its initial state parameters.  The
 * predicate receives the player parameters used to give birth, two maybe
 * parameters that are used to override the site index and neighbourhood.
 */

:- pred createInitialSite(
	ebea.player.parameters(P)                     :: in,
	ebea.population.site.parameters.parameters(C) :: in,
	maybe(int)                                    :: in,
	maybe(list(int))                              :: in,
	site :: out,
	ebea.population.players.key           :: in, ebea.population.players.key           :: out,
	ebea.population.players.players(C, T) :: in, ebea.population.players.players(C, T) :: out,
	R                                     :: in, R                                     :: out
)
	is det
	<= (chromosome(C, T, P), ePRNG(R))
.

/**
 * Create the initial site for a lattice population.
 */
:- pred createLatticeInitialSite(
	float                     :: in,
	geometry                  :: in(lattice),
	ebea.player.parameters(P) :: in,
	int :: in, site :: out,
	list(ebea.population.site.parameters.parameters(C)) :: in, list(ebea.population.site.parameters.parameters(C)) :: out,
	ebea.population.players.key                         :: in, ebea.population.players.key                         :: out,
	ebea.population.players.players(C, T)               :: in, ebea.population.players.players(C, T)               :: out,
	R                                                   :: in, R                                                   :: out
)
	is det
	<= (chromosome(C, T, P), ePRNG(R)).

/**
 * Return the number of players in the site.
 */
:- func numberPlayers(site) = int.


/**
 * stepBirth(Parameters, Player, Site, Parent, !Distribution, !Random, !NextID, !Nursery)
  
 * Apply the birth algorithm of the EBEA.  Players can reproduce when their
 * energy reaches the reproduction threshold.  They produce an offspring,
 * and their energy goes back to zero.  The predicate returns the offsprings.
 */

:- pred stepBirth(
	ebea.player.parameters(P),
	player(C, T),
	player(C, T),
	distribution, distribution,
	R, R,
	ebea.population.players.key, ebea.population.players.key,
	list(player(C, T)), list(player(C, T)))
	<= (ePRNG(R), chromosome(C, T, P)).
:- mode stepBirth(in, in, out, in, out, in, out, in, out, in, out) is det.



/**
 * death(Parameters, !Random, !Population, Deaths)
  
 * Apply the death algorithm to all players in the population.  The
 * probability that a player dies is given by a sigmoid function that
 * depends on the population size and a "carrying capacity" parameter.
 * Points of this sigmoid function are calculated by {@code
 * carryingCapacity/2}.
 */

:- pred stepDeath(
	ebea.population.parameters(P) :: in,
	population(C, T)              :: in,
	player(C, T) :: in,
	bool         :: out,
	R                  :: in, R                  :: out,
	list(player(C, T)) :: in, list(player(C, T)) :: out,
	list(player(C, T)) :: in, list(player(C, T)) :: out,
	list(player(C, T)) :: in, list(player(C, T)) :: out
)
	is det
	<= (ePRNG(R), chromosome(C, T, P))
.

% :- pred stepDeath(
% 	ebea.population.parameters(P),
% 	player(C, T), site,
% 	R, R,
% 	list(player(C, T)), list(player(C, T)),
% 	list(player(C, T)), list(player(C, T)),
% 	list(player(C, T)), list(player(C, T)),
% 	list(player(C, T)), list(player(C, T))
% 	)
% 	<= (ePRNG(R), chromosome(C, T, P)).
% :- mode stepDeath(in, in, in, in, out, in, out, in, out, in, out, in, out) is det.


% /**
%  * This function is used to put newborn players in their site.
%  */
% :- func placePlayerXX(player(C, T), array(site)) = array(site).
% :- mode placePlayerXX(in, array_di) = array_uo is det.

/**
 * This function is used to put newborn players in their site.  Players may
 * migrate to new sites depending on parameter {@code
 * migrationProbability}.
 */

:- pred placePlayers(float, list(player(C, T)), list(player(C, T)), array(site), array(site), R, R)
	<= ePRNG(R).
:- mode placePlayers(in, in, out, array_di, array_uo, in, out) is det.

/**
 * For every player that has died removes its reference in its site.  The
 * predicate also returns a list with players' keys.
  
 */
:- pred removePlayers(list(player(C, T)), list(key), list(key), array(site), array(site)).
:- mode removePlayers(in, in, out, array_di, array_uo) is det.

%% ************************************************************************
%% parseDynamics(ListUpdateState, Dynamics, !State)
%%
%% Parse site dynamics.
%%
:- pred parseDynamics(
	pred(int, updateState(AA)),
	dynamics(AA),
	parseable.state, parseable.state
).
:- mode parseDynamics(in(pred(out, in)  is det),     in,  out, in)  is det.
:- mode parseDynamics(in(pred(in,  out) is semidet), out, in,  out) is semidet.

:- func fold_player(population(C, T), site, func(player(C, T), A) = A, A) = A.


:- pred debug(io.state, io.state).
:- mode debug(di, uo) is det.

:- implementation.

:- import_module ebea.player.age.
:- import_module ebea.player.chromosome.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

initialisePlayers(PlayerParameters, SiteIndex, InitialPlayers, !KeyGenerator, !SitePlayerKeys, !PopulationPlayers, !Random) :-
	(if
		InitialPlayers^quantity >= 0
	then
		list.foldl4(
			makeCopy(PlayerParameters, SiteIndex, InitialPlayers^chromosome),
			1..InitialPlayers^quantity,
			!KeyGenerator,
			!SitePlayerKeys,
			!PopulationPlayers,
			!Random)
	else
		throw("ebea.population.site.parameters.initialisePlayers/11: Invalid number of chromosome copies")
	).

createInitialSite(PlayerParameters, SiteParameters, MSiteIndex, MSiteNeighbourhood, InitialSite, !KeyGenerator, !PopulationPlayers, !Random) :-
	(
		MSiteIndex = no,
		SiteIndex = SiteParameters^id
		;
		MSiteIndex = yes(SiteIndex)
	),
	list.foldl4(
		initialisePlayers(PlayerParameters, SiteIndex),
		SiteParameters^chromosomes,
		!KeyGenerator,
		[], SitePlayerKeys,
		!PopulationPlayers,
		!Random),
	(
		MSiteNeighbourhood = no,
		SiteNeighbourhood = SiteParameters^neighbourhood
		;
		MSiteNeighbourhood = yes(SiteNeighbourhood)
	),
	SiteState^carryingCapacity = float(SiteParameters^carryingCapacity),
	InitialSite^state = SiteState,
	InitialSite^playerIDs = SitePlayerKeys,
	InitialSite^neighbourSiteIdxs = array.from_list(SiteNeighbourhood)
	.

createLatticeInitialSite(
	DefaultCarryingCapacity, Geometry, PlayerParameters,
	SiteIndex, InitialSite,
	!ListSiteParameters,
	!KeyGenerator,
	!PopulationPlayers,
	!Random
) :-
	(if
		!.ListSiteParameters = [SiteParameters | LSP],
		SiteParameters^id = SiteIndex
	then
		!:ListSiteParameters = LSP,
		createInitialSite(
			PlayerParameters, SiteParameters, no, yes(makeConnections(Geometry, SiteIndex)),
			InitialSite,
			!KeyGenerator,
			!PopulationPlayers,
			!Random)
	else
		SiteState^carryingCapacity = DefaultCarryingCapacity,
		InitialSite^state = SiteState,
		InitialSite^playerIDs = [],
		InitialSite^neighbourSiteIdxs = array.from_list(makeConnections(Geometry, SiteIndex))
	).

	
numberPlayers(Site) = list.length(Site^playerIDs).

% neighbours(ArraySites, Players, Player, Neighbours) :-
% 	array.member(ArraySites, Site),
% 	%
% 	list.delete(Site^playerIDs, PlayerID, RestIDs),
% 	Player = ebea.population.getPlayer(Players, PlayerID),
% 	SiteNeighbours = list.map(ebea.population.getPlayer(Players), RestIDs),
% 	%
% 	array.foldl(AppendPlayersSite, Site^neighbourSiteIdxs, SiteNeighbours) = Neighbours,
% 	AppendPlayersSite =
% 	(func(I, AC) = R :-
% 		S = array.lookup(ArraySites, I),
% 		R = list.append(list.map(ebea.population.getPlayer(Players), S^playerIDs), AC)
% 	).


% neighbours(ArraySites, Players, Player) = Neighbours :-
% 	array.lookup(ArraySites, Player^siteIndex) = Site,
% 	%
% 	list.delete_all(Site^playerIDs, Player^id) = RestIDs,
% 	SiteNeighbours = list.map(ebea.population.getPlayer(Players), RestIDs),
% 	%
% 	array.foldl(AppendPlayersSite, Site^neighbourSiteIdxs, SiteNeighbours) = Neighbours,
% 	AppendPlayersSite =
% 	(func(I, AC) = R :-
% 		S = array.lookup(ArraySites, I),
% 		R = list.append(list.map(ebea.population.getPlayer(Players), S^playerIDs), AC)
% 	).

stepBirth(Parameters, Player, Parent, !Distribution, !Random, !NextID, !Nursery) :-
	ebea.player.reproduce(Parameters, Player, Parent, !.NextID, MOffspring, !Distribution, !Random),
	(
		MOffspring = no
		;
		MOffspring = yes(Offspring),
		ebea.population.players.nextKey(!NextID),
		!:Nursery = [Offspring | !.Nursery]
	)
.

stepDeath(Parameters, Population, Player, Filter, !Random,
	!CemeteryCarryingCapacity, !CemeteryOldAge, !CemeteryStarvation)
:-
	Site = playerSite(Population, Player),
	deathProbability(Site^state^carryingCapacity, numberPlayers(Site)) = Value,
	rng.flipCoin(Value, DiesCC, !Random),
	(if
		DiesCC = yes
	then
		Dies = yes(carryingCapacity)
	else
		ebea.player.energy.stepSurvive(Parameters^playerParameters^energyPar, Player, !Random, DiesS),
		(if
			DiesS = yes
		then
			Dies = yes(starvation)
		else
			ebea.player.age.stepSurvive(Parameters^playerParameters^agePar, Player, !Random, DiesOA),
			(
				DiesOA = yes,
				Dies = yes(oldAge)
				;
				DiesOA = no,
				Dies = no
			)
		)
	),
	(	%
		Dies = no,
		Filter = yes
		;
		Dies = yes(Cause),
		Filter = no,
		(	%
			Cause = carryingCapacity,
			list.cons(Player, !CemeteryCarryingCapacity)
		;
			Cause = oldAge,
			list.cons(Player, !CemeteryOldAge)
		;
			Cause = starvation,
			list.cons(Player, !CemeteryStarvation)
		)
	).

placePlayers(MigrationProbability, ListPlayers, MappedPlayers, !ArraySites, !Random) :-
	ListPlayers = [PlayerToPlace | RestListPlayers],
	array.lookup(!.ArraySites, PlayerToPlace^siteIndex) = BornSite,
	(if
		% if the player born in a site with neighbours flip a coin
		NumOptions = array.size(BornSite^neighbourSiteIdxs),
		NumOptions > 0
	then
		rng.flipCoin(MigrationProbability, Move, !Random),
		(
			Move = yes,
			rng.nextInt(0, NumOptions - 1, Index, !Random),
			PlacedSite = array.lookup(!.ArraySites, Index),
			PlacedPlayer = 'siteIndex :='(PlayerToPlace, Index)
			;
			Move = no,
			PlacedSite = BornSite,
			PlacedPlayer = PlayerToPlace
		)
	else
		PlacedPlayer = PlayerToPlace,
		PlacedSite = BornSite
	),
	NewSite = 'playerIDs :='(PlacedSite, [PlacedPlayer^id | PlacedSite^playerIDs]),
	MappedPlayers = [PlacedPlayer | RestMappedPlayers],
	!:ArraySites = array.set(!.ArraySites, PlacedPlayer^siteIndex, NewSite),
	placePlayers(MigrationProbability, RestListPlayers, RestMappedPlayers, !ArraySites, !Random)
	;
	ListPlayers = [],
	MappedPlayers = [].

removePlayers(ListPlayers, !ListPlayersID, !ArraySites) :-
	ListPlayers = [Player | Rest],
	array.lookup(!.ArraySites, Player^siteIndex) = Site,
	(if
		list.delete_first(Site^playerIDs, Player^id, NPI)
	then
		NewPlayerIDs = NPI
	else
		throw("ebea.population.site.removePlayer/5: player is not in site")
	),
	NewSite = 'playerIDs :='(Site, NewPlayerIDs),
	!:ArraySites = array.set(!.ArraySites, Player^siteIndex, NewSite),
	list.cons(Player^id, !ListPlayersID), 
	removePlayers(Rest, !ListPlayersID, !ArraySites)
	;
	ListPlayers = [].


fold_player(Population, Site, Func, AC) = Result :-
	list.foldl(BridgeFunc, Site^playerIDs, AC) = Result,
	BridgeFunc =
	(func(I, A) = R :-
		ebea.population.players.player(Population^players, I) = P,
		R = Func(P, A)
	).

debug(!IO) :-
	io.read_line_as_string(ILine, !IO),
	(if
		ILine = ok(Line),
		string.words(Line) = [SX, SY, SI],
		string.to_int(SX, X),
		string.to_int(SY, Y),
		string.to_int(SI, I)
	then
		PredLattice =
		(pred(G::out) is nondet :-
			list.member(G^boundary, [torus, ring, closed]),
			list.member(G^neighbourhood, [moore, hexagonal, vonNeumann]),
			G^xSize = X,
			G^ySize = Y
		),
		Gss = solutions.solutions(PredLattice),
		io.print(Gss, !IO),
		io.nl(!IO),
		PredTest =
		(pred(G::in, IOdi::di, IOuo::uo) is det :-
			io.format("%20s => %30s\n", [s(string(G)), s(string(makeConnections(G, I)))], IOdi, IOuo)
		),
		list.foldl(PredTest, Gss, !IO),
		debug(!IO)
	else
		true
	).

parseDynamics(MapUpdateState, Dynamics) -->
	{Dynamics = static},
	[0]
	;
	{Dynamics = dynamic(UF)},
	[1, Code],
	{MapUpdateState(Code, UF)}
	.

/*
:- pred parseDynamicsUpdateFunction(
	list(updateState(AA)),
	int,
	updateState(AA),
	parseable.state, parseable.state
).
:- mode parseDynamicsUpdateFunction(in, in, in, out, in) is det.
:- mode parseDynamicsUpdateFunction(in, in, out, in, out) is semidet.

parseDynamicsUpdateFunction(
	ListUpdateState :: in,
	Index           :: in,
	Result          :: out,
	!.State :: in,  !:State :: out
) :-
	!.State = [Code | !:State],
	ListUpdateState = [AnUpdateState | RestUpdateState],
	(if
		Code = Index
	then
		AnUpdateState = Result
	else
		parseDynamicsUpdateFunction(RestUpdateState, Index + 1, Result, !State)
	).

parseDynamicsUpdateFunction(
	ListUpdateState :: in,
	Index           :: in,
	Result          :: in,
	!.State :: out,  !:State :: in
) :-
	ListUpdateState = [AnUpdateState | RestUpdateState],
	(if
		AnUpdateState = Result
	then
		!.State = [Index | !:State]
	else
		parseDynamicsUpdateFunction(RestUpdateState, Index + 1, Result, !State)
	).
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- pred makeCopy(
	ebea.player.parameters(P)            :: in,
	int                                  :: in,
	ebea.player.chromosome.chromosome(C) :: in,
	int                                  :: in,
	ebea.population.players.key           :: in, ebea.population.players.key           :: out,
	list(ebea.population.players.key)     :: in, list(ebea.population.players.key)     :: out,
	ebea.population.players.players(C, T) :: in, ebea.population.players.players(C, T) :: out,
	R                                     :: in, R                                     :: out
)
	is det
	<= (chromosome(C, T, P), ePRNG(R))
.

makeCopy(PlayerParameters, SiteIndex, Chromosome, _, !KeyGenerator, !SitePlayerKeys, !PopulationPlayers, !Random) :-
	ebea.player.init(PlayerParameters, Chromosome, !.KeyGenerator, SiteIndex, NewPlayer, !Random),
	list.cons(!.KeyGenerator, !SitePlayerKeys),
	ebea.population.players.insert(NewPlayer, !KeyGenerator, !PopulationPlayers)
	.

% :- pred ixy(geometry, int, int, int).
% :- mode ixy(in, in, out, out) is det.
% :- mode ixy(in, out, in, in) is det.

:- func ix(geometry, int) = int.

ix(wellmixed, I) = I.
ix(lattice(XL, _, _, _), I) = I mod XL.

:- func iy(geometry, int) = int.

iy(wellmixed, I) = I.
iy(lattice(XL, _, _, _), I) = I / XL.

:- func xyi(geometry, int, int) = int.

xyi(wellmixed, _, _) = throw("Not applicable").
xyi(lattice(XL, _, _, _), X, Y) = X + Y * XL.

:- func makeConnections(geometry, int) = list(int).
makeConnections(Geometry, SiteIndex) = solutions.solutions(neighbour(Geometry, ix(Geometry, SiteIndex), iy(Geometry, SiteIndex))).

:- pred neighbour(geometry, int, int, int).
:- mode neighbour(in, in, in, out) is nondet.

% up
neighbour(lattice(XL, YL, _N, B),   X, Y,    X + ((Y + 1) mod YL) * XL) :-
	B = torus
	;
	B = closed,
	Y < YL - 1
	;
	B = ring,
	Y < YL - 1
	.

% down
neighbour(lattice(XL, YL, _N, B),   X, Y,    X + ((Y - 1) mod YL) * XL) :-
	B = torus
	;
	B = closed,
	Y > 0
	;
	B = ring,
	Y > 0
	.

% right
neighbour(lattice(XL, _YL, _N, B),   X, Y,    (X + 1) mod XL + Y * XL) :-
	B = torus
	;
	B = closed,
	X < XL - 1
	;
	B = ring
	.

% left
neighbour(lattice(XL, _YL, _N, B),   X, Y,    (X - 1) mod XL + Y * XL) :-
	B = torus
	;
	B = closed,
	X > 0
	;
	B = ring
	.


% left up
neighbour(lattice(XL, YL, N, B),   X, Y,    (X - 1) mod XL + ((Y + 1) mod YL) * XL) :-
	(
		N = vonNeumann
	),
	(
		B = torus
		;
		B = closed,
		X > 0,
		Y < YL - 1
		;
		B = ring,
		Y < YL - 1
	)
	.


% left down
neighbour(lattice(XL, YL, N, B),   X, Y,    (X - 1) mod XL  +  ((Y - 1) mod YL) * XL) :-
	(
		N = hexagonal
		;
		N = vonNeumann
	),
	(
		B = torus
		;
		B = closed,
		X > 0,
		Y > 0
		;
		B = ring,
		Y > 0
	)
	.

% right up
neighbour(lattice(XL, YL, N, B),   X, Y,    (X + 1) mod XL + ((Y + 1) mod YL) * XL) :-
	(
		N = hexagonal
		;
		N = vonNeumann
	),
	(
		B = torus
		;
		B = closed,
		X < XL - 1,
		Y < YL - 1
		;
		B = ring,
		Y < YL - 1
	)
	.

% right down
neighbour(lattice(XL, YL, N, B),   X, Y,    (X + 1) mod XL + ((Y - 1) mod YL) * XL) :-
	(
		N = hexagonal
		;
		N = vonNeumann
	),
	(
		B = torus
		;
		B = closed,
		X < XL - 1,
		Y > 0
		;
		B = ring,
		Y > 0
	)
	.



:- end_module ebea.population.site.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
