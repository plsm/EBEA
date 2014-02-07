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

:- type site --->
	site(
		carryingCapacity  :: float,
		playerIDs         :: list(int),
		neighbourSiteIdxs :: array(int)
	).

/**
 * Initialise the players in some site given the initial site state parameters.
 */
:- pred initialisePlayers(
	ebea.player.parameters(P), int,
	initialPlayers(CS),
	list(player(CS, T)), list(player(CS, T)),
	list(player(CS, T)), list(player(CS, T)),
	int, int,
	R, R)
	<= (chromosome(CS, T, P), ePRNG(R)).
:- mode initialisePlayers(in, in, in, in, out, in, out, in, out, in, out) is det.

/**
 * Create the initial site given its initial state parameters.  The
 * predicate receives the player parameters used to give birth, two maybe
 * parameters that are used to override the site index and neighbourhood.
 */

:- pred createInitialSite(
	ebea.player.parameters(P), ebea.population.site.parameters.parameters(C), maybe(int), maybe(list(int)),
	site,
	list(player(C, T)), list(player(C, T)),
	int, int,
	R, R)
	<= (chromosome(C, T, P), ePRNG(R)).
:- mode createInitialSite(in, in, in, in,  out,  in, out, in, out, in, out) is det.


/**
 * Create the initial site for a lattice population.
 */
:- pred createLatticeInitialSite(
	float, geometry, ebea.player.parameters(P),
	int, site,
	list(ebea.population.site.parameters.parameters(C)), list(ebea.population.site.parameters.parameters(C)),
	list(player(C, T)), list(player(C, T)),
	int, int,
	R, R)
	<= (chromosome(C, T, P), ePRNG(R)).
:- mode createLatticeInitialSite(
	in, in(lattice), in,
	in, out,
	in, out, in, out, in, out, in, out) is det.




/**
 * Return the number of players in the site.
 */
:- func numberPlayers(site) = int.

/**
 * neighbours(ArraySites, RestPlayers, Player, Neighbours)
  
 * For each player in the population return a list with the neighbours.
 * This predicate is similar to {@code neighbours/3} but is amenable to
 * predicates from module {@code solutions}.
 */
:- pred neighbours(array(site), list(player(C, P)), player(C, P), list(player(C, P))).
:- mode neighbours(in, in, out, out) is nondet.
%:- mode neighbours(in, in, in, out) is det.

:- func neighbours(array(site), list(player(C, P)), player(C, P)) = list(player(C, P)).


/**
 * stepBirth(Parameters, Player, Site, Parent, !Distribution, !Random, !NextID, !Nursery)
  
 * Apply the birth algorithm of the EBEA.  Players can reproduce when their
 * energy reaches the reproduction threshold.  They produce an offspring,
 * and their energy goes back to zero.  The predicate returns the offsprings.
 */

:- pred stepBirth(
	ebea.player.parameters(P),
	player(C, T), site,
	player(C, T),
	distribution, distribution,
	R, R,
	int, int,
	list(player(C, T)), list(player(C, T)))
	<= (ePRNG(R), chromosome(C, T, P)).
:- mode stepBirth(in, in, in, out, in, out, in, out, in, out, in, out) is det.



/**
 * death(Parameters, !Random, !Population, Deaths)
  
 * Apply the death algorithm to all players in the population.  The
 * probability that a player dies is given by a sigmoid function that
 * depends on the population size and a "carrying capacity" parameter.
 * Points of this sigmoid function are calculated by {@code
 * carryingCapacity/2}.
 */

:- pred stepDeath(
	ebea.population.parameters(P),
	player(C, T), site,
	R, R,
	list(player(C, T)), list(player(C, T)),
	list(player(C, T)), list(player(C, T)),
	list(player(C, T)), list(player(C, T)),
	list(player(C, T)), list(player(C, T))
	)
	<= (ePRNG(R), chromosome(C, T, P)).
:- mode stepDeath(in, in, in, in, out, in, out, in, out, in, out, in, out) is det.


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

:- pred removePlayers(list(player(C, T)), list(int), list(int), array(site), array(site)).
:- mode removePlayers(in, in, out, array_di, array_uo) is det.


:- pred debug(io.state, io.state).
:- mode debug(di, uo) is det.

:- implementation.

:- import_module ebea.player.age.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

initialisePlayers(PlayerParameters, SiteIndex, InitialPlayers, !SitePlayers, !ListPlayers, !ID, !Random) :-
	(if
		InitialPlayers^quantity >= 0
	then
		list.foldl3(
			makeCopy(PlayerParameters, SiteIndex, InitialPlayers^chromosome),
			!.ID..(!.ID + InitialPlayers^quantity - 1),
			!SitePlayers,
			!ListPlayers,
			!Random),
		!:ID = !.ID + InitialPlayers^quantity
	else
		throw("ebea.population.site.parameters.initialisePlayers/1: Invalid number of chromosome copies")
	).

createInitialSite(PlayerParameters, SiteParameters, MSiteIndex, MSiteNeighbourhood, InitialSite, !ListPlayers, !ID, !Random) :-
	(
		MSiteIndex = no,
		SiteIndex = SiteParameters^id
		;
		MSiteIndex = yes(SiteIndex)
	),
	list.foldl4(
		initialisePlayers(PlayerParameters, SiteIndex),
		SiteParameters^chromosomes,
		[], SitePlayers,
		!ListPlayers,
		!ID,
		!Random),
	InitialSite^carryingCapacity = float(SiteParameters^carryingCapacity),
	InitialSite^playerIDs = list.map(ebea.player.'ID', SitePlayers),
	(
		MSiteNeighbourhood = no,
		SiteNeighbourhood = SiteParameters^neighbourhood
		;
		MSiteNeighbourhood = yes(SiteNeighbourhood)
	),
	InitialSite^neighbourSiteIdxs = array.from_list(SiteNeighbourhood)
	.

createLatticeInitialSite(
	DefaultCarryingCapacity, Geometry, PlayerParameters,
	SiteIndex, InitialSite,
	!ListSiteParameters,
	!ListPlayers,
	!ID,
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
			!ListPlayers,
			!ID,
			!Random)
	else
		InitialSite^carryingCapacity = DefaultCarryingCapacity,
		InitialSite^playerIDs = [],
		InitialSite^neighbourSiteIdxs = array.from_list(makeConnections(Geometry, SiteIndex))
	).

	
numberPlayers(Site) = list.length(Site^playerIDs).

neighbours(ArraySites, Players, Player, Neighbours) :-
	array.member(ArraySites, Site),
	%
	list.delete(Site^playerIDs, PlayerID, RestIDs),
	Player = ebea.population.getPlayer(Players, PlayerID),
	SiteNeighbours = list.map(ebea.population.getPlayer(Players), RestIDs),
	%
	array.foldl(AppendPlayersSite, Site^neighbourSiteIdxs, SiteNeighbours) = Neighbours,
	AppendPlayersSite =
	(func(I, AC) = R :-
		S = array.lookup(ArraySites, I),
		R = list.append(list.map(ebea.population.getPlayer(Players), S^playerIDs), AC)
	).


neighbours(ArraySites, Players, Player) = Neighbours :-
	array.lookup(ArraySites, Player^siteIndex) = Site,
	%
	list.delete_all(Site^playerIDs, Player^id) = RestIDs,
	SiteNeighbours = list.map(ebea.population.getPlayer(Players), RestIDs),
	%
	array.foldl(AppendPlayersSite, Site^neighbourSiteIdxs, SiteNeighbours) = Neighbours,
	AppendPlayersSite =
	(func(I, AC) = R :-
		S = array.lookup(ArraySites, I),
		R = list.append(list.map(ebea.population.getPlayer(Players), S^playerIDs), AC)
	).

stepBirth(Parameters, Player, _Site, Parent, !Distribution, !Random, !NextID, !Nursery) :-
	ebea.player.reproduce(Parameters, Player, Parent, !.NextID + 1, MOffspring, !Distribution, !Random),
	(
		MOffspring = no
		;
		MOffspring = yes(Offspring),
		!:NextID = !.NextID + 1,
		!:Nursery = [Offspring | !.Nursery]
	)
	.

stepDeath(Parameters, Player, Site, !Random, !Players,
	!CemeteryCarryingCapacity, !CemeteryOldAge, !CemeteryStarvation)
:-
	deathProbability(Site^carryingCapacity, numberPlayers(Site)) = Value,
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
	(
		Dies = no,
		list.cons(Player, !Players)
		;
		Dies = yes(carryingCapacity),
		list.cons(Player, !CemeteryCarryingCapacity)
		;
		Dies = yes(oldAge),
		list.cons(Player, !CemeteryOldAge)
		;
		Dies = yes(starvation),
		list.cons(Player, !CemeteryStarvation)
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- pred makeCopy(
	ebea.player.parameters(P), int, ebea.player.chromosome(C), int,
	list(player(C, T)), list(player(C, T)),
	list(player(C, T)), list(player(C, T)),
	R, R)
	<= (chromosome(C, T, P), ePRNG(R)).
:- mode makeCopy(in, in, in, in, in, out, in, out, in, out) is det.

makeCopy(PlayerParameters, SiteIndex, Chromosome, PlayerID, !SitePlayers, !ListPlayers, !Random) :-
	ebea.player.init(PlayerParameters, Chromosome, PlayerID, SiteIndex, NewPlayer, !Random),
	!:ListPlayers = [NewPlayer | !.ListPlayers],
	!:SitePlayers = [NewPlayer | !.SitePlayers]
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
neighbour(lattice(XL, YL, N, B),   X, Y,    X + ((Y + 1) mod YL) * XL) :-
	B = torus
	;
	B = closed,
	Y < YL - 1
	;
	B = ring,
	Y < YL - 1
	.

% down
neighbour(lattice(XL, YL, N, B),   X, Y,    X + ((Y - 1) mod YL) * XL) :-
	B = torus
	;
	B = closed,
	Y > 0
	;
	B = ring,
	Y > 0
	.

% right
neighbour(lattice(XL, YL, N, B),   X, Y,    (X + 1) mod XL + Y * XL) :-
	B = torus
	;
	B = closed,
	X < XL - 1
	;
	B = ring
	.

% left
neighbour(lattice(XL, YL, N, B),   X, Y,    (X - 1) mod XL + Y * XL) :-
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
