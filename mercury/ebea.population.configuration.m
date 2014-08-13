/**
 * Provides types that are used to initialise the EBEA population.  These
 * types are aggregated in {@code configuration/2}.  There are two major
 * parameters, one for population geometry and the second for initial
 * players.  Geometry parameters dictate how many sites there are and how
 * they are connected.

 * @author Pedro Mariano
 * @version 1.0 2014/01/20
 */
:- module ebea.population.configuration.

:- interface.

:- import_module userInterface, parseable.
:- import_module ebea.population.site.parameters.
:- import_module list.

%% ************************************************************************
%% Represents the initial configuration of an EBEA population.  The user
%% can specify the population geometry, the chromosomes in each site, the
%% site's default carrying capacity, and the site update dynamics
%%
%% @param CS The game strategy and strategy genes.
%%
%% @param A The game actions.
%%
:- type configuration(CS, A) --->
	configuration(
		geometry                :: ebea.population.configuration.geometry ,
		sites                   :: list(ebea.population.site.parameters.parameters(CS)) ,
		defaultCarryingCapacity :: int ,
		siteDynamics            :: ebea.population.site.dynamics(A)
	).

:- type neighbourhood --->
	moore ;
	hexagonal ;
	vonNeumann .

:- type geometry --->
	wellmixed ;
	lattice(
		xSize         :: int ,
		ySize         :: int ,
		neighbourhood :: neighbourhood ,
		boundary      :: boundary
	) .

:- inst lattice == bound(lattice(ground, ground, ground, ground)).

:- type boundary --->
	torus ;
	ring ;
	closed .

/**
 * Return a default value of {@code configuration}.
 */
:- func default(CS) = ebea.population.configuration.configuration(CS, A).

:- func dialog(CS, list(dialogItem(CS))) = list(dialogItem(ebea.population.configuration.configuration(CS, A))).

:- pred parse(ebea.population.configuration.configuration(CS, A), list(int), list(int))
	<= parseable(CS).
:- mode parse(in, out, in) is det.
:- mode parse(out, in, out) is semidet.

/**
 * Return the initial population size.
 */
:- func populationSize(ebea.population.configuration.configuration(_, _)) = int.

:- pred parse_geometry(geometry, list(int), list(int)).
:- mode parse_geometry(in, out, in) is det.
:- mode parse_geometry(out, in, out) is semidet.

:- implementation.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

:- instance parseable(neighbourhood) where
[
	pred(parse/3) is ebea.population.configuration.parse_neighbourhood
].

:- instance parseable(geometry) where
[
	pred(parse/3) is ebea.population.configuration.parse_geometry
].

:- instance parseable(ebea.population.configuration.configuration(CS, A))
	<= parseable(CS) where
[
	pred(parse/3) is ebea.population.configuration.parse
].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

default(DefaultStrategyChromosome) = Result :-
	Result^geometry = default_geometry,
	Result^sites = [ebea.population.site.parameters.default(DefaultStrategyChromosome)],
	Result^defaultCarryingCapacity = default_defaultCarryingCapacity,
	Result^siteDynamics = static.

dialog(DefaultStrategyChromosome, DialogStrategyChromosome) =
	[
	 di(label("geometry"), 'new selectOneOf'(selectedGeometry, selectGeometry, set(set_geometry), listGeometryChoices)),
%	di(label("geometry"),  'new editField'(  get_geometry,  set(set_geometry),  ebea.population.parameters.dialog_geometry)),
	 di(label("sites"),
		'new editListFieldAny'(  get_sites,     set(set_sites),
	 		ebea.population.site.parameters.default(DefaultStrategyChromosome),
	 		ebea.population.site.parameters.dialog(yes, DefaultStrategyChromosome, DialogStrategyChromosome))),
	di(label("default carrying capacity"),  updateFieldInt(      get_defaultCarryingCapacity,  checkInt(   "default carrying capacity",  unbound, unbound, set_defaultCarryingCapacity)))
	].

parse(P) -->
	parse_geometry(P^geometry),
	parseable.parseList(normalType, P^sites),
	parseable.int32(P^defaultCarryingCapacity),
	{P^siteDynamics = throw("Implement siteDynamics parsing")}
	.

populationSize(Configuration) = list.foldl(Sum, Configuration^sites, 0) :-
	Sum =
	(func(Site, AC) = R :-
		R = ebea.population.site.parameters.populationSize(Site) + AC
	).
/*													 [Site | Rest]) =
	ebea.population.site.parameters.populationSize(Site)
	+ ebea.population.parameters.populationSize(Rest)
*/

parse_geometry(P) -->
	{P = wellmixed},
	[0]
	.

parse_geometry(P) -->
	{P = lattice(_, _, _, _)},
	[1],
	parseable.int32(P^xSize),
	parseable.int32(P^ySize),
	parse_neighbourhood(P^neighbourhood),
	parse_boundary(P^boundary)
	.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions


% :- pred geometries(geometry, int, int, geometry).
% :- mode geometries(in, out, out, out) is det.
% :- mode geometries(in, out, in, out) is semidet.

% geometries(wellmixed,           0, 0, wellmixed).
% geometries(lattice(_, _, _, _), 1, 0, wellmixed).
% geometries(wellmixed,           0, 1, lattice(default_xSize, default_ySize, default_neighbourhood, default_boundary)).
% geometries(lattice(X, Y, N, B), 1, 1, lattice(X, Y, N, B)).


:- func selectedGeometry(ebea.population.configuration.configuration(CS, A)) = maybe(currentChoice(geometry)).

selectedGeometry(Configuration) = yes(cc(Index, Configuration^geometry)) :-
	Configuration^geometry = wellmixed,
	Index = 0
	;
	Configuration^geometry = lattice(_, _, _, _),
	Index = 1
	.

:- func selectGeometry(ebea.population.configuration.configuration(CS, A), int) = setResult(selectChoice(ebea.population.configuration.configuration(CS, A), geometry)).

selectGeometry(Configuration, Index) = ok(sc(NextConfiguration, Field)) :-
	(if
		Configuration^geometry = wellmixed,
		Index = 0,
		NP = Configuration,
		F = Configuration^geometry
		;
		Configuration^geometry = lattice(_, _, _, _),
		Index = 0,
		NP = 'geometry :='(Configuration, F),
		F = wellmixed
		;
		Configuration^geometry = wellmixed,
		Index = 1,
		NP = 'geometry :='(Configuration, F),
		F = lattice(default_xSize, default_ySize, default_neighbourhood, default_boundary)
		;
		Configuration^geometry = lattice(_, _, _, _),
		Index = 1,
		NP = Configuration,
		F = Configuration^geometry
	then
		NextConfiguration = NP,
		Field = F
	else
		throw("selectGeometry/2: invalid index")
	).

:- func listGeometryChoices = list(choiceItem(geometry)).

listGeometryChoices =
	[
	 ci(label("wellmixed"),  []),
	 ci(label("lattice"),
		[
		 di(label("x size"),          updateFieldInt(      get_xSize,          checkInt(   "x size",          bounded(0, no), unbound, set_xSize))),
		 di(label("y size"),          updateFieldInt(      get_ySize,          checkInt(   "y size",          bounded(0, no), unbound, set_ySize))),
		 di(label("neighbourhood"),  'new selectOneOf'( selected_neighbourhood, select_neighbourhood,  set(set_neighbourhood),  listNeighbourhoodChoices)),
		 di(label("boundary"),       'new selectOneOf'( selected_boundary,      select_boundary,       set(set_boundary),       listBoundaryChoices))
		])].

/**
 * Return a default value of {@code neighbourhood}.
 */
:- func default_neighbourhood = neighbourhood.

default_neighbourhood = moore.

:- func selected_neighbourhood(geometry) = maybe(currentChoice(neighbourhood)).

selected_neighbourhood(Geometry) = Result :-
	Geometry = wellmixed,
	Result = no
	;
	Geometry = lattice(_, _, Neighbourhood, _),
	Result = yes(cc(Index, Neighbourhood)),
	(
		Neighbourhood = moore,
		Index = 0
		;
		Neighbourhood = hexagonal,
		Index = 1
		;
		Neighbourhood = vonNeumann,
		Index = 2
	)
	.

:- func select_neighbourhood(geometry, int) = setResult(selectChoice(geometry, neighbourhood)).

select_neighbourhood(Geometry, Index) = ok(sc(NextGeometry, Neighbourhood)) :-
	(
		Geometry = wellmixed,
		NG = lattice(default_xSize, default_ySize, default_neighbourhood, default_boundary)
		;
		Geometry = lattice(_, _, _, _),
		NG = Geometry
	),
	(if
		Index = 0,
		N = moore
		;
		Index = 1,
		N = hexagonal
		;
		Index = 2,
		N = vonNeumann
	then
		NextGeometry = 'neighbourhood :='(NG, N),
		Neighbourhood = N
	else
		throw("select_neighbourhood/2: Invalid index")
	).

:- func listNeighbourhoodChoices =  list(choiceItem(neighbourhood)).

listNeighbourhoodChoices =
	[
	 ci(label("moore"),       []),
	 ci(label("hexagonal"),   []),
	 ci(label("vonNeumann"),  [])
	].

:- func dialog_neighbourhood = list(dialogItem(neighbourhood)).

dialog_neighbourhood =
	[
	di(label("moore"),       newValue(moore)),
	di(label("hexagonal"),   newValue(hexagonal)),
	di(label("vonNeumann"),  newValue(vonNeumann))
	].


/**
 * Return a default value of {@code geometry}.
 */
:- func default_geometry = geometry.

default_geometry = wellmixed.

:- func dialog_geometry = list(dialogItem(geometry)).

dialog_geometry =
	[
	di(label("wellmixed"),  newValue(wellmixed)),
	di(label("lattice"),    subdialog( [
		di(label("x size"),          updateFieldInt(      get_xSize,          checkInt(   "xSize",          bounded(0, no), unbound, set_xSize))),
		di(label("y size"),          updateFieldInt(      get_ySize,          checkInt(   "ySize",          bounded(0, no), unbound, set_ySize))),
		di(label("neighbourhood"),  'new editField'(  get_neighbourhood,  set(set_neighbourhood),  dialog_neighbourhood)),
		di(label("boundary"),       'new editField'(  get_boundary,       set(set_boundary),       dialog_boundary))
		]))
	].


/**
 * Return a default value of {@code boundary}.
 */
:- func default_boundary = boundary.

default_boundary = torus.

:- func selected_boundary(geometry) = maybe(currentChoice(boundary)).

selected_boundary(Geometry) = Result :-
	Geometry = wellmixed,
	Result = no
	;
	Geometry = lattice(_, _, _, Boundary),
	Result = yes(cc(Index, Boundary)),
	(
		Boundary = torus,
		Index = 0
		;
		Boundary = ring,
		Index = 1
		;
		Boundary = closed,
		Index = 2
	)
	.

:- func select_boundary(geometry, int) = setResult(selectChoice(geometry, boundary)).

select_boundary(Geometry, Index) = ok(sc(NextGeometry, Boundary)) :-
	(
		Geometry = wellmixed,
		NG = lattice(default_xSize, default_ySize, default_neighbourhood, default_boundary)
		;
		Geometry = lattice(_, _, _, _),
		NG = Geometry
	),
	(if
		Index = 0,
		B = torus
		;
		Index = 1,
		B = ring
		;
		Index = 2,
		B = closed
	then
		NextGeometry = 'boundary :='(NG, B),
		Boundary = B
	else
		throw("select_boundary/2: Invalid index")
	).

:- func listBoundaryChoices =  list(choiceItem(boundary)).

listBoundaryChoices =
	[
	 ci(label("torus"),   []),
	 ci(label("ring"),    []),
	 ci(label("closed"),  [])
	].

:- func dialog_boundary = list(dialogItem(boundary)).

dialog_boundary =
	[
	di(label("torus"),   newValue(torus)),
	di(label("ring"),    newValue(ring)),
	di(label("closed"),  newValue(closed))
	].

:- pred parse_boundary(boundary, list(int), list(int)).
:- mode parse_boundary(in, out, in) is det.
:- mode parse_boundary(out, in, out) is semidet.

parse_boundary(P) -->
	{P = torus},
	[0]
	.

parse_boundary(P) -->
	{P = ring},
	[1]
	.

parse_boundary(P) -->
	{P = closed},
	[2]
	.

:- pred parse_neighbourhood(neighbourhood, list(int), list(int)).
:- mode parse_neighbourhood(in, out, in) is det.
:- mode parse_neighbourhood(out, in, out) is semidet.

parse_neighbourhood(P) -->
	{P = moore},
	[0]
	.

parse_neighbourhood(P) -->
	{P = hexagonal},
	[1]
	.

parse_neighbourhood(P) -->
	{P = vonNeumann},
	[2]
	.

:- func default_xSize = int.

default_xSize = 10.

:- func default_ySize = int.

default_ySize = 10.



:- func get_xSize(geometry) = int.

get_xSize(P) = R :-
	P = wellmixed,
	R = default_xSize
	;
	P = lattice(_, _, _, _),
	R = P^xSize
	.

:- func set_xSize(geometry, int) = geometry.

set_xSize(P, V) = R :-
	P = wellmixed,
	R = lattice(V, default_ySize, default_neighbourhood, default_boundary)
	;
	P = lattice(_, _, _, _),
	R = 'xSize :='(P, V)
	.


:- func get_ySize(geometry) = int.

get_ySize(P) = R :-
	P = wellmixed,
	R = default_ySize
	;
	P = lattice(_, _, _, _),
	R = P^ySize
	.

:- func set_ySize(geometry, int) = geometry.

set_ySize(P, V) = R :-
	P = wellmixed,
	R = lattice(default_xSize, V, default_neighbourhood, default_boundary)
	;
	P = lattice(_, _, _, _),
	R = 'ySize :='(P, V)
	.


:- func get_neighbourhood(geometry) = neighbourhood.

get_neighbourhood(P) = R :-
	P = wellmixed,
	R = default_neighbourhood
	;
	P = lattice(_, _, _, _),
	R = P^neighbourhood
	.

:- func set_neighbourhood(geometry, neighbourhood) = geometry.

set_neighbourhood(P, V) = R :-
	P = wellmixed,
	R = lattice(default_xSize, default_ySize, V, default_boundary)
	;
	P = lattice(_, _, _, _),
	R = 'neighbourhood :='(P, V)
	.


:- func get_boundary(geometry) = boundary.

get_boundary(P) = R :-
	P = wellmixed,
	R = default_boundary
	;
	P = lattice(_, _, _, _),
	R = P^boundary
	.

:- func set_boundary(geometry, boundary) = geometry.

set_boundary(P, V) = R :-
	P = wellmixed,
	R = lattice(default_xSize, default_ySize, default_neighbourhood, V)
	;
	P = lattice(_, _, _, _),
	R = 'boundary :='(P, V)
	.


:- func default_sites = list(ebea.population.site.parameters.parameters(CS)).

default_sites = [].

:- func default_defaultCarryingCapacity = int.

default_defaultCarryingCapacity = 10.


:- func get_geometry(ebea.population.configuration.configuration(CS, A)) = ebea.population.configuration.geometry.

get_geometry(P) = P^geometry.


:- func set_geometry(ebea.population.configuration.configuration(CS, A), ebea.population.configuration.geometry) = ebea.population.configuration.configuration(CS, A).

set_geometry(P, V) = 'geometry :='(P, V).



:- func get_sites(ebea.population.configuration.configuration(CS, A)) = list(ebea.population.site.parameters.parameters(CS)).

get_sites(P) = P^sites.


:- func set_sites(ebea.population.configuration.configuration(CS, A), list(ebea.population.site.parameters.parameters(CS))) = ebea.population.configuration.configuration(CS, A).

set_sites(P, V) = 'sites :='(P, V).



:- func get_defaultCarryingCapacity(ebea.population.configuration.configuration(CS, A)) = int.

get_defaultCarryingCapacity(P) = P^defaultCarryingCapacity.


:- func set_defaultCarryingCapacity(ebea.population.configuration.configuration(CS, A), int) = ebea.population.configuration.configuration(CS, A).

set_defaultCarryingCapacity(P, V) = 'defaultCarryingCapacity :='(P, V).




:- end_module ebea.population.configuration.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:

