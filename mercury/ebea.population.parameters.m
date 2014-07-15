/**
 * Provides types that are used to initialise the EBEA population.  These
 * types are aggregated in {@code parameters/1}.  There are two major
 * parameters, one for population geometry and the second for initial
 * players.  Geometry parameters dictate how many sites there are and how
 * they are connected.

 * @author Pedro Mariano
 * @version 1.0 2014/01/20
 */
:- module ebea.population.parameters.

:- interface.

:- import_module userInterface, parseable.
:- import_module ebea.population.site.parameters.
:- import_module list.

:- type parameters(CS) --->
	parameters(
		geometry :: ebea.population.parameters.geometry ,
		sites    :: list(ebea.population.site.parameters.parameters(CS)),
		defaultCarryingCapacity :: int
	) .

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
 * Return a default value of {@code parameters}.
 */
:- func default(CS) = ebea.population.parameters.parameters(CS).

:- func dialog(CS, list(dialogItem(CS))) = list(dialogItem(ebea.population.parameters.parameters(CS))).

:- pred parse(ebea.population.parameters.parameters(CS), list(int), list(int))
	<= parseable(CS).
:- mode parse(in, out, in) is det.
:- mode parse(out, in, out) is semidet.


:- implementation.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

:- instance parseable(neighbourhood) where
[
	pred(parse/3) is ebea.population.parameters.parse_neighbourhood
].

:- instance parseable(geometry) where
[
	pred(parse/3) is ebea.population.parameters.parse_geometry
].

:- instance parseable(ebea.population.parameters.parameters(CS))
	<= parseable(CS) where
[
	pred(parse/3) is ebea.population.parameters.parse
].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

default(DefaultStrategyChromosome) = Result :-
	Result^geometry = default_geometry,
	Result^sites = [ebea.population.site.parameters.default(DefaultStrategyChromosome)],
	Result^defaultCarryingCapacity = default_defaultCarryingCapacity.

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
	% {P = parameters(_, _, _)},
	% {P^geometry = throw("TODO implement parsing")},
	% {P^sites = throw("TODO implement parsing")},
	parseable.int32(P^defaultCarryingCapacity)
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


:- func selectedGeometry(ebea.population.parameters.parameters(CS)) = maybe(currentChoice(geometry)).

selectedGeometry(Parameters) = yes(cc(Index, Parameters^geometry)) :-
	Parameters^geometry = wellmixed,
	Index = 0
	;
	Parameters^geometry = lattice(_, _, _, _),
	Index = 1
	.

:- func selectGeometry(ebea.population.parameters.parameters(CS), int) = setResult(selectChoice(ebea.population.parameters.parameters(CS), geometry)).

selectGeometry(Parameters, Index) = ok(sc(NextParameters, Field)) :-
	(if
		Parameters^geometry = wellmixed,
		Index = 0,
		NP = Parameters,
		F = Parameters^geometry
		;
		Parameters^geometry = lattice(_, _, _, _),
		Index = 0,
		NP = 'geometry :='(Parameters, F),
		F = wellmixed
		;
		Parameters^geometry = wellmixed,
		Index = 1,
		NP = 'geometry :='(Parameters, F),
		F = lattice(default_xSize, default_ySize, default_neighbourhood, default_boundary)
		;
		Parameters^geometry = lattice(_, _, _, _),
		Index = 1,
		NP = Parameters,
		F = Parameters^geometry
	then
		NextParameters = NP,
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


:- pred parse_geometry(geometry, list(int), list(int)).
:- mode parse_geometry(in, out, in) is det.
:- mode parse_geometry(out, in, out) is semidet.

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
	% {P^neighbourhood = throw("TODO implement parsing")},
	% {P^boundary = throw("TODO implement parsing")}
	.

:- func default_sites = list(ebea.population.site.parameters.parameters(CS)).

default_sites = [].

:- func default_defaultCarryingCapacity = int.

default_defaultCarryingCapacity = 10.


:- func get_geometry(ebea.population.parameters.parameters(CS)) = ebea.population.parameters.geometry.

get_geometry(P) = P^geometry.


:- func set_geometry(ebea.population.parameters.parameters(CS), ebea.population.parameters.geometry) = ebea.population.parameters.parameters(CS).

set_geometry(P, V) = 'geometry :='(P, V).



:- func get_sites(ebea.population.parameters.parameters(CS)) = list(ebea.population.site.parameters.parameters(CS)).

get_sites(P) = P^sites.


:- func set_sites(ebea.population.parameters.parameters(CS), list(ebea.population.site.parameters.parameters(CS))) = ebea.population.parameters.parameters(CS).

set_sites(P, V) = 'sites :='(P, V).



:- func get_defaultCarryingCapacity(ebea.population.parameters.parameters(CS)) = int.

get_defaultCarryingCapacity(P) = P^defaultCarryingCapacity.


:- func set_defaultCarryingCapacity(ebea.population.parameters.parameters(CS), int) = ebea.population.parameters.parameters(CS).

set_defaultCarryingCapacity(P, V) = 'defaultCarryingCapacity :='(P, V).




:- end_module ebea.population.parameters.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:

