/**
 * 

 * @author Pedro Mariano
 * @version 1.0 2014/01/20
 */
:- module ebea.population.site.parameters.

:- interface.

:- import_module ebea.player.

:- import_module userInterface.

:- import_module parseable.

:- import_module bool.

/**
 * Represents the initial state of some site.  Each site has a carrying
 * capacity, a number of initial players and a neighbourhood.
 */

:- type parameters(CS) --->
	parameters(
		id               :: int ,
		carryingCapacity :: int ,
		chromosomes      :: list(initialPlayers(CS)) ,
		neighbourhood    :: list(int)
	) .

:- type initialPlayers(CS) --->
	initialPlayers(
		quantity   :: int,
		chromosome :: ebea.player.chromosome(CS)
	).

:- instance parseable(ebea.population.site.parameters.parameters(CS)) <= parseable(CS).

/**
 * Return a default value of {@code parameters}.
 */
:- func default(CS) = ebea.population.site.parameters.parameters(CS).

:- func dialog(bool, CS, list(dialogItem(CS))) = list(dialogItem(ebea.population.site.parameters.parameters(CS))).


:- implementation.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

:- instance parseable(ebea.population.site.parameters.parameters(CS))
	<= parseable(CS) where
[
	pred(parse/3) is ebea.population.site.parameters.parse
].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

:- instance parseable(initialPlayers(CS))
	<= parseable(CS) where
[
	pred(parse/3) is parse_initialPlayers
].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

default(DefaultStrategyChromosome) = Result :-
	Result^id =  default_id,
	Result^carryingCapacity = default_carryingCapacity,
	Result^chromosomes = [default_chromosomes(DefaultStrategyChromosome)],
	Result^neighbourhood =  default_neighbourhood.

dialog(NeighbourhoodFlag, DefaultStrategyChromosome, DialogStrategyChromosome) =
	[
	 di(label("site index"),         updateFieldInt(    get_id,  checkInt( "site index",  bounded(0, yes), unbound, set_id))),
	 di(label("carrying capacity"),  updateFieldInt(    get_carryingCapacity,  checkInt( "carryingCapacity",  bounded(0, no), unbound, set_carryingCapacity))),
	 di(label("chromosomes"),       'new editListFieldAny'(  get_chromosomes,       set(set_chromosomes),       default_chromosomes(DefaultStrategyChromosome), dialog_initialPlayers(DialogStrategyChromosome))) |
	Neighbourhood
	] :-
	(
		NeighbourhoodFlag = yes,
		Neighbourhood = [di(label("neighbourhood"),     updateListFieldInt(  get_neighbourhood,     set(set_neighbourhood)))]
		;
		NeighbourhoodFlag = no,
		Neighbourhood = []
	).

% /**
%  * Initialise the players in some site given the initial site state parameters.
%  */
% :- pred initialisePlayers(ebea.player.parameters(P), int, initialPlayers(CS), list(player(CS, T)), list(player(CS, T)), int, int, R, R)
% 	<= (chromosome(CS, T, P), ePRNG(R)).
% :- mode initialisePlayers(in, in, in, in, out, in, out, in, out) is det.


% initialisePlayers(PlayerParameters, SiteIndex, InitialPlayers, !ListPlayers, !ID, !Random) :-
% 	(if
% 		InitialPlayers^quantity >= 0
% 	then
% 		InitChromosome =
% 		(pred(ID2::in, AC2::in, Result2::out, RndIn2::in, RndOu2::out) is det :-
% 			ebea.player.init(PlayerParameters, InitialPlayers^chromosome, ID2, SiteIndex, Off, RndIn2, RndOu2),
% 			Result2 = [Off | AC2]
% 		),
% 		int.fold_up2(InitChromosome, !.ID, !.ID + InitialPlayers^quantity - 1, !ListPlayers, !Random),
% 		!:ID = !.ID + InitialPlayers^quantity
% 	else
% 		throw("ebea.population.site.parameters.initialisePlayers/1: Invalid number of chromosome copies")
% 	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- func default_id = int.

default_id = 0.

:- func default_carryingCapacity = int.

default_carryingCapacity = 10.

:- func default_chromosomes(CS) = initialPlayers(CS).

default_chromosomes(DefaultStrategyChromosome) = default_initialPlayers(DefaultStrategyChromosome).

:- func default_neighbourhood = list(int).

default_neighbourhood = [].



:- func get_id(ebea.population.site.parameters.parameters(CS)) = int.

get_id(P) = P^id.


:- func set_id(ebea.population.site.parameters.parameters(CS), int) = ebea.population.site.parameters.parameters(CS).

set_id(P, V) = 'id :='(P, V).



:- func get_carryingCapacity(ebea.population.site.parameters.parameters(CS)) = int.

get_carryingCapacity(P) = P^carryingCapacity.


:- func set_carryingCapacity(ebea.population.site.parameters.parameters(CS), int) = ebea.population.site.parameters.parameters(CS).

set_carryingCapacity(P, V) = 'carryingCapacity :='(P, V).




:- func get_chromosomes(ebea.population.site.parameters.parameters(CS)) = list(initialPlayers(CS)).

get_chromosomes(P) = P^chromosomes.


:- func set_chromosomes(ebea.population.site.parameters.parameters(CS), list(initialPlayers(CS))) = ebea.population.site.parameters.parameters(CS).

set_chromosomes(P, V) = 'chromosomes :='(P, V).



:- func get_neighbourhood(ebea.population.site.parameters.parameters(CS)) = list(int).

get_neighbourhood(P) = P^neighbourhood.


:- func set_neighbourhood(ebea.population.site.parameters.parameters(CS), list(int)) = ebea.population.site.parameters.parameters(CS).

set_neighbourhood(P, V) = 'neighbourhood :='(P, V).




:- pred parse(ebea.population.site.parameters.parameters(CS), list(int), list(int))
	<= parseable(CS).
:- mode parse(in, out, in) is det.
:- mode parse(out, in, out) is semidet.

parse(P) -->
	{P = parameters(_, _, _, _)},
	parseable.int32(P^id),
	parseable.int32(P^carryingCapacity),
	parseable.parseList(withLength, P^chromosomes), %{P^chromosomes = throw("TODO implement parsing")},
	parseable.parseList(normalType, P^neighbourhood) %{P^neighbourhood = throw("TODO implement parsing")}
	.



/**
 * Return a default value of {@code initialPlayers(CS)}.
 */
:- func default_initialPlayers(CS) = initialPlayers(CS).

default_initialPlayers(DefaultStrategyChromosome) = Result :-
	Result^quantity = default_quantity,
	Result^chromosome = ebea.player.defaultChromosome(DefaultStrategyChromosome).


:- func dialog_initialPlayers(list(dialogItem(CS))) = list(dialogItem(initialPlayers(CS))).

dialog_initialPlayers(DialogStrategyChromosome) =
	[
	di(label("quantity"),    updateFieldInt(  get_quantity,   checkInt("quantity", bounded(0, no), unbound, set_quantity))),
	di(label("chromosome"),  'new editField'( get_chromosome, set(set_chromosome), ebea.player.dialog(DialogStrategyChromosome)))
	].

:- func default_quantity = int.

default_quantity = 10.


:- func get_quantity(initialPlayers(CS)) = int.

get_quantity(P) = P^quantity.


:- func set_quantity(initialPlayers(CS), int) = initialPlayers(CS).

set_quantity(P, V) = 'quantity :='(P, V).



:- func get_chromosome(initialPlayers(CS)) = ebea.player.chromosome(CS).

get_chromosome(P) = P^chromosome.


:- func set_chromosome(initialPlayers(CS), ebea.player.chromosome(CS)) = initialPlayers(CS).

set_chromosome(P, V) = 'chromosome :='(P, V).

:- pred parse_initialPlayers(initialPlayers(CS), list(int), list(int))
	<= parseable(CS).
:- mode parse_initialPlayers(in, out, in) is det.
:- mode parse_initialPlayers(out, in, out) is semidet.

parse_initialPlayers(X) -->
	parseable.int16(X^quantity),
	ebea.player.parseChromosome(X^chromosome).

:- end_module ebea.population.site.parameters.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:

