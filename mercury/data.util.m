/**
 * 

 * @author Pedro Mariano
 * @version 1.0 2014/03/11
 */
:- module data.util.

:- interface.

:- import_module chromosome, game.
:- import_module foldable, printable.
:- import_module data.config.
:- import_module ebea, ebea.population, ebea.population.configuration.


:- type exGameConfig3 --->
	some [G, CS, P, T, ACS, A] (gcex(G, P, ebea.population.configuration.configuration(CS, A))
	=> (asymmetricGame(G, CS),
			chromosome(CS, T, P),
			foldable(CS, ACS),
			parseable(CS),
			printable(CS),
			printable(T),
			printable(ACS))).

:- func gameConfig(config) = exGameConfig3.

:- implementation.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

gameConfig(Config) = Result :-
	Config^selectedGame = '2x2',       Result = 'new gcex'( Config^cfg_2x2    ^game,  Config^cfg_2x2    ^parameters,  Config^cfg_2x2    ^initialPopulation) ;
	Config^selectedGame = battlesexes, Result = 'new gcex'( Config^battlesexes^game,  Config^battlesexes^parameters,  Config^battlesexes^initialPopulation) ;
	Config^selectedGame = centipede,   Result = 'new gcex'( Config^centipede  ^game,  Config^centipede  ^parameters,  Config^centipede  ^initialPopulation) ;
	Config^selectedGame = givetake,    Result = 'new gcex'( Config^givetake   ^game,  Config^givetake   ^parameters,  Config^givetake   ^initialPopulation) ;
	Config^selectedGame = investment,  Result = 'new gcex'( Config^investment ^game,  Config^investment ^parameters,  Config^investment ^initialPopulation) ;
	Config^selectedGame = pgp,         Result = 'new gcex'( Config^pgp        ^game,  Config^pgp        ^parameters,  Config^pgp        ^initialPopulation) ;
	Config^selectedGame = 'pgp+pa',    Result = 'new gcex'( Config^'pgp+pa'   ^game,  Config^'pgp+pa'   ^parameters,  Config^'pgp+pa'   ^initialPopulation) ;
	Config^selectedGame = ultimatum,   Result = 'new gcex'( Config^ultimatum  ^game,  Config^ultimatum  ^parameters,  Config^ultimatum  ^initialPopulation)
	.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- end_module data.util.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
