/**
 * Base module that contains the data and predicates necessary to run a
 * simulation of EBEA.  Module {@code data.config} and its submodules
 * contain configuration parameters, predicates to edit this parameters,
 * and predicates to read and save the parameter to a file.  Module {@code
 * data.prng} contains the pseudo-random number generators used in the
 * configuration parameters.  Module {@code data.seed} contains the seed
 * used by the pseudo-random number generators.

 * @author Pedro Mariano
 * @version 1.0 2013/12/07
 */
:- module data.

:- interface.

:- include_module config, prng, seed.
:- include_module util.

:- type games --->
	'2x2' ;
	battlesexes ;
	centipede ;
	givetake ;
	investment ;
	pgp ;
	'pgp+pa' ;
	ultimatum.

:- implementation.

:- import_module parseable.
:- import_module list.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions


:- pred parseGames(games, parseable.state, parseable.state).
:- mode parseGames(in, out, in) is det.
:- mode parseGames(out, in, out) is semidet.

parseGames('2x2')       --> [0].
parseGames(battlesexes) --> [1].
parseGames(centipede)   --> [2].
parseGames(pgp)         --> [3].
parseGames('pgp+pa')    --> [4].
parseGames(ultimatum)   --> [5].
parseGames(givetake)    --> [6].
parseGames(investment)  --> [7].

:- end_module data.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
