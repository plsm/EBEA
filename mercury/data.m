/**
 * 

 * @author Pedro Mariano
 * @version 1.0 2013/12/ 7
 */
:- module data.

:- interface.

:- include_module config, prng.

:- type games --->
	'2x2' ;
	battlesexes ;
	centipede ;
%	investment ;
	pgp ;
	'pgp+pa' ;
	ultimatum.

:- implementation.

:- import_module list.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions


:- pred parseGames(games, list(int), list(int)).
:- mode parseGames(in, out, in) is det.
:- mode parseGames(out, in, out) is semidet.

parseGames('2x2')       --> [0].
parseGames(battlesexes) --> [1].
parseGames(centipede)   --> [2].
parseGames(pgp)         --> [3].
parseGames('pgp+pa')    --> [4].
parseGames(ultimatum)   --> [5].

:- end_module data.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
