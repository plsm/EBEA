/**
 * Game library

 * @author Pedro Mariano
 * @version 1.0 2012/06/25
 * @version 2.0 2013/12/30
 */
:- module gl.

:- interface.


:- include_module '2x2', battlesexes, centipede, givetake, investment, pgp, 'pgp+pa', ultimatum.

:- import_module chromosome, game, gfactory, foldable, printable, parseable.
:- import_module userInterface, rng, rng.distribution.
:- import_module array, float, io, int, list.

:- implementation.

:- import_module unit.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

/**
 * This function is used to specify the born behaviour of type class {@code
 * chromosome(Strategy,Parameter)}.

 * <p> This function is part of type class {@code chromosome(strategy)}.
 *
 */
:- func simpleBorn(P, S) = unit.

simpleBorn(_, _) = unit.

:- end_module gl.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
