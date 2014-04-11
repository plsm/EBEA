/**
 * Provides a type that is used to perform a series of runs of the Energy
 * Based Evolutionary Algorithm with the same set of parameters.

 * @author Pedro Mariano
 * @version 1.0 2014/01/ 3
 */
:- module data.config.runnable.

:- interface.

% /**
%  * Represents the data needed to perform a run of the Energy Based
%  * Evolutionary Algorithm.  An instance of this type can be used to obtain
%  * the input parameters for predicates and function {@code
%  * ebea.streams.openOutputStreams/4}, {@code ebea.population.init/5},
%  * {@code ebea.core.init/3}.  The result of these predicates and function
%  * is needed for predicate {@code ebea.core.run/8}.
  
%  */
% :- type config --->
% 	some [R, G, CS, P, T, A]
% 	(cfg(config(R, G, CS, P))
% 	=> (
% 		ePRNG(R),
% 		game(G, CS),
% 		chromosome(CS, T, P),
% 		foldable(CS, A),
% 		printable(CS),
% 		printable(T),
% 		printable(A))
% 	).

% :- type config(R, G, CS, P) --->
% 	config(
% 		random            :: R,
% 		numberRuns        :: int,
% 		game              :: G,
% 		parameters        :: ebea.population.parameters(P),
% 		initialPopulation :: list({int, ebea.player.chromosome(CS)}),
% 		streams           :: ebea.streams.outStreams
% 	).


:- implementation.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- end_module data.config.runnable.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
