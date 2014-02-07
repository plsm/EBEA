/**
 * Defines the type class game.  Defines the interface of a game.  The
 * methods give information about the payoff range, the payoff obtained by
 * a Pareto profile, and the number of players in the game.  There is
 * another method that plays the game given a strategy profile.

 * @author Pedro Mariano
 * @version 1.2 2012/05/26
 */
:- module game.

:- interface.

:- import_module rng.
:- import_module array.

% /**
%  * Type class that defines the interface of a game.  The methods give
%  * information about the payoff range, the payoff obtained by a Pareto
%  * profile, and the number of players in the game.  There is another method
%  * that plays the game given a strategy profile.
%  */

:- typeclass game(G, S) <= ((G -> S), (S -> G)) where
[
	func lowestPayoff(G) = float,
	func highestPayoff(G) = float,
	func paretoPayoff(G) = float,
	func numberPlayers(G) = int,

	pred play(G, profile(S), R, R, payoffVector) <= ePRNG(R),
	mode play(in, in, in, out, out) is det
].

:- typeclass game(G, S, A) <= (game(G, S), (G -> (S, A))) where
[
	pred play(G, profile(S), R, R, payoffVector, actionVector(A)) <=ePRNG(R),
	mode play(in, in, in, out, out, out) is det
].

/**
 * A strategy profile is a strategy array.
 */
:- type profile(S) == array(S).

/**
 * The payoff vector is implemented as an array of floating point values.
 */
:- type payoffVector == array(float).

:- type actionVector(A) == array(A).

/**
 * Return the {@code index} strategy of the given profile.  Indexes start at zero.
  
 */
:- func strategy(profile(S), int) = S.

:- func payoff(payoffVector, int) = float.

:- implementation.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

strategy(Profile, Index) = Result :-
	array.lookup(Profile, Index) = Result.

payoff(PayoffVector, Index) = Result :-
	array.lookup(PayoffVector, Index) = Result.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- end_module game.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
