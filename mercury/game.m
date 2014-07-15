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
:- import_module array, maybe.

% /**
%  * Type class that defines the interface of a game.  The methods give
%  * information about the payoff range, the payoff obtained by a Pareto
%  * profile, and the number of players in the game.  There is another method
%  * that plays the game given a strategy profile.
%  */

:- typeclass abstractGame(G) where
[
	/**
	 * Return the lowest payoff obtainable in the game.
	 */
	func lowestPayoff(G) = float,
	/**
	 * Return the highest payoff obtainable in the game.
	 */
	func highestPayoff(G) = float,
	/**
	 * Return the Pareto payoff which is interpreted as the most social
	 * favourable payoff.
	 */
	func paretoPayoff(G) = float,
	/**
	 * Return the number of players in a game.
	 */
	func numberPlayers(G) = int

	% pred play(G, profile(S), R, R, payoffVector) <= ePRNG(R),
	% mode play(in, in, in, out, out) is det
].

:- typeclass asymmetricGame(G, S) <= (abstractGame(G), (G -> S)) where
[
	func numberRoles(G) = int,
 
	pred playAsymmetric(G, profile(S), R, R, maybe(payoffVector)) <= ePRNG(R),
	mode playAsymmetric(in, in, in, out, out) is det
].

:- typeclass symmetricGame(G, S) <= (abstractGame(G), (G -> S)) where
[
	pred playSymmetric(G, profile(S), R, R, payoffVector) <= ePRNG(R),
	mode playSymmetric(in, in, in, out, out) is det
].


:- typeclass game(G, S, A) <= (abstractGame(G), (G -> (S, A))) where
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

/**
 * This function can be used in instances of {@code asymmetricGame(G,S)}
 * that are really symmetric games.  This function returns the value one.
  
 */
:- func singleRole(_) = int.

:- pred playSymmetricBridge(G, profile(S), R, R, maybe(payoffVector))
	<= (symmetricGame(G, S), ePRNG(R)).
:- mode playSymmetricBridge(in, in, in, out, out) is det.

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

singleRole(_) = 1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

playSymmetricBridge(Game, Profile, !Random, yes(Payoff)) :-
	playSymmetric(Game, Profile, !Random, Payoff).

:- end_module game.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
