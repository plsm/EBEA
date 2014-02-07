/**
 * Provides an implementation of the Public Good Provision game.  

 * @author Pedro Mariano
 * @version 1.0 2012/06/25
 * @version 2.0 2013/12/30
 */
:- module gl.pgp.

:- interface.
:- import_module userInterface.

:- include_module game, strategy, parameters, factory.
:- import_module gl.pgp.game, gl.pgp.strategy, gl.pgp.factory, gl.pgp.parameters.

:- import_module unit.


/**
 * The accumulator used to reduce a collection of PGP strategies.
 */
:- type pgpAc.


:- instance game(game, strategy).

:- instance chromosome(strategy, unit, parameters).

:- instance factory(factory, game, strategy, parameters).

:- instance foldable(strategy, pgpAc).

:- instance printable(pgpAc).

:- implementation.

:- import_module rng, rng.distribution.
:- import_module array, bool, exception, float, int, list, maybe, string.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types


:- type pgpAc --->
	ac(qty::int,
		prob::float
	  ).


:- instance game(game, strategy) where
[
	func(lowestPayoff/1)  is gl.pgp.game.lowestPayoff,
	func(highestPayoff/1) is gl.pgp.game.highestPayoff,
	func(paretoPayoff/1)  is gl.pgp.game.paretoPayoff,
	func(numberPlayers/1) is gl.pgp.game.players,
	pred(play/5)          is gl.pgp.play
].

:- instance chromosome(strategy, unit, parameters) where
[
	func(numberGenes/1) is gl.pgp.strategy.numberParameters,
	pred(mutateGene/8)  is gl.pgp.strategy.mutateGene,
	func(born/2)        is gl.pgp.born
].

:- instance factory(factory, game, strategy, parameters) where
[
	pred(value/5) is gl.pgp.factory.value
].

:- instance foldable(strategy, pgpAc) where
[
	func(fold/2) is gl.pgp.fold,
	func(initAC/0) is gl.pgp.fold
].


:- instance printable(pgpAc)
	where
[
	pred(print/4) is gl.pgp.writeAc
].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

% :- pred printParseErrors(string, io, io).
% :- mode printParseErrors(in, di, uo) is det.

% printParseErrors(Line, !IO) :-
% 	(if
% 		string.words(Line) = [SPlayers, SGood, SProvisionCost | Rest]
% 	then
% 		% handle parsing errors
% 		(if
% 			string.to_int(SPlayers, Players1),
% 			Players1 =< 0
% 		then
% 			io.print(io.stderr_stream, "The number of players must be a positive integer\n", !IO)
% 		else if
% 			not string.to_int(SPlayers, _)
% 		then
% 			io.print(io.stderr_stream, "The number of players is not an integer\n", !IO)
% 		else
% 			true
% 		),
% 		(if
% 			string.to_float(SGood, Good1),
% 			string.to_float(SProvisionCost, ProvisionCost1),
% 			Good1 =< ProvisionCost1
% 		then
% 			io.print(io.stderr_stream, "The good value must be greater than provision cost\n", !IO)
% 		else if
% 			not string.to_float(SGood, _)
% 		then
% 			io.print(io.stderr_stream, "The good value is not a floating point number\n", !IO)
% 		else
% 			true
% 		),
% 		(if
% 			string.to_float(SProvisionCost, ProvisionCost2),
% 			ProvisionCost2 =< 0.0
% 		then
% 			io.print(io.stderr_stream, "The provision cost must be a positive floating point number\n", !IO)
% 		else if
% 			not string.to_float(SProvisionCost, _)
% 		then
% 			io.print(io.stderr_stream, "The provision cost is not a floating point number\n", !IO)
% 		else
% 			true
% 		),
% 		(if
% 			string.to_int(SPlayers, Players2),
% 			string.to_float(SGood, Good2),
% 			string.to_float(SProvisionCost, ProvisionCost3),
% 			Players2 > 0,
% 			Good2 > ProvisionCost3,
% 			ProvisionCost3 > 0.0,
% 			Good2 - ProvisionCost3 >= float(Players2 - 1) * Good2 / float(Players2)
% 		then
% 			io.print(io.stderr_stream, "There is no dillema\n", !IO)
% 		else
% 			true
% 		),
% 		(if
% 			util.comment(Rest)
% 		then
% 			true
% 		else
% 			io.print(io.stderr_stream, "Unknown information: '" ++ string(Rest) ++ "\n", !IO)
% 		)
% 	else
% 		io.print(io.stderr_stream, "Expecting three values: number of players, good value and provision cost.  Read ' " ++ Line ++ " '\n", !IO)
% 	).

/**
 * play(Game, Profile, !Random, Payoffs)
  
 * Compute a game between player profile and return their payoff.

 * @param Random The pseudo-random number generator used to calculate
 * players' provision.
 */

:- pred play(game, array(strategy), R, R, array(float))
	<= ePRNG(R).
:- mode play(in, in, in, out, out) is det.

play(Game, Profile, !Random, Payoffs) :-
	% compute the contribution of each player to the good
	ContributePred =
	(pred(Strategy::in, ActionsIn::in, ActionsOut::out, Rin::in, Rout::out, NumberContributorsIn::in, NumberContributorsOut::out) is det :-
		Strategy = prob(ContributeProbability),
		rng.flipCoin(ContributeProbability, Contributes, Rin, Rout),
		ActionsOut = [Contributes | ActionsIn],
		(
			Contributes = no,
			NumberContributorsIn = NumberContributorsOut
			;
			Contributes = yes,
			NumberContributorsIn + 1.0 = NumberContributorsOut
		)
		;
		Strategy = dete(Contributes),
		ActionsOut = [Contributes | ActionsIn],
		Rin = Rout,
		(
			Contributes = no,
			NumberContributorsIn = NumberContributorsOut
			;
			Contributes = yes,
			NumberContributorsIn + 1.0 = NumberContributorsOut
		)
	),
	array.foldl3(ContributePred, Profile, [], Actions, !Random, 0.0, NumberContributors),
	% compute players' payoff based on their action and number of contributors
	PayoffPred =
	(pred(_Index::in, Payoff::out, ActionsIn::in, ActionsOut::out) is det :-
		ActionsIn = [],
		throw("gl.pgp.play/5: Not reachable")
		;
		ActionsIn = [Action | ActionsOut],
		(
			Action = no,
			Payoff = NumberContributors * Game^good / float(Game^players)
			;
			Action = yes,
			Payoff = NumberContributors * Game^good / float(Game^players) - Game^provisionCost
		)
	),
	array.generate_foldl(Game^players, PayoffPred, Payoffs, list.reverse(Actions), _).



/**
 * The result of developing a Public Good Provision chromosome is itself.

 * <p> This function is part of type class {@code chromosome(strategy,strategy)}.
 *
 */
:- func born(parameters, strategy) = unit.

born(_, _) = unit.

/**
 * fold(Strategy, AC) = Result
  
 * Adds the strategy provide probability to the accumulator.  Updates the
 * number of strategies reduced so far.
 */

:- func fold(strategy, pgpAc) = pgpAc.

fold(Strategy, AC) = Result :-
	Result^qty = AC^qty + 1,
	(
		Strategy = prob(ProvideProbability),
		Result^prob = AC^prob + ProvideProbability
		;
		Strategy = dete(yes),
		Result^prob = AC^prob + 1.0
		;
		Strategy = dete(no),
		Result^prob = AC^prob
	).

/**
 * fold = Result
  
 * Returns the initial value of the accumulator used to reduce a collection
 * of PGP strategies.
 */

:- func fold = pgpAc.

fold = Result :-
	Result^qty = 0,
	Result^prob = 0.0.

:- pred writeAc(io.output_stream, pgpAc, io, io).
:- mode writeAc(in, in, di, uo) is det.

writeAc(Stream, Ac, !IO) :-
	(if
		Ac^qty = 0
	then
		io.print(Stream, "1/0", !IO)
	else
		io.print(Stream, Ac^prob / float(Ac^qty), !IO)
	).

:- end_module gl.pgp.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
