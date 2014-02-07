/**
 * Provides an implementation of the Public Good Provision game with
 * punishing and abstaining options.  The good value is one, while
 * provision cost is a value lower than one.  Punishing must be less than
 * one.

 * <p> The parameters of a Public Good Provision game with Punishing and
 * Abstaining are the number of players, the provision cost, the punishing
 * cost (for subject and performer) and the abstaining value.  This is the
 * order predicate {@code readGame} expects the parameters to be in the text
 * stream. The good is worth one unit.

 * @author Pedro Mariano
 * @version 1.0 2012/06/25
 * @version 2.0 2013/12/30
 */
:- module gl.'pgp+pa'.

:- interface.

:- include_module game, strategy, parameters, factory.
:- import_module gl.'pgp+pa'.game, gl.'pgp+pa'.strategy,  gl.'pgp+pa'.parameters, gl.'pgp+pa'.factory.

:- import_module unit.


/**
 * The accumulator used to reduce a collection of PGP with punishing and abstaining strategies.
 */

:- type ac.

:- instance game(game, strategy).

:- instance chromosome(strategy, unit, parameters).

:- instance foldable(strategy, ac).

:- instance factory(factory, game, strategy, parameters).

:- instance printable(ac).

:- implementation.

:- import_module rng, rng.distribution.
:- import_module array, bool, exception, float, int, list, maybe, solutions, string.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types


:- type ac --->
	ac(lonerQty::int,
		defectorQty::int,
		cooperatorQty::int,
		punisherQty::int
	  ).

:- instance game(game, strategy) where
[
	func(lowestPayoff/1)  is gl.'pgp+pa'.game.lowestPayoff,
	func(highestPayoff/1) is gl.'pgp+pa'.game.highestPayoff,
	func(paretoPayoff/1)  is gl.'pgp+pa'.game.paretoPayoff,
	func(numberPlayers/1) is gl.'pgp+pa'.game.numberPlayers,
	pred(play/5)          is gl.'pgp+pa'.play
].

:- instance chromosome(strategy, unit, parameters) where
[
	func(numberGenes/1) is gl.'pgp+pa'.strategy.numberParameters,
	pred(mutateGene/8)  is gl.'pgp+pa'.strategy.mutateGene,
	func(born/2)        is gl.'pgp+pa'.born
].

:- instance foldable(strategy, ac) where
[
	func(fold/2) is gl.'pgp+pa'.fold,
	func(initAC/0) is gl.'pgp+pa'.fold
].

:- instance factory(factory, game, strategy, parameters) where
[
	pred(value/5) is gl.'pgp+pa'.factory.value
].

:- instance printable(ac)
	where
[
	pred(print/4) is gl.'pgp+pa'.writeAc
].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types


:- type action --->
	abstain ;
	participate(cooperate::bool, punish::bool).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions
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
	(pred(Strategy::in,
		ActionsIn::in, ActionsOut::out,
		GroupSizeIn::in, GroupSizeOut::out,
		NumberContributorsIn::in, NumberContributorsOut::out,
		DefectorsIn::in, DefectorsOut::out,
		PunishersIn::in, PunishersOut::out
		) is det :-
		Strategy = pure(Type),
		(
			Type = loner,
			ActionsOut = [abstain | ActionsIn],
			GroupSizeIn = GroupSizeOut,
			NumberContributorsIn = NumberContributorsOut,
			DefectorsIn = DefectorsOut,
			PunishersIn = PunishersOut
			;
			Type = defector,
			ActionsOut = [participate(no, no) | ActionsIn],
			GroupSizeIn + 1.0 = GroupSizeOut,
			NumberContributorsIn = NumberContributorsOut,
			DefectorsOut = 1.0,
			PunishersIn = PunishersOut
			;
			Type = cooperator,
			ActionsOut = [participate(yes, no) | ActionsIn],
			GroupSizeIn + 1.0 = GroupSizeOut,
			NumberContributorsIn + 1.0 = NumberContributorsOut,
			DefectorsIn = DefectorsOut,
			PunishersIn = PunishersOut
			;
			Type = punisher,
			ActionsOut = [participate(yes, yes) | ActionsIn],
			GroupSizeIn + 1.0 = GroupSizeOut,
			NumberContributorsIn + 1.0 = NumberContributorsOut,
			DefectorsIn = DefectorsOut,
			PunishersOut = 1.0
		)
	),
	array.foldl5(ContributePred, Profile,
		[], Actions,
		0.0, GroupSize,
		0.0, NumberContributors,
		0.0, Defectors,
		0.0, Punishers),
	% compute players' payoff based on their action and number of contributors
	PayoffPred =
	(pred(_Index::in, Payoff::out, ActionsIn::in, ActionsOut::out) is det :-
		ActionsIn = [],
		throw("gl.'pgp+pa'.play/5: Not reachable")
		;
		ActionsIn = [Action | ActionsOut],
		(if
			GroupSize =< 1.0
		then
			Payoff = Game^lonerPayoff
		else
			Action = abstain,
			Payoff = Game^lonerPayoff
			;
			Action = participate(no, no),
			Payoff = NumberContributors / GroupSize - Punishers * Game^punishSubjectCost
			;
			Action = participate(yes, no),
			Payoff = NumberContributors / GroupSize - Game^provisionCost
			;
			Action = participate(yes, yes),
			Payoff = NumberContributors / GroupSize - Game^provisionCost - Defectors * Game^punishPerformerCost
			;
			Action = participate(no, yes),
			Payoff = NumberContributors / GroupSize - Game^punishSubjectCost - Game^punishPerformerCost
		)
	),
	array.generate_foldl(Game^players, PayoffPred, Payoffs, list.reverse(Actions), Check),
	(if
		Check = []
	then
		true
	else
		throw("gl.'pgp+pa'.play/5: Problem calculating the players' payoff")
	).


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

:- func fold(strategy, ac) = ac.

fold(Strategy, AC) = Result :-
	Strategy = pure(Type),
	(
		Type = loner,
		Result = 'lonerQty :='(AC, AC^lonerQty + 1)
		;
		Type = defector,
		Result = 'defectorQty :='(AC, AC^defectorQty + 1)
		;
		Type = cooperator,
		Result = 'cooperatorQty :='(AC, AC^cooperatorQty + 1)
		;
		Type = punisher,
		Result = 'punisherQty :='(AC, AC^punisherQty + 1)
	).

/**
 * fold = Result
  
 * Returns the initial value of the accumulator used to reduce a collection
 * of PGP strategies.
 */

:- func fold = ac.

fold = ac(0, 0, 0, 0).

:- pred writeAc(io.output_stream, ac, io, io).
:- mode writeAc(in, in, di, uo) is det.

writeAc(Stream, Ac, !IO) :-
	io.print(Stream, Ac^lonerQty, !IO),
	io.print(Stream, " ", !IO),
	io.print(Stream, Ac^defectorQty, !IO),
	io.print(Stream, " ", !IO),
	io.print(Stream, Ac^cooperatorQty, !IO),
	io.print(Stream, " ", !IO),
	io.print(Stream, Ac^punisherQty, !IO).

% utility functions and predicates

:- pred printParseErrors(string, io, io).
:- mode printParseErrors(in, di, uo) is det.

printParseErrors(Line, !IO) :-
	(if
		string.words(Line) = [SPlayers, SLonerPayoff, SProvisionCost, SPunishPerformerCost, SPunishSubjectCost]
	then
		% handle parsing errors
		(if
			string.to_int(SPlayers, Players1),
			Players1 =< 0
		then
			io.print(io.stderr_stream, "The number of players must be a positive integer\n", !IO)
		else if
			not string.to_int(SPlayers, _)
		then
			io.print(io.stderr_stream, "The number of players is not an integer\n", !IO)
		else
			true
		),
		(if
			string.to_float(SProvisionCost, ProvisionCost1),
			(
				ProvisionCost1 =< 0.0
				;
				ProvisionCost1 >= 1.0
			)
		then
			io.print(io.stderr_stream, "The provision cost must be a floating point number between 0.0 and 1.0 (exclusive)\n", !IO)
		else if
			not string.to_float(SProvisionCost, _)
		then
			io.print(io.stderr_stream, "The provision cost is not a floating point number\n", !IO)
		else
			true
		),
		(if
			string.to_int(SPlayers, Players2),
			string.to_float(SProvisionCost, ProvisionCost2),
			Players2 > 0,
			ProvisionCost2 > 0.0,
			1.0 - ProvisionCost2 >= float(Players2 - 1) / float(Players2)
		then
			io.print(io.stderr_stream, "There is no dillema\n", !IO)
		else
			true
		),
		(if
			string.to_float(SLonerPayoff, LonerPayoff1),
			string.to_float(SProvisionCost, ProvisionCost3),
			(
				LonerPayoff1 =< 0.0
				;
				LonerPayoff1 >= 1.0 - ProvisionCost3
			)
		then
			io.print(io.stderr_stream, "Loner payoff must be greater than zero and lower than one minus provision cost\n", !IO)
		else if
			not string.to_float(SLonerPayoff, _)
		then
			io.print(io.stderr_stream, "Loner payoff is not a floating point number\n", !IO)
		else
			true
		),
		(if
			string.to_float(SPunishPerformerCost, PunishPerformerCost),
			PunishPerformerCost =< 0.0
		then
			io.print(io.stderr_stream, "Punish performer cost must be a positive floating point number\n", !IO)
		else if
			not string.to_float(SPunishPerformerCost, _)
		then
			io.print(io.stderr_stream, "Punish performer cost is not a positive floating point number\n", !IO)
		else
			true
		),
		(if
			string.to_float(SPunishSubjectCost, PunishSubjectCost),
			PunishSubjectCost =< 0.0
		then
			io.print(io.stderr_stream, "Punish subject cost must be a positive floating point number\n", !IO)
		else if
			not string.to_float(SPunishSubjectCost, _)
		then
			io.print(io.stderr_stream, "Punish subject cost is not a positive floating point number\n", !IO)
		else
			true
		)
	else
		io.print(io.stderr_stream, "Expecting five values: number of players, loner payoff, provision cost, punish performer cost and punish subject cost\n", !IO)
	).

:- end_module gl.'pgp+pa'.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
