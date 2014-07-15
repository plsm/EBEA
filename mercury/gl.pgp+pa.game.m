/**
 * 

 * @author Pedro Mariano
 * @version 1.0 2013/12/12
 */
:- module gl.'pgp+pa'.game.

:- interface.

/**
 * The parameters of a Public Good Provision game with punishing and
 * abstaining are the number of players, the provision cost, the punishing
 * cost (for subject and performer) and the abstaining value..
 */
:- type game --->
	pgp_pa(
		players             :: int,
		provisionCost       :: float,
		lonerPayoff         :: float,
		punishPerformerCost :: float,
		punishSubjectCost   :: float
	).

:- instance parseable(game).

/**
 * Return a default Public Good Provision with Punishment and Abstaining
 * {@code game} that can be used to run games.
 */
:- func default = game.

/**
 * initGame(Players, LonerPayoff, ProvisionCost, PunishPerformerCost, PunishSubjectCost, Game)

 * Unify {@code Game} with a PGP with punishment and abstain game.  If the
 * parameters are outside their valid domain, the predicate fails.
 */
:- pred init(int, float, float, float, float, game).
:- mode init(in, in, in, in, in, out) is semidet.

:- func init(int, float, float, float, float) = game.

:- pred errors(game, string).
:- mode errors(in, out) is semidet.

/**
 * The lowest payoff obtained in the PGP game is obtained by the cooperator
 * when there is a single contributor to the good.

 * <p> Function of the type class {@code game(game,strategy)}.
 */

:- func lowestPayoff(game) = float.

/**
 * The highest payoff obtained in the PGP game is obtained by the defector
 * when all but one player contribute to the good.

 * <p> Function of the type class {@code game(game strategy)}.
 */
:- func highestPayoff(game) = float.

/**
 * The pareto payoff obtained in the PGP game is obtained when all players
 * contribute to the good.

 * <p> Function of the type class {@code game(game strategy)}.
 */
:- func paretoPayoff(game) = float.

/**
 * numberPlayers(Game) = Result
 
 * Return the value of attribute {@code players} of {@code Game}.

 * <p> Function of the type class {@code game(game strategy)}.
 */
:- func numberPlayers(game) = int.

:- func dialog = list(dialogItem(game)).

:- func players(game) = int.
:- func provisionCost(game) = float.
:- func lonerPayoff(game) =  float.
:- func punishPerformerCost(game) =  float.
:- func punishSubjectCost(game) = float.


:- func 'players :='(game, int) = game.
:- func 'provisionCost :='(game, float) = game.
:- func 'lonerPayoff :='(game,  float) = game.
:- func 'punishPerformerCost :='(game, float) =  game.
:- func 'punishSubjectCost :='(game, float) = game.

:- implementation.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

:- instance parseable(game) where
[
	pred(parse/3) is gl.'pgp+pa'.game.parse
].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

default = init(3, 0.2, 0.5, 0.2, 0.3).

init(Players, LonerPayoff, ProvisionCost, PunishPerformerCost, PunishSubjectCost, Game) :-
	Players > 0,
	ProvisionCost > 0.0,
	1.0 > ProvisionCost,
	1.0 - ProvisionCost < float(Players - 1) / float(Players),
	LonerPayoff > 0.0,
	LonerPayoff < 1.0 - ProvisionCost,
	PunishPerformerCost > 0.0,
	PunishSubjectCost > 0.0,
	Game = pgp_pa(Players, LonerPayoff, ProvisionCost, PunishPerformerCost, PunishSubjectCost).
	% Game^players = Players,
	% Game^lonerPayoff = LonerPayoff,
	% Game^provisionCost = ProvisionCost,
	% Game^punishPerformerCost = PunishPerformerCost,
	% Game^punishSubjectCost = PunishSubjectCost.

init(Players, LonerPayoff, ProvisionCost, PunishPerformerCost, PunishSubjectCost) =
	(if
		init(Players, LonerPayoff, ProvisionCost, PunishPerformerCost, PunishSubjectCost, Result)
	then
		Result
	else
		throw("gl.pgp+pa.game.init/5: invalid parameters")
	).

errors(Game, Msg) :-
	Msg1 =
	(if
		Game^players > 0
	then
		""
	else
		"Number of players must be greater than or equal to two"
	),
	Msg2 =
	(if
		Game^provisionCost > 0.0
	then
		""
	else
		"Provision cost must be positive"
	),
	Msg = Msg1 ++ Msg2,
	Msg \= "".

:- pragma memo(lowestPayoff/1).

lowestPayoff(Game) = Result :-
	Result = float.min(
		1.0 / float(Game^players) - Game^provisionCost, % payoff of single cooperator
		float.min(
			1.0 / float(Game^players) - Game^punishSubjectCost, % payoff of defector with single punisher
			1.0 / float(Game^players) - Game^provisionCost - Game^punishPerformerCost)).

:- pragma memo(highestPayoff/1).

highestPayoff(Game) = float(Game^players - 1) / float(Game^players).

:- pragma memo(paretoPayoff/1).

paretoPayoff(Game) = 1.0 - Game^provisionCost.

numberPlayers(Game) = Game^players.

dialog =
	[di(label("number of players"),      updateFieldInt(   players,  setPlayers)),
	 di(label("provision cost"),         updateFieldFloat( provisionCost,       setProvisionCost)),
	 di(label("loner payoff"),           updateFieldFloat( lonerPayoff,         setLonerPayoff)),
	 di(label("punish performer cost"),  updateFieldFloat( punishPerformerCost, checkFloat("punish performer cost", bounded(0.0, no), unbound, 'punishPerformerCost :='))),
	 di(label("punish subject cost"),    updateFieldFloat( punishPerformerCost, checkFloat("punish subject cost",   bounded(0.0, no), unbound, 'punishSubjectCost :=')))
	].
					 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- pred parse(game, list(int), list(int)).
:- mode parse(in, out, in) is det.
:- mode parse(out, in, out) is semidet.

parse(P) -->
	{P = pgp_pa(Players, LonerPayoff, ProvisionCost, PunishPerformerCost, PunishSubjectCost)},
	parseable.int8(Players),
	parseable.float32(LonerPayoff),
	parseable.float32(ProvisionCost),
	parseable.float32(PunishPerformerCost),
	parseable.float32(PunishSubjectCost)
	.

:- func check(setResult(game)) = setResult(game).

check(ok(Game)) =
	(if
		errors(Game, Msg)
	then
		error(Msg)
	else
		ok(Game)
	).

check(error(Msg)) = error(Msg).

:- func setPlayers(game, int) = setResult(game).

setPlayers(Game, Players) = Result :-
	checkInt("number of players", bounded(2, yes), unbound, 'players :=', Game, Players) = MGame,
	Result = check(MGame).

:- func setProvisionCost(game, float) = setResult(game).

setProvisionCost(Game, ProvisionCost) = Result :-
	checkFloat("provision cost", bounded(0.0, yes), bounded(1.0, yes), 'provisionCost :=', Game, ProvisionCost) = MGame,
	Result = check(MGame).

:- func setLonerPayoff(game, float) = setResult(game).

setLonerPayoff(Game, LonerPayoff) = Result :-
	checkFloat("loner payoff", bounded(0.0, no), bounded(1.0, no), 'lonerPayoff :=', Game, LonerPayoff) = MGame,
	Result = check(MGame).


:- end_module gl.'pgp+pa'.game.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
