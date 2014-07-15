/**
 * Base module of the implementation of the Give-And-Take game.

 * @author Pedro Mariano
 * @version 1.0 2013/12/ 5
 * @version 2.0 2013/12/30
 */
:- module gl.givetake.

:- interface.

:- include_module game, strategy, parameters.
:- import_module bool.
:- import_module gl.givetake.game, gl.givetake.strategy, gl.givetake.parameters.

:- instance chromosome(strategy, unit, parameters).

:- instance abstractGame(game).
:- instance symmetricGame(game, strategy).
:- instance asymmetricGame(game, strategy).

:- implementation.

:- import_module exception, maybe.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

:- instance abstractGame(game) where
[
	func(lowestPayoff/1)  is gl.givetake.game.lowestPayoff,
	func(highestPayoff/1) is gl.givetake.game.highestPayoff,
	func(paretoPayoff/1)  is gl.givetake.game.paretoPayoff,
	func(numberPlayers/1) is gl.givetake.game.numberPlayers
].

:- instance symmetricGame(game, strategy) where
[
	pred(playSymmetric/5) is gl.givetake.play
].

:- instance asymmetricGame(game, strategy) where
[
	func(numberRoles/1)    is game.singleRole,
	pred(playAsymmetric/5) is game.playSymmetricBridge
].

:- instance chromosome(strategy, unit, parameters) where
[
	func(numberGenes/1) is gl.givetake.strategy.numberParameters,
	pred(mutateGene/8)  is gl.givetake.strategy.mutateGene,
	func(born/2)        is gl.simpleBorn
].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types


/**
 * Represents the two roles in Give-Take.
 */
:- type player --->
	fst ;
	snd.

/**
  * Provides the actions available in a Give-Take game stage.
  */
:- type action --->
	noneg ;
	give ;
	nonet ;
	take.

/**
 * Represents the possible combinations of actions in each stage of the
 * Give-Take game.
 */

:- type actions --->
	give_take ;
	give_nonet ;
	noneg_take ;
	noneg_nonet ;
	take_give ;
	nonet_give ;
	take_noneg ;
	nonet_noneg.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- func partner(player) = player.

partner(fst) = snd.
partner(snd) = fst.

:- pred bool(player, bool).
:- mode bool(in, out) is det.
:- mode bool(out, in) is det.

bool(fst, yes).
bool(snd, no).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% type class {@code game(game,strategy)} predicates and functions

/**
 * play(Game, Profile, !Random, Payoffs)
  
 * Compute the game using the given player profile and return the
 * strategies' payoff.

 * @param Random The pseudo-random number generator used to calculate
 * players' cake division and division acceptance.
 */

:- pred play(game, array(strategy), R, R, array(float))
	<= ePRNG(R).
:- mode play(in, in, in, out, out) is det.

play(Game, Profile, !Random, Payoffs) :-
	Game = game(_, _, _, NumberStages),
	NumberStages = computeClass,
	Strategy1 = array.lookup(Profile, 0),
	Strategy2 = array.lookup(Profile, 1),
	(if
		Strategy1 = time(TG1, TT1),
		Strategy2 = time(TG2, TT2)
	then
		Player1TimeWithResource    = int.min(TG1, TT2),
		Player1TimeWithoutResource = int.min(TT1, TG2),
		Payoff1 =
			(+ float(Player1TimeWithResource)
			+ (if Player1TimeWithResource = TG1 then Game^bg else 0.0)
			- (if Player1TimeWithResource = TT2 then Game^cst else 0.0)
			- (if Player1TimeWithoutResource = TT1 then Game^cpt else 0.0)
			) / float(Player1TimeWithResource + Player1TimeWithoutResource),
		Payoff2 =
			(+ float(Player1TimeWithoutResource)
			+ (if Player1TimeWithoutResource = TG2 then Game^bg else 0.0)
			- (if Player1TimeWithoutResource = TT1 then Game^cst else 0.0)
			- (if Player1TimeWithoutResource = TT2 then Game^cpt else 0.0)
			) / float(Player1TimeWithResource + Player1TimeWithoutResource),
		Payoffs = array.from_list([Payoff1, Payoff2])
	else
		throw("Not implemented")
	).


/**
 * action(Strategy, WhoIAm, PlayerWithResource, MLastActions, ResourceExchange) = Result

 * Return the action performed by a strategy given its role (either first
 * or second) who has the resource (either first or second), the actions
 * performed in the last stage (if applicable) and when occurred the last
 * resource exchanged (in number of stages).
  
 */
:- func action(givetake.strategy.strategy, player, player, maybe(actions), int) = action.

action(history(GT, GN, NT, NNy, TG, NG, TN, NNn, FG, FT), WhoIAm, PlayerWithResource, MLastActions, _ResourceExchange) = Result :-
	MLastActions = yes(LastActions),
	lookup(GT, GN, NT, NNy, TG, NG, TN, NNn, WhoIAm, LastActions) = Result
	;
	MLastActions = no,
	(if
		PlayerWithResource = WhoIAm
	then
		FG = yes,
		Result = give
		;
		FG = no,
		Result = noneg
	else
		FT = yes,
		Result = take
		;
		FT = no,
		Result = nonet
	).

action(time(TG, TT), WhoIAm, PlayerWithResource, _MLastActions, ResourceExchange) = Result :-
	(if
		WhoIAm = PlayerWithResource
	then
		(if
			ResourceExchange >= TG
		then
			Result = give
		else
			Result = noneg
		)
	else
		(if
			ResourceExchange >= TT
		then
			Result = take
		else
			Result = nonet
		)
	).

/**
 * lookup(GT, GN, NT, NNy, TG, NG, TN, NNn, WhoIAm, LastActions) = Result
 
 * Return the action performed by a history strategy given the actions
 * played in the previous stage.
 */

:- func lookup(bool, bool, bool, bool, bool, bool, bool, bool, player, actions) = action.

lookup(yes, _GN, _NT, _NNy, _TG, _NG, _TN, _NNn, fst, give_take)   = take.
lookup(no,  _GN, _NT, _NNy, _TG, _NG, _TN, _NNn, fst, give_take)   = nonet.
lookup(_GT, yes, _NT, _NNy, _TG, _NG, _TN, _NNn, fst, give_nonet)  = take.
lookup(_GT, no,  _NT, _NNy, _TG, _NG, _TN, _NNn, fst, give_nonet)  = nonet.
lookup(_GT, _GN, yes, _NNy, _TG, _NG, _TN, _NNn, fst, noneg_take)  = take.
lookup(_GT, _GN, no,  _NNy, _TG, _NG, _TN, _NNn, fst, noneg_take)  = nonet.
lookup(_GT, _GN, _NT, yes,  _TG, _NG, _TN, _NNn, fst, noneg_nonet) = give.
lookup(_GT, _GN, _NT, no,   _TG, _NG, _TN, _NNn, fst, noneg_nonet) = noneg.

lookup(_GT, _GN, _NT, _NNy, yes, _NG, _TN, _NNn, fst, take_give)   = give.
lookup(_GT, _GN, _NT, _NNy, no,  _NG, _TN, _NNn, fst, take_give)   = noneg.
lookup(_GT, _GN, _NT, _NNy, _TG, yes, _TN, _NNn, fst, nonet_give)  = give.
lookup(_GT, _GN, _NT, _NNy, _TG, no,  _TN, _NNn, fst, nonet_give)  = noneg.
lookup(_GT, _GN, _NT, _NNy, _TG, _NG, yes, _NNn, fst, take_noneg)  = give.
lookup(_GT, _GN, _NT, _NNy, _TG, _NG, no,  _NNn, fst, take_noneg)  = noneg.
lookup(_GT, _GN, _NT, _NNy, _TG, _NG, _TN, yes,  fst, nonet_noneg) = take.
lookup(_GT, _GN, _NT, _NNy, _TG, _NG, _TN, no,   fst, nonet_noneg) = nonet.

lookup(_GT, _GN, _NT, _NNy, yes, _NG, _TN, _NNn, snd, give_take)   = give.
lookup(_GT, _GN, _NT, _NNy, no,  _NG, _TN, _NNn, snd, give_take)   = noneg.
lookup(_GT, _GN, _NT, _NNy, _TG, yes, _TN, _NNn, snd, give_nonet)  = give.
lookup(_GT, _GN, _NT, _NNy, _TG, no,  _TN, _NNn, snd, give_nonet)  = noneg.
lookup(_GT, _GN, _NT, _NNy, _TG, _NG, yes, _NNn, snd, noneg_take)  = give.
lookup(_GT, _GN, _NT, _NNy, _TG, _NG, no,  _NNn, snd, noneg_take)  = noneg.
lookup(_GT, _GN, _NT, _NNy, _TG, _NG, _TN, yes,  snd, noneg_nonet) = take.
lookup(_GT, _GN, _NT, _NNy, _TG, _NG, _TN, no,   snd, noneg_nonet) = nonet.

lookup(yes, _GN, _NT, _NNy, _TG, _NG, _TN, _NNn, snd, take_give)   = take.
lookup(no,  _GN, _NT, _NNy, _TG, _NG, _TN, _NNn, snd, take_give)   = nonet.
lookup(_GT, yes, _NT, _NNy, _TG, _NG, _TN, _NNn, snd, nonet_give)  = take.
lookup(_GT, no,  _NT, _NNy, _TG, _NG, _TN, _NNn, snd, nonet_give)  = nonet.
lookup(_GT, _GN, yes, _NNy, _TG, _NG, _TN, _NNn, snd, take_noneg)  = take.
lookup(_GT, _GN, no,  _NNy, _TG, _NG, _TN, _NNn, snd, take_noneg)  = nonet.
lookup(_GT, _GN, _NT, yes,  _TG, _NG, _TN, _NNn, snd, nonet_noneg) = give.
lookup(_GT, _GN, _NT, no,   _TG, _NG, _TN, _NNn, snd, nonet_noneg) = noneg.

	
:- end_module gl.givetake.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
