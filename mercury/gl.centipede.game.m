/**
 * 

 * @author Pedro Mariano
 * @version 1.0 2013/12/11
 */
:- module gl.centipede.game.

:- interface.

:- import_module parseable.

/**
 * Represents the parameters of a Centipede game.
 * The game is characterised by the number of stages, the initial pot size,
 * the pot increase per stage and the pot share given to the player that
 * stops the game.

 * <p> To reduce parameters, the initial pot size is {@code 1}.
 */
:- type game --->
	centipede(
		numberStages   :: int,
		potShare       :: float,
		potIncrease    :: potIncrease
	).

:- type potIncrease --->
	arithmetic(float) ;
	geometric(float).

:- instance parseable(game).

%:- instance parseable(game).

/**
 * Return a default Centipede {@code game} that can be used to run games.
 */
:- func default = game.

/**
 * init(NumberStages, PotShare, PotIncrease)

 * Initialises a Centipede game with the given parameters.  If the
 * parameters are invalid an exception is thrown.

 * <p> The share of the pot the player that decides to continue or to stop
 * the game must be greater than {@code 0.5}.  The pot must increase by a
 * positive amount each stage.  Finally the payoff of a player that decides
 * to stop must be greater than the payoff he would obtain if he continued
 * and the partner decides to stop.  This last condition is expressed by
 * the following equation:

 * <math>
 * <apply>
 *  <eq/>
    <apply>
     <times/>
     <apply>
      <plus/>
      <cn>1</cn>
      <apply>
       <times/>
       <ci>t</ci>
       <ci>pi</ci>
      </apply>
     </apply>
    </apply>
   <apply>
  </apply>
 * </apply>
 * </math>
  
 */
:- func init(int, float, potIncrease) = game.

/**
 * init(NumberStages, PotShare, PotIncrease, Game)

 * Initialises a Centipede game with the given parameters.  The predicate
 * fails if the parameters are invalid.
  
 */
:- pred init(int, float, potIncrease, game).
:- mode init(in, in, in, out) is semidet.

/**
 * The lowest payoff obtained in the Centipede game is the pot share
 * obtained by the second player in the game ends in the first stage.

 * <p> Function of the type class {@code game(game,strategy)}.
 */

:- func lowestPayoff(game) = float.

/**
 * highestPayoff(Game) = Result

 * The highest payoff obtained in the Centipede game is the pot share
 * obtained by the partner of the player to move in the last stage.  If
 * this player continues, the pot increases again and his partner receives
 * the larger share.

 * <p> Function of the type class {@code game(game, strategy)}.
 */
:- func highestPayoff(game) = float.


/**
 * paretoPayoff(Game) = Result

 * The Pareto payoff obtained in the Centipede game is the payoff obtained
 * by the player that stops in the last past one stage.

 * <p> Function of the type class {@code game(game,strategy)}.
 */
:- func paretoPayoff(game) = float.

/**
 * numberPlayers(Game) = Result
 
 * Returns two.  Centipede is a 2-player game.

 * <p> Function of the type class {@code game(game,strategy)}.
 */
:- func numberPlayers(game) = int.

/**
 * initialPotSize = Result
  
 * The initial size of the pot is set to 1.
  
 * @returns {@code 1}.
 */
:- func initialPotSize = float.

/**
 * Return a user dialog to edit a centipede game parameters.
 */
:- func dialog = list(dialogItem(game)).

/**
 * Return the pot size at the given stage.
 */
:- func potSize(game, int) = float.

:- func numberStages(game) = int.

:- func 'numberStages :='(game, int) = game.

:- func potShare(game) = float.

:- func 'potShare :='(game, float) = game.

:- pred statusResetMemoTables(io.state, io.state).
:- mode statusResetMemoTables(di, uo) is det.

% :- pred parse(game, list(int), list(int)).
% :- mode parse(in, out, in) is det.
% :- mode parse(out, in, out) is semidet.

:- implementation.

:- import_module bool.
:- import_module table_statistics.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

:- instance parseable(game) where
[
	pred(parse/3) is gl.centipede.game.parse
].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

default = init(4, 0.6, geometric(1.1)).

init(NumberStages, PotShare, PotIncrease) = Result :-
	(if
		init(NumberStages, PotShare, PotIncrease, Game)
	then
		Result = Game
	else
		throw("invalid centipede game parameters")
	).

init(NumberStages, PotShare, PotIncrease, Game) :-
	NumberStages > 1,
	PotShare > 0.5,
	(
		PotIncrease = arithmetic(Increase),
		Increase > 0.0,
		(initialPotSize + Increase) * (1.0 - PotShare) - initialPotSize * PotShare < 0.0
		;
		PotIncrease = geometric(Ratio),
		Ratio > 1.0,
		PotShare > Ratio * (1.0 - PotShare)
	),
	Game = centipede(NumberStages, PotShare, PotIncrease).

initialPotSize = 1.0.

dialog =
	[
	di(label("number of stages"),   updateFieldInt(    numberStages,  userInterface.checkInt("number of stages", bounded(2, yes), unbound, 'numberStages :=') )),
	di(label("pot share"),          updateFieldFloat(  potShare,      setPotShare)),
	di(label("geometric"),          updateData( changePotIncrease(yes))),
	di(label("arithmetic"),         updateData( changePotIncrease(no))),
	di(label("factor"),             updateFieldFloat(  getFactor,     setFactor))
	].
					 

                                 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                                 % type class
                                 % {@code game(game, strategy)}
                                 % predicates and functions

:- pragma memo(lowestPayoff/1, [allow_reset, statistics]).

lowestPayoff(_Game) = 0.0.
%lowestPayoff(Game) = Result :-
%	Result = initialPotSize * (1.0 - Game^potShare).

:- pragma memo(highestPayoff/1, [allow_reset, statistics]).

highestPayoff(Game) = Result :-
	Result = potSize(Game, Game^numberStages) * Game^potShare.

:- pragma memo(paretoPayoff/1, [allow_reset, statistics]).

paretoPayoff(Game) = Result :-
	(if
		Game^numberStages > 1
	then
		Result = potSize(Game, Game^numberStages - 1) * Game^potShare
	else
		Result = potSize(Game, 1) * Game^potShare
	).

numberPlayers(_Game) = 2.

:- pragma memo(potSize/2, [allow_reset, statistics]).

potSize(Game, Stage) = Result :-
	(if
		Stage = 1
	then
		Result = initialPotSize
	else
		Game^potIncrease = arithmetic(Delta),
		Result = potSize(Game, Stage - 1) + Delta
		;
		Game^potIncrease = geometric(Ratio),
		Result = potSize(Game, Stage - 1) * Ratio
	).

statusResetMemoTables(!IO) :-
	% Pred =
	% (pred(PredStr::in, Status::in(pred(out,di,uo) is det), Reset::in(pred(di,uo) is det), IOdi::di, IOuo::uo) is det :-
	% 	io.format("\n* ** %s ** *\n", [s(PredStr)], IOdi, IO1),
	% 	Status(Stat, IO1, IO2),
	% 	table_statistics.write_table_stats(Stat^call_table_stats^current_stats, IO2, IO3),
	% 	Reset(IO3, IOuo)
	% ),
	% io.print("   *************************\n* ** CENTIPEDE MEMO TABLES ** *\n   *************************\n", !IO),
	% Pred("potSize/2", table_statistics_for_potSize_2, table_reset_for_potSize_2, !IO),
	% Pred("lowestPayoff/1", table_statistics_for_lowestPayoff_1, table_reset_for_lowestPayoff_1, !IO),
	% Pred("highestPayoff/1", table_statistics_for_highestPayoff_1, table_reset_for_highestPayoff_1, !IO),
	% Pred("paretoPayoff/1",  table_statistics_for_paretoPayoff_1, table_reset_for_paretoPayoff_1, !IO),
	true
	.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions


:- pred parse(game, list(int), list(int)).
:- mode parse(in, out, in) is det.
:- mode parse(out, in, out) is semidet.

parse(Game) -->
	{Game = centipede(NumberStages, PotShare, PotIncrease)},
	[NumberStages],
	parseable.float32(PotShare),
	(
		{PotIncrease = arithmetic(Float)},
		[0],
		parseable.float32(Float)
		;
		{PotIncrease = geometric(Float)},
		[1],
		parseable.float32(Float)
	).

:- func setNumberStages(game, int) = setResult(game).

setNumberStages(Game, NumberStages) = userInterface.checkInt("number of stages", bounded(2, yes), unbound, 'numberStages :=', Game, NumberStages).

:- func setPotShare(game, float) = setResult(game).

setPotShare(Game, PotShare) = Result :-
	(if
		init(Game^numberStages, PotShare, Game^potIncrease, R)
	then
		Result = ok(R)
	else
		Result = error("game has invalid parameters")
	).

:- func getFactor(game) = float.

getFactor(centipede(_, _, arithmetic(Result))) = Result.
getFactor(centipede(_, _, geometric(Result))) = Result.

:- func setFactor(game, float) = setResult(game).

setFactor(Game, Factor) = Result :-
	(if
		Game^potIncrease = arithmetic(_),
		init(Game^numberStages, Game^potShare, arithmetic(Factor), R)
		;
		Game^potIncrease = geometric(_),
		init(Game^numberStages, Game^potShare, geometric(Factor), R)
	then
		Result = ok(R)
	else
		Result = error("game has invalid parameters")
	).


:- func changePotIncrease(bool, game) = game.

changePotIncrease(no, Game) = centipede(Game^numberStages, Game^potShare, geometric(1.1)).
changePotIncrease(yes, Game) = centipede(Game^numberStages, Game^potShare, arithmetic(1.1)).


:- end_module gl.centipede.game.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
