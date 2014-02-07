/**
 * One of the functionalities of the Energy Based Evolutionary Algorithm
 * Toolkit is to analyse games.  The user can enter game and strategy
 * parameters in order to obtain calculate the payoffs.

 * @author Pedro Mariano
 * @version 1.0 2013/05/17
 */
:- module tool.analyseGame.

:- interface.

:- import_module io.

:- pred go(io.state, io.state).
:- mode go(di, uo) is det.

:- implementation.

:- import_module rng, scanable.
:- import_module menu.
:- import_module my, my.random.
:- import_module array, bool, int, list, map, maybe, random, solutions, string, unit.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

/**
 * The state that is passed to predicate {@code printMenu} of module {@code
 * menu}.  The user can play games that may require a pseudo-random number
 * generator.  Playing games require the current game parameters and
 * strategy parameters.   The user can change these parameters.
  
 */
:- type state --->
	state(
		random   :: my.random.supply,
		gameData :: map(tool.games, tool.gameData)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

go(!IO) :-
	menu.printMenu(initMenu, initState, _, !IO).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- func initState = tool.analyseGame.state.

initState = State :-
	State = state('new supply'(Random), GameData),
	random.init(11, Random),
	PredGenerate =
	(pred({K, GD}::out) is multi :-
		tool.gameData(_, _, K, GD)
	),
	PredAssemble =
	(pred({K, GD}::in, MapIn::in, MapOut::out) is det :-
		map.det_insert(MapIn, K, GD) = MapOut
	),
	solutions.aggregate(
		PredGenerate,
		PredAssemble,
		map.init, GameData
	).

:- func initMenu = menu(tool.analyseGame.state).
:- mode initMenu = out(menu.menuSkel).

initMenu = Result :-
	solutions.solutions(gameMenuItem, GameMenuItems),
	Result = list.append(
		GameMenuItems,
		[menuItem("change RNG", predicate2(changeSupply))]
	).

/**
 * List of menus to show game information, change the game parameters, the
 * overall parameters, print a collection of mutations, play the game.
 */

:- pred gameMenuItem(item(tool.analyseGame.state)).
:- mode gameMenuItem(out(menu.itemSkel)) is multi.

gameMenuItem(MenuItem) :-
	tool.gameData(_, _, Key, _),
	MenuItem = gameMenuItem(Key).

:- func gameMenuItem(tool.games) = item(tool.analyseGame.state).
:- mode gameMenuItem(in) = out(menu.itemSkel).

gameMenuItem(Key) = Result :-
	tool.gameData(_, Label, Key, _),
	Result = menuItem(
		Label,
		submenu(
			[menuItem("information", predicate2(gameInformation(Key))),
			 menuItem("play", predicate2(playGame(Key))),
			 menuItem("change game parameters", predicate2(changeGameParameters(Key)))]
		)
	).

:- pred playGame(tool.games, tool.analyseGame.state, tool.analyseGame.state, io.state, io.state).
:- mode playGame(in, in, out, di, uo) is det.

playGame(Key, !State, !IO) :-
	!.State^random = supply(ThisSupply),
	map.lookup(!.State^gameData, Key, GD),
	GD = gd(GameData),
	(if
		list.length(GameData^strategy) < numberPlayers(GameData^game)
	then
		GameData^strategy = [Repeat | _],
		Remaining = list.duplicate(numberPlayers(GameData^game) - list.length(GameData^strategy), Repeat),
		ProfileAsList = list.append(GameData^strategy, Remaining),
		io.format("Duplicating strategy %s...\n", [s(string(Repeat))], !IO)
		;
		GameData^strategy = [],
		ProfileAsList = [],
		io.print("Not enough players\n", !IO)
	else
		GameData^strategy = ProfileAsList
	),
	(
		ProfileAsList = []
		;
		ProfileAsList = [_ | _],
		StrategyProfile = array.array(ProfileAsList),
		game.play(GameData^game, StrategyProfile, ThisSupply, NextSupply, Payoffs),
		io.print("Payoffs:\n", !IO),
		PrintPred =
		(pred(Strategy::in, Payoff::in, IOdi::di, IOuo::uo) is det :-
			io.format("%30s => %+10.7f\n", [s(string(Strategy)), f(Payoff)], IOdi, IOuo)
		),
		list.foldl_corresponding(PrintPred, ProfileAsList, array.to_list(Payoffs), !IO),
		io.nl(!IO),
		!:State = state('new supply'(NextSupply), !.State^gameData)
	).

:- pred changeGameParameters(tool.games, tool.analyseGame.state, tool.analyseGame.state, io.state, io.state).
:- mode changeGameParameters(in, in, out, di, uo) is det.

changeGameParameters(Key, !State, !IO) :-
	map.lookup(!.State^gameData, Key, GD),
	GD = gd(GameData),
	scanable.scan(io.stdin_stream, IMGame, !IO),
	(if
		IMGame = ok(ok(Game))
	then
		NewGameData = 'game :='(GameData, Game),
		NewGD = 'new gd'(NewGameData),
		map.det_update(!.State^gameData, Key, NewGD) = NewStateGameData,
		!:State = state(!.State^random, NewStateGameData)
	else
		io.print("" + IMGame, !IO),
		io.print("\nEnter new parameters:\n", !IO),
		changeGameParameters(Key, !State, !IO)
	).

:- pred gameInformation(tool.games, tool.analyseGame.state, tool.analyseGame.state, io.state, io.state).
:- mode gameInformation(in, in, out, di, uo) is det.

gameInformation(Key, !State, !IO) :-
	map.lookup(!.State^gameData, Key, GD),
	GD = gd(GameData),
	GameData^game = Game,
	io.print("Game value:\n", !IO),
	io.print(Game, !IO),
	io.nl(!IO),
	io.format(
		"Lowest payoff:  %+10.7f\nHighest payoff: %+10.7f\nPareto payoff:  %+10.7f\n",
		[f(game.lowestPayoff(Game)),
		 f(game.highestPayoff(Game)),
		 f(game.paretoPayoff(Game))],
		!IO).

:- pred calculateMutations(tool.games, tool.analyseGame.state, tool.analyseGame.state, io.state, io.state).
:- mode calculateMutations(in, in, out, di, uo) is det.

calculateMutations(Key, !State, !IO) :-
	io.print("How many mutants? ", !IO),
	scanable.scanInt(stdin_stream, yes, "number of mutants", bounded(1, yes), unbound, IMNumber, !IO),
	(if
		IMNumber = ok(ok(Number))
	then
		map.lookup(!.State^gameData, Key, GD),
		GD = gd(GameData),
		true
	else
		io.print("" + IMNumber, !IO),
		io.nl(!IO),
		calculateMutations(Key, !State, !IO)
	).

/**
 * Menu option to change the current pseudo-random number generator.
 */
:- pred changeSupply(tool.analyseGame.state, tool.analyseGame.state, io.state, io.state).
:- mode changeSupply(in, out, di, uo) is det.

changeSupply(!State, !IO) :-
	io.print("Enter pseudo-random number generator parameters:\n", !IO),
	my.random.scan(io.stdin_stream, IMSupply, !IO),
	(if
		IMSupply = ok(ok(Supply))
	then
		!:State = 'random :='(!.State, Supply)
	else
		io.print("" + IMSupply, !IO),
		changeSupply(!State, !IO)
	).



:- end_module tool.analyseGame.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
