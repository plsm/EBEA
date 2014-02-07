/**
 * 

 * @author Pedro Mariano
 * @version 1.0 2013/05/24
 */
:- module file.config.

:- interface.

:- import_module ebea, ebea.player, ebea.population, ebea.streams.
:- import_module game, chromosome.
:- import_module rng, foldable, printable, scanable.
:- import_module io, list, maybe.

/**
 * Represents the data in a configuration file needed to perform a run of
 * the Energy Based Evolutionary Algorithm.
  
 */
:- type config(R, G, CS, P) --->
	config(
		random            :: R,
		numberRuns        :: int,
		game              :: G,
		parameters        :: ebea.population.parameters(P),
		initialPopulation :: list({int, ebea.player.chromosome(CS)}),
		level             :: ebea.streams.level
	).

:- type config --->
	some [R, G, CS, P, T, A]
	(cfg(config(R, G, CS, P))
	=> (
		ePRNG(R),
		game(G, CS),
		chromosome(CS, T, P),
		foldable(CS, A),
		printable(CS),
		printable(T),
		printable(A))
	).

:- pred scan(maybe(string), scanable.result(config), io, io).
:- mode scan(in, out, di, uo) is det.

:- implementation.

:- import_module ebea, ebea.core, ebea.streams, ebea.player, ebea.population.
:- import_module gl, gl.'2x2', gl.centipede, gl.investment, gl.pgp, gl.'pgp+pa', gl.ultimatum.
:- import_module tool.
:- import_module my, my.random.
:- import_module rng, rng.distribution.
:- import_module util.
:- import_module bool, string.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

:- type config1(R) --->
	state1(R) ;
	state2(R, int).

:- type config2(R, G) --->
	state3(R, int, scanable.result(G)) ;
	state4(R, int, G).

:- type config3(R, G, P) --->
	state5(R, int, G, P) ;
	state6(R, int, G, ebea.population.parameters(P)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

scan(MFilename, IMConfig, !IO) :-
	(
		MFilename = yes(Filename)
		;
		MFilename = no,
		Filename = "config.txt"
	),
	io.open_input(Filename, RStream, !IO),
	(
		RStream = ok(Stream),
		my.random.scan(Stream, IMSupply, !IO),
		(if
			IMSupply = ok(ok(Supply))
		then
			Supply = supply(Random),
			scanFile1(state1(Random), Stream, IMConfig, !IO)
		else
			IMConfig = scanable.noOkToIOresultMaybeError(IMSupply)
		),
		io.close_input(Stream, !IO)
		;
		RStream = error(Error),
		IMConfig = ok(error(string.format("Error opening file `%s`:\n%s\n",
			[s(Filename), s(io.error_message(Error))])))
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- pred scanFile1(config1(R), io.input_stream, scanable.result(config), io.state, io.state) <= ePRNG(R).
:- mode scanFile1(in, in, out, di, uo) is det.

scanFile1(state1(Random), Stream, IMConfig, !IO) :-
	scanable.scanInt(Stream, yes, "number of runs", bounded(0, yes), unbound, MNumberRuns, !IO),
	(if
		MNumberRuns = ok(ok(NumberRuns))
	then
		scanFile1(state2(Random, NumberRuns), Stream, IMConfig, !IO)
	else
		IMConfig = scanable.noOkToIOresultMaybeError(MNumberRuns)
	).

scanFile1(state2(Random, NumberRuns), Stream, IMConfig, !IO) :-
	io.read_line_as_string(Stream, RLine, !IO),
	(if
		RLine = ok(Line)
	then
		GameName = string.strip(Line),
		(if
			tool.gameData(GameName, _, Game, _)
		then
			Game = '2x2',
			gl.'2x2'.readGame(Stream, IMGame, !IO),
			scanFile2(state3(Random, NumberRuns, IMGame), Stream, IMConfig, !IO)
			;
			Game = centipede,
			gl.centipede.readGame(Stream, IMGame, !IO),
			scanFile2(state3(Random, NumberRuns, IMGame), Stream, IMConfig, !IO)
			;
			Game = investment,
			gl.investment.scanGame(Stream, IMGame, !IO),
			scanFile2(state3(Random, NumberRuns, IMGame), Stream, IMConfig, !IO)
			;
			Game = pgp,
			gl.pgp.readGame(Stream, IMGame, !IO),
			scanFile2(state3(Random, NumberRuns, IMGame), Stream, IMConfig, !IO)
			;
			Game = 'pgp+pa',
			gl.'pgp+pa'.readGame(Stream, IMGame, !IO),
			scanFile2(state3(Random, NumberRuns, IMGame), Stream, IMConfig, !IO)
			;
			Game = ultimatum,
			gl.ultimatum.readGame(Stream, IMGame, !IO),
			scanFile2(state3(Random, NumberRuns, IMGame), Stream, IMConfig, !IO)
		else
			IMConfig = ok(error(string.format("Unknown game: %s\n", [s(GameName)])))
		)
	else
		IMConfig = scanable.noOkToResult("game name", RLine)
	).

:- pred scanFile2(config2(R, G), io.input_stream, scanable.result(config), io.state, io.state)
	<= (
	ePRNG(R),
	game(G, CS),
	chromosome(CS, T, P),
	foldable(CS, A),
	printable(CS),
	printable(T),
	printable(A),
	scanable(CS),
	scanable(P))
	.
:- mode scanFile2(in, in, out, di, uo) is det.

scanFile2(state3(Random, NumberRuns, IMGame), Stream, IMConfig, !IO) :-
	(if
		IMGame = ok(ok(Game))
	then
		scanFile2(state4(Random, NumberRuns, Game), Stream, IMConfig, !IO)
	else
		IMConfig = scanable.noOkToIOresultMaybeError(IMGame)
	).

scanFile2(state4(Random, NumberRuns, Game), Stream, IMConfig, !IO) :-
	scanable.scan(Stream, IMParameters, !IO),
	(if
		IMParameters = ok(ok(Parameters))
	then
		scanFile3(state5(Random, NumberRuns, Game, Parameters), Stream, IMConfig, !IO)
	else
		IMConfig = scanable.noOkToIOresultMaybeError(IMParameters)
	).

:- pred scanFile3(config3(R, G, P), io.input_stream, scanable.result(config), io.state, io.state)
	<= (
	ePRNG(R),
	game(G, CS),
	chromosome(CS, T, P),
	foldable(CS, A),
	printable(CS),
	printable(T),
	printable(A),
	scanable(CS),
	scanable(P)).
:- mode scanFile3(in, in, out, di, uo) is det.

scanFile3(state5(Random, NumberRuns, Game, GameStrategyParameters), Stream, IMConfig, !IO) :-
	ebea.player.scanParameters(Stream, GameStrategyParameters, MPlayerParameters, !IO),
	(if
		MPlayerParameters = ok(ok(PlayerParameters))
	then
		ebea.population.readParameters(Stream, PlayerParameters, MPopulationParameters, !IO),
		(if
			MPopulationParameters = yes(PopulationParameters)
		then
			scanFile3(state6(Random, NumberRuns, Game, PopulationParameters), Stream, IMConfig, !IO)
		else
			IMConfig = ok(error("Error reading population parameters"))
		)
	else
		IMConfig = scanable.noOkToIOresultMaybeError(MPlayerParameters)
	).

scanFile3(state6(Random, NumberRuns, Game, PopulationParameters), Stream, IMConfig, !IO) :-
	ebea.streams.scanLevel(Stream, IMLevel, !IO),
	(if
		IMLevel = ok(ok(Level))
	then
		scanable.scanRestTextStreamQuantityElement(Stream, PListQuantityElement, !IO),
		(if
			PListQuantityElement = ok(ListQuantityElement)
		then
			IMConfig = ok(ok('new cfg'(config(Random, NumberRuns, Game, PopulationParameters, ListQuantityElement, Level))))
		else
			IMConfig = ok(error("Some error occurred while scanning population composition"))
		)
	else
		IMConfig = scanable.noOkToIOresultMaybeError(IMLevel)
	).

:- end_module file.config.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
