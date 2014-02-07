/**
 * This module contains all the modules that implement the tools in the
 * Energy Based Evolutionary Algorithm Toolkit.

 * @author Pedro Mariano
 * @version 1.0 2013/05/17
 */
:- module tool.

:- interface.

%:- include_module runSim, replaySim, batchRun, analyseGame, editFileBatch.
:- include_module runSim, batchRun, analyseGame, editFileBatch.

:- import_module chromosome, game.
:- import_module foldable, scanable, printable.
:- import_module list.

:- type gameData(G, CS, T, P, A) --->
	gameData(
		game        :: G,
		strategy    :: list(CS),
		phenotype   :: T,
		parameters  :: P,
		accumulator :: A
	).

:- type gameData --->
	some [G, CS, T, P, A]
	gd(gameData(G, CS, T, P, A))
	=> (
		game(G, CS),
		chromosome(CS, T, P),
		foldable(CS, A),
		% scanable(G),
		% scanable(CS),
		% scanable(P),
		printable(CS)
	).

% :- typeclass gameData(G, CS, T, P, A)
% 	<= (
% 	game(G, CS),
% 	chromosome(CS, T, P),
% 	foldable(CS, A),
% 	scanable(G),
% 	scanable(CS),
% 	scanable(P),
% 	printable(CS),
% 	printable(T),
% 	printable(A)
% 	) where
% 	[].

:- type games --->
	'2x2' ;
	centipede ;
	investment ;
	pgp ;
	'pgp+pa' ;
	ultimatum.

%:- inst gameData == bound(gameData(ground, non_empty_list, ground, ground, ground)).

:- pred gameData(string, string, games, gameData).
:- mode gameData(out, out, out, out) is multi.
:- mode gameData(in, out, out, out) is semidet.
:- mode gameData(out, out, in, out) is det.

:- implementation.

:- import_module gl, gl.'2x2', gl.centipede, gl.investment, gl.pgp, gl.'pgp+pa', gl.ultimatum.
:- import_module gl.'2x2', gl.'2x2'.game, gl.'2x2'.strategy, gl.'2x2'.parameters.
:- import_module exception, unit.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%:- instance gameData('2x2'.game, '2x2'.strategy, unit, '2x2'.parameters, '2x2'.ac) where [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

gameData("2x2", "2-player 2-action", '2x2', 'new gd'(gameData(Game, [Strategy], Phenotype, Parameters, Accumulator))) :-
	Game = gl.'2x2'.game.initGame(1.0, 0.0),
	Strategy = gl.'2x2'.strategy.initProbabilisticStrategy(0.5),
	Phenotype = chromosome.born(Parameters, Strategy),
	Parameters = parameters(0.5),
	Accumulator = foldable.initAC `with_type` gl.'2x2'.ac.

gameData("centipede", "Centipede", centipede, 'new gd'(gameData(Game, [Strategy], Phenotype, Parameters, Accumulator))) :-
	(if
		gl.centipede.initGame(4, 0.9, geometric(2.0), G),
		gl.centipede.initStrategy(first, 1, S),
		gl.centipede.initParameters(4, 1.0, P)
	then
		G = Game,
		S = Strategy,
		P = Parameters
	else
		throw("gameData/3: Invalid pre-compiled centipede data")
	),
	Phenotype = chromosome.born(Parameters, Strategy),
	Accumulator = foldable.initAC `with_type` gl.centipede.ac.

gameData("investment", "Public Good Investment", investment,
			'new gd'(gameData(Game, [Strategy1, Strategy2], Phenotype, Parameters, Accumulator))) :-
	(if
		gl.investment.initGame(4, 2.0, G),
		gl.investment.initStochasticStrategy(0.2, S1),
		gl.investment.initStochasticStrategy(0.8, S2),
		gl.investment.initParameters(0.5, P)
	then
		Game = G,
		Strategy1 = S1,
		Strategy2 = S2,
		Parameters = P
	else
		throw("gameData/3: Invalid pre-compiled investment data")
	),
	Phenotype = chromosome.born(Parameters, Strategy1),
	Accumulator = foldable.initAC `with_type` gl.investment.ac.


gameData("pgp", "Public Good Provision", pgp, 'new gd'(gameData(Game, [Strategy], Phenotype, Parameters, Accumulator))) :-
	(if
		gl.pgp.initGame(3, 1.0, 0.4, G),
		gl.pgp.initStrategy(0.5, S)
	then
		G = Game,
		S = Strategy
	else
		throw("gameData/3: Invalid pre-compiled PGP data")
	),
	Phenotype = chromosome.born(Parameters, Strategy),
	Parameters = gl.pgp.parameters(0.5),
	Accumulator = foldable.initAC `with_type` gl.pgp.pgpAc.

gameData("pgp+pa", "Public Good Provision with Punishment and Abstaining", 'pgp+pa', 'new gd'(gameData(Game, [Strategy], Phenotype, Parameters, Accumulator))) :-
	(if
		gl.'pgp+pa'.initGame(3, 0.2, 0.4, 0.1, 0.3, G)
	then
		Game = G
	else
		throw("gameData/3: Invalid pre-compiled PGP+PA data")
	),
	Strategy = gl.'pgp+pa'.initStrategy(loner),
	Phenotype = chromosome.born(Parameters, Strategy),
	Parameters = noParameters,
	Accumulator = foldable.initAC `with_type` gl.'pgp+pa'.ac.

gameData("ultimatum", "Ultimatum", ultimatum, 'new gd'(gameData(Game, [Strategy], Phenotype, Parameters, Accumulator))) :-
	(if
		gl.ultimatum.initGame(4, G),
		gl.ultimatum.initDictatorStrategy(5, S)
	then
		Game = G,
		Strategy = S
	else
		throw("gameData/3: Invalid pre-compiled ultimatum data")
	),
	Phenotype = chromosome.born(Parameters, Strategy),
	Parameters = gl.ultimatum.initParameters(4, 0.5),
	Accumulator = foldable.initAC `with_type` gl.ultimatum.ac.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- end_module tool.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
