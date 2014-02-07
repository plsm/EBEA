/**
 * One tool of the Energy Based Evolutionary Algorithm Toolkit is running a
 * simulation.  Parameters for the simulation are read from a configuration
 * file.

 * @author Pedro Mariano
 * @version 1.0 2013/05/18
 */
:- module tool.runSim.

:- interface.

:- import_module io, maybe, string.

:- pred go(maybe(string), io.state, io.state).
:- mode go(in, di, uo) is det.

:- implementation.

:- import_module ebea, ebea.core, ebea.streams, ebea.player, ebea.population.
:- import_module file, file.config.
:- import_module my, my.random.
:- import_module rng, rng.distribution.
:- import_module util.
:- import_module bool, list, maybe.

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

go(MFilename, !IO) :-
	file.config.scan(MFilename, IMConfig, !IO),
	(if
		IMConfig = ok(ok(cfg(Config)))
	then
%		io.print(Config, !IO), io.nl(!IO),
		ebea.streams.openOutputStreams(Config^level, IMStreams, !IO),
		(if
			IMStreams = ok(ok(Streams))
		then
			ebea.population.init(Config^parameters, Config^initialPopulation, Population, Config^random, NextRandom),
			ebea.core.initData(Config^game, Config^parameters, Streams) = Data,
			ebea.core.run(Data, Config^numberRuns, Population, rng.distribution.init, _, NextRandom, _, !IO),
			ebea.streams.closeOutputStreams(Streams, !IO)
		else
			scanable.printNoOkResult("opening EBEA streams", IMStreams, !IO)
		)
	else
		scanable.printNoOkResult("configuration file", IMConfig, !IO)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- end_module tool.runSim.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
