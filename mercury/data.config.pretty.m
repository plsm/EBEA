/**
 * 

 * @author Pedro Mariano
 * @version 1.0 2014/01/27
 */
:- module data.config.pretty.

:- interface.

:- type format --->
	plain.

:- pred print(io.output_stream, format, data.config.config, io.state, io.state).
:- mode print(in, in, in, di, uo) is det.

:- implementation.

:- import_module ebea.population.site, ebea.population.site.parameters.
:- import_module game.

:- import_module fraction.
:- import_module float.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

print(Stream, plain, Config, !IO) :-
	io.format(Stream, "-- statistical --
pseudo-random number generator: %s
number of runs:  %d
number of iterations:  %d
streams:  %s
-- general population dynamics --
dynamic:  %s
mutation probability:   ",
		[s(string(Config^random)),
		 i(Config^numberRuns),
		 i(Config^numberIterations),
		 s(string(Config^level)),
		 s(string(Config^data.config.dynamic))
		],
		!IO),
	probability.print(Stream, Config^mutationProbability, !IO),
	io.print(Stream, "\nmigration probability:  ", !IO),
	probability.print(Stream, Config^migrationProbability, !IO),
	io.print(Stream, "\n-- age --\n", !IO),
	Config^ageParameters = AP,
	(
		AP = noDeath,
		io.print(Stream, "no death\n", !IO)
		;
		AP = deathByOldAge(_, _),
		io.format(Stream, "old age:         %d
death suaveness:  %.1f\n", [i(AP^oldAge), f(AP^deathSuaveness)], !IO)
	),
	Config^energyParameters = EP,
	io.format(Stream, "-- energy --
energy scaling:     %s
energy reproduce:   %.1f
energy newborn:     %.1f%% e_R
energy lost birth:  %.1f%% e_R
coefficient:        %.1f\n",
			[s(string(EP^energyScaling)),
			 f(EP^energyReproduce),
			 f(EP^energyNewborn * 100.0),
			 f(EP^energyLostBirth * 100.0),
			 f(EP^coefficient)],
			!IO),
	Config^selectionParameters = SP,
	io.format(Stream, "-- selection --
stddev pool size:                 %f
stddev bits per probability:      %f
stddev probability update factor: %f
stddev payoff threshold:          %f
uncertainty increase factor: %f
mu:                          %f
pool size parent-offspring transmission: %d%%
stddev mu:                          %f
stddev uncertainty increase factor: %f\n",
		[f(SP^poolSizeStdDev),
		 f(SP^bitsPerProbabilityStdDev),
		 f(SP^probabilityUpdateFactorStdDev),
		 f(SP^payoffThresholdStdDev),
		
		 f(SP^uncertaintyIncreaseFactor),
		 f(SP^mu),

		 i(SP^poolSizePercentageTransmission),

		 f(SP^muStdDev),
		 f(SP^uncertaintyIncreaseFactorStdDev)
		],
		!IO),
	io.format(Stream, "-- game --\nselected: %s\n", [s(string(Config^selectedGame))], !IO),
	(
		Config^selectedGame = '2x2',
		print_2x2(Stream, plain, Config, Config^cfg_2x2, !IO)
		;
		Config^selectedGame = battlesexes,
		print_battlesexes(Stream, plain, Config, Config^battlesexes, !IO)
		;
		Config^selectedGame = centipede,
		print_centipede(Stream, plain, Config, Config^centipede, !IO)
		;
		Config^selectedGame = givetake,
		print_givetake(Stream, plain, Config, Config^givetake, !IO)
		;
		Config^selectedGame = investment,
		print_investment(Stream, plain, Config, Config^investment, !IO)
		;
		Config^selectedGame = pgp,
		print_pgp(Stream, plain, Config, Config^pgp, !IO)
		;
		Config^selectedGame = 'pgp+pa',
		'print_pgp+pa'(Stream, plain, Config, Config^'pgp+pa', !IO)
		;
		Config^selectedGame = ultimatum,
		print_ultimatum(Stream, plain, Config, Config^ultimatum, !IO)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions


% :- pred printGame(io.output_stream, G, io.state, io.state) <= (game(G,_)).
% :- mode printGame(in, in, di, uo) is det.

% printGame(Stream, Game, !IO) :-
% 	io.format(Stream, "maximum payoff: %f\nminimum payoff: %f\nPareto payoff: %f\n",
% 		[f(highestPayoff(Game)), f(minimumPayoff(Game)), f(paretoPayoff(Game))],
% 				 !IO).

:- pred print_2x2(io.output_stream, format, config, config_2x2, io.state, io.state).
:- mode print_2x2(in, in, in, in, di, uo) is det.

print_2x2(Stream, plain, _AllConfig, GameConfig, !IO) :-
	
	io.format(Stream, "      C     D
    ------------
C  | 1.0   %4.1f |
D  | %3.1f    0.0 |\n", [f(GameConfig^game^sucker), f(GameConfig^game^temptation)], !IO),
	(if
		GameConfig^game^temptation = 1.0
		;
		GameConfig^game^sucker = 0.0
	then
		io.print(Stream, "In boundary between games\n", !IO)
	else if
		GameConfig^game^temptation < 1.0,
		GameConfig^game^sucker < 0.0
	then
		io.print(Stream, "Stag hunt\n", !IO)
	else if
		GameConfig^game^temptation < 1.0,
		GameConfig^game^sucker > 0.0
	then
		io.print(Stream, "Harmony\n", !IO)
	else if
		GameConfig^game^temptation > 1.0,
		GameConfig^game^sucker < 0.0
	then
		io.print(Stream, "Prisoner's dilemma\n", !IO)
	else
		io.print(Stream, "Snowdrift\n", !IO)
	),
	io.format(Stream, "-- geometry --\n%s\n", [s(string(GameConfig^initialPopulation^geometry))], !IO),
	list.foldl(printSite(Stream, plain), GameConfig^initialPopulation^sites, !IO).

:- pred print_battlesexes(io.output_stream, format, config, config_battlesexes, io.state, io.state).
:- mode print_battlesexes(in, in, in, in, di, uo) is det.

print_battlesexes(Stream, plain, _AllConfig, GameConfig, !IO) :-
	io.format(Stream, "         male payoff      female payoff
        opera | tennis               opera | tennis
opera   %5.1f | %6.1f                %5.1f | %6.1f
tennis  %5.1f | %6.1f                %5.1f | %6.1f\n",
		[f(float(GameConfig^game^payoffSamePlaceDislike)),
		 f(float(GameConfig^game^payoffDiffPlace)),
		 f(float(GameConfig^game^payoffSamePlaceLike)),
		 f(float(GameConfig^game^payoffDiffPlace)),

		 f(float(GameConfig^game^payoffDiffPlace)),
		 f(float(GameConfig^game^payoffSamePlaceLike)),
		 f(float(GameConfig^game^payoffDiffPlace)),
		 f(float(GameConfig^game^payoffSamePlaceDislike))
		], !IO),
	io.format(Stream, "-- geometry --\n%s\n", [s(string(GameConfig^initialPopulation^geometry))], !IO),
	list.foldl(printSite(Stream, plain), GameConfig^initialPopulation^sites, !IO).

:- pred print_centipede(io.output_stream, format, config, config_centipede, io.state, io.state).
:- mode print_centipede(in, in, in, in, di, uo) is det.

print_centipede(_Stream, plain, _AllConfig, _GameConfig, !IO).

:- pred print_givetake(io.output_stream, format, config, config_givetake, io.state, io.state).
:- mode print_givetake(in, in, in, in, di, uo) is det.

print_givetake(Stream, plain, _AllConfig, GameConfig, !IO) :-
	io.format(Stream, "give bonus:           %f\n", [f(GameConfig^game^bg)], !IO),
	io.format(Stream, "take performer cost:  %f\n", [f(GameConfig^game^cpt)], !IO),
	io.format(Stream, "take subject cost:    %f\n", [f(GameConfig^game^cst)], !IO),
	io.format(Stream, "number of stages:     %s\n", [s(string(GameConfig^game^numberStages))], !IO),
	io.format(Stream, "-- geometry --\n%s\n", [s(string(GameConfig^initialPopulation^geometry))], !IO),
	list.foldl(printSite(Stream, plain), GameConfig^initialPopulation^sites, !IO).

:- pred print_investment(io.output_stream, format, config, config_investment, io.state, io.state).
:- mode print_investment(in, in, in, in, di, uo) is det.

print_investment(_Stream, plain, _AllConfig, _GameConfig, !IO).

:- pred print_pgp(io.output_stream, format, config, config_pgp, io.state, io.state).
:- mode print_pgp(in, in, in, in, di, uo) is det.

print_pgp(Stream, plain, _AllConfig, GameConfig, !IO) :-
	io.format(Stream, "number of players:  %d\n", [i(GameConfig^game^players)], !IO),
	io.format(Stream, "good values:        %f\n", [f(GameConfig^game^good)], !IO),
	io.format(Stream, "provision cost:     %f\n", [f(GameConfig^game^provisionCost)], !IO),
	io.format(Stream, "standard deviation used in mutation:     %f\n", [f(GameConfig^parameters^stdev)], !IO),
	printConfiguration(Stream, plain, GameConfig^initialPopulation, !IO)
	.
%	io.format(Stream, "-- geometry --\n%s\n", [s(string(GameConfig^initialPopulation^geometry))], !IO),
%	list.foldl(printSite(Stream, plain), GameConfig^initialPopulation^sites, !IO).

:- pred 'print_pgp+pa'(io.output_stream, format, config, 'config_pgp+pa', io.state, io.state).
:- mode 'print_pgp+pa'(in, in, in, in, di, uo) is det.

'print_pgp+pa'(_Stream, plain, _AllConfig, _GameConfig, !IO).

:- pred print_ultimatum(io.output_stream, format, config, config_ultimatum, io.state, io.state).
:- mode print_ultimatum(in, in, in, in, di, uo) is det.

print_ultimatum(Stream, plain, _AllConfig, GameConfig, !IO) :-
	io.format(Stream, "Standard deviation of
  Gaussian distribution
  used to mutate
  strategy parameters:   %f\n", [f(GameConfig^parameters^stdev)], !IO),
	io.format(Stream, "Cake size:  %d\n", [i(GameConfig^game^cakeSize)], !IO),
	io.format(Stream, "-- geometry --\n%s\n", [s(string(GameConfig^initialPopulation^geometry))], !IO),
	list.foldl(printSite(Stream, plain), GameConfig^initialPopulation^sites, !IO).



:- pred printConfiguration(
	io.output_stream                                    :: in,
	data.config.pretty.format                           :: in,
	ebea.population.configuration.configuration(CS, MU) :: in,
	io.state :: di,  io.state :: uo
	) is det.

printConfiguration(Stream, Format, Configuration, !IO) :-
	(	%
		Format = plain,
		io.print(Stream, "-- initial population --\n", !IO),
		io.format(Stream, "  geometry:    %s\n", [s(string(Configuration^geometry))], !IO),
		io.format(Stream, "  default carrying capacity:  %i\n", [i(Configuration^defaultCarryingCapacity)], !IO),
		io.format(Stream, "  site dynamics:  %s\n", [s(string(Configuration^siteDynamics))], !IO)
	),
	list.foldl(printSite(Stream, Format), Configuration^sites, !IO).


:- pred printSite(io.output_stream, format, ebea.population.site.parameters.parameters(CS), io.state, io.state).
:- mode printSite(in, in, in, di, uo) is det.

printSite(Stream, plain, SiteParameters, !IO) :-
	io.format(Stream, "\tsite index:     %d\n\tcarrying capacity: %d\n\tchromosomes:\n",
		[i(SiteParameters^id), i(SiteParameters^carryingCapacity)], !IO),
	PredPrint =
	(pred(IP::in, IOdi::di, IOuo::uo) is det :-
	io.format(Stream, "\t\t#%5d  %s\n", [i(IP^quantity), s(string(IP^chromosome))], IOdi, IOuo)
	),
	list.foldl(PredPrint, SiteParameters^chromosomes, !IO).

:- end_module data.config.pretty.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
