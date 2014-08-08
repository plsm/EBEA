/**
 * Main module of the console version of the Energy Based Evolutionary
 * Algorithm Tool Kit.  In this application the user can edit the
 * parameters of an EBEA simulation.  These parameters are represented by
 * type {@code config} from module {@code data.config}.

 * @author Pedro Mariano
 * @version 1.0 2014/01/26
 */
:- module 'EBEAtk_console'.

:- interface.

:- import_module io.

:- pred main(io.state, io.state).
:- mode main(di, uo) is det.

:- implementation.

:- import_module ui_console, userInterface.

:- import_module data, data.config, data.config.io.
:- import_module data.config.pretty.
:- import_module ebea, ebea.core, ebea.population, ebea.player.
:- import_module ebea.player.selection, ebea.player.energy, ebea.player.selection.pcv.
:- import_module tools, tools.export_playerProfiles_graphviz, tools.'PCVNetwork'.

:- import_module gl, gl.battlesexes, gl.battlesexes.strategy.
:- import_module gl, gl.givetake, gl.givetake.strategy.
:- import_module gl, gl.pgp, gl.pgp.strategy.
:- import_module fraction.
:- import_module array, bool, char, float, getopt, int, list, map, maybe, string, thread, unit.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

:- type data --->
	data(
		config   :: data.config.config,
		filename :: maybe(string)
	).

:- type opt --->
	filename ;
	run ;
	print ;
	help.

:- type runMode --->
	gui ;
	run ;
	print.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

main(!IO) :-
	io.command_line_arguments(Arguments, !IO),
	(if
		Arguments = []
	then
		ui_console.show(menu, initData, _, !IO)
	else
		getopt.process_options(option_ops_multi(optShort, optLong, optDefault), Arguments, RestArgs, IResult),
		check1(IResult, RestArgs, !IO)
	).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions



:- pred optShort(char::in, opt::out) is semidet.

optShort('f', filename).
optShort('r', run).
optShort('h', help).
optShort('p', print).

:- pred optLong(string::in, opt::out) is semidet.

optLong("print", print).
optLong("filename", filename).
optLong("help", help).
optLong("run", run).

:- pred optDefault(opt::out, getopt.option_data::out) is multi.

optDefault(run, bool(no)).
optDefault(filename, maybe_string(no)).
optDefault(help, bool(no)).
optDefault(print, bool(no)).

:- pred printUsage(io::di, io::uo) is det.

printUsage(!IO) :-
	io.print("Usage:
\tEBEAtk_console [OPTION]
Available options are:
\t-h, --help           print this help and exit
\t-p, --print          print configuration file and exit
\t-f, --filename FILE  filename to load, by default is config.txt
\t-r, --run            run EBEA in the background\n", !IO).

:- pred check1(getopt.maybe_option_table(opt)::in, list(string)::in, io::di, io::uo) is det.

check1(IResult, RestArgs, !IO) :-
	(
		IResult = ok(Result),
		(if
			RestArgs = []
		then
			check2(Result, !IO)
		else
			io.print(io.stderr_stream, "Unrecognised options\n", !IO),
			io.set_exit_status(1, !IO),
			printUsage(!IO)
		)
	;
		IResult = error(Message),
		io.format("Error processing options:\n%s\n", [s(Message)], !IO),
		io.set_exit_status(1, !IO),
		printUsage(!IO)
	).


:- pred check2(option_table(opt)::in, io::di, io::uo) is det.

check2(Result, !IO) :-
	(if
		map.lookup(Result, help, bool(yes))
	then
		printUsage(!IO)
	else
		check3(Result, !IO)
	).


:- pred check3(option_table(opt)::in, io::di, io::uo) is det.

check3(Result, !IO) :-
	map.lookup(Result, run, FlagRun),
	map.lookup(Result, print, FlagPrint),
	(if
		FlagRun = bool(yes),
		FlagPrint = bool(no),
		Mode = run
		;
		FlagRun = bool(no),
		FlagPrint = bool(yes),
		Mode = print
		;
		FlagRun = bool(no),
		FlagPrint = bool(no),
		Mode = gui
	then
		(if
			map.lookup(Result, filename, maybe_string(MFilename))
		then
			run(Mode, MFilename, !IO)
		else
			true % never reached
		)
	else
		io.print(io.stderr_stream, "More than one mode specified!\n", !IO),
		io.set_exit_status(1, !IO),
		printUsage(!IO)
	).


:- pred run(runMode, maybe(string), io.state, io.state).
:- mode run(in, in, di, uo) is det.

run(print, MFilename, !IO) :-
	(
		MFilename = no,
		Filename = "config.txt"
		;
		MFilename = yes(Filename)
	),
	data.config.io.read(Filename, MConfig, !IO),
	(
		MConfig = ok(Config),
		data.config.pretty.print(io.stdout_stream, plain, Config, !IO)
		;
		MConfig = error(Msg),
		io.print(Msg, !IO),
		io.nl(!IO),
		io.set_exit_status(1, !IO)
	).

run(run, MFilename, !IO) :-
	(
		MFilename = no,
		Filename = "config.txt"
		;
		MFilename = yes(Filename)
	),
	data.config.io.read(Filename, MConfig, !IO),
	(
		MConfig = ok(Config),
		data.config.runBackground(Config, !IO)
		;
		MConfig = error(Msg),
		io.print(Msg, !IO),
		io.nl(!IO),
		io.set_exit_status(1, !IO)
	).
	
run(gui, MFilename, !IO) :-
	(
		MFilename = no,
		Data = initData
		;
		MFilename = yes(Filename),
		data.config.io.read(Filename, MConfig, !IO),
		(
			MConfig = ok(Config),
			Data = data(Config, MFilename)
			;
			MConfig = error(Msg),
			io.print(Msg, !IO),
			io.nl(!IO),
			Data = data(data.config.default, MFilename)
		)
	),
	ui_console.show(menu, Data, _, !IO)
	.


:- func menu = userInterface(data).
:- mode menu = out(userInterface).

menu = m(
	[mi(label("Configuration Editor"), edit('new dialog'(get_config, set_config, data.config.dialog))),
	 mi(label("Load configuration"),   updateDataIO(loadConfiguration)),
	 mi(label("Save configuration"),   updateDataIO(saveConfiguration)),
	 mi(label("Save As"),              updateDataIO(saveAsConfiguration)),
	 mi(label("Print"),                actionDataIO(printConfiguration)),
	 mi(label("Run background"),       actionDataIO('EBEAtk_console'.runBackground)),
	 mi(label("Flash Talk 2014"),      actionDataIO(run_flashTalk2014)),
	 mi(label("Run interactively"),    actionDataIO(runInteractively))
	]).

:- func initData = data.

initData = data(data.config.default, no).

:- func get_config(data) = data.config.config.

get_config(D) = D^config.

:- func set_config(data, data.config.config) = setResult(data).

set_config(D, C) = ok('config :='(D, C)).

:- pred loadConfiguration(data, data, io.state, io.state).
:- mode loadConfiguration(in, out, di, uo) is det.

loadConfiguration(!Data, !IO) :-
	io.print("File name? ", !IO),
	io.read_line_as_string(ILine, !IO),
	(if
		ILine = ok(Line)
	then
		Filename = string.strip(Line),
		data.config.io.read(Filename, MConfig, !IO),
		(
			MConfig = ok(Config),
			NewData^config = Config,
			NewData^filename = yes(Filename),
			!:Data = NewData,
			data.config.pretty.print(io.stdout_stream, plain, Config, !IO)
			;
			MConfig = error(Msg),
			io.print(Msg, !IO),
			io.nl(!IO)
		)
	else
		true
	).


:- pred saveConfiguration(data, data, io.state, io.state).
:- mode saveConfiguration(in, out, di, uo) is det.

saveConfiguration(!Data, !IO) :-
	!.Data^filename = no,
	saveAsConfiguration(!Data, !IO)
	;
	!.Data^filename = yes(Filename),
	data.config.io.write(binary, Filename, !.Data^config, MErrors, !IO),
	(
		MErrors = no
		;
		MErrors = yes(Errors),
		io.print(Errors, !IO),
		io.nl(!IO)
	).

:- pred saveAsConfiguration(data, data, io.state, io.state).
:- mode saveAsConfiguration(in, out, di, uo) is det.

saveAsConfiguration(!Data, !IO) :-
	io.print("File name? ", !IO),
	io.read_line_as_string(ILine, !IO),
	(if
		ILine = ok(Line)
	then
		Filename = string.strip(Line),
		data.config.io.write(binary, Filename, !.Data^config, MErrors, !IO),
		(
			MErrors = no
			;
			MErrors = yes(Errors),
			io.print(Errors, !IO),
			io.nl(!IO)
		),
		!:Data = 'filename :='(!.Data, yes(Filename))
	else
		true
	)
	.

:- pred printConfiguration(data, io.state, io.state).
:- mode printConfiguration(in, di, uo) is det.

printConfiguration(Data, !IO) :-
	data.config.pretty.print(io.stdout_stream, plain, Data^config, !IO)
	.



:- pred runBackground(data, io.state, io.state).
:- mode runBackground(in, di, uo) is det.

runBackground(Data, !IO) :-
	data.config.runBackground(Data^config, !IO).

:- pred runInteractively(data, io.state, io.state).
:- mode runInteractively(in, di, uo) is det.

runInteractively(Data, !IO) :-
	(if
		thread.can_spawn
	then
		promise_equivalent_solutions [!:IO] thread.spawn(launchWindow_PopulationSize, !IO),
		io.print("Launched window\nPress ENTER to run", !IO),
		io.read_line_as_string(_, !IO),
	Config = Data^config,
	(
		Config^selectedGame = '2x2',
		data.config.runInteractively('2x2'(startRun, print_PopulationSize, clickToContinue), Config, !IO)
		;
		Config^selectedGame = battlesexes,
		data.config.runInteractively(battlesexes(startRun, print_PopulationSize, clickToContinue), Config, !IO)
		;
		Config^selectedGame = centipede,
		data.config.runInteractively(centipede(startRun, print_PopulationSize, clickToContinue), Config, !IO)
		;
		Config^selectedGame = givetake,
		data.config.runInteractively(givetake(startRun, print_PopulationSize, clickToContinue), Config, !IO)
		;
		Config^selectedGame = investment,
		data.config.runInteractively(investment(startRun, print_PopulationSize, clickToContinue), Config, !IO)
		;
		Config^selectedGame = pgp,
		data.config.runInteractively(pgp(startRun, print_PopulationSize, clickToContinue), Config, !IO)
		;
		Config^selectedGame = 'pgp+pa',
		data.config.runInteractively('pgp+pa'(startRun, print_PopulationSize, clickToContinue), Config, !IO)
		;
		Config^selectedGame = ultimatum,
		data.config.runInteractively(ultimatum(startRun, print_PopulationSize, clickToContinue), Config, !IO)
	)
	else
		io.print(io.stderr_stream, "Could not launch window\n", !IO)
	).



:- pragma foreign_decl("C",
"
#include ""plot-population.h""
							 
").

:- pred launchWindow_PopulationSize(io.state, io.state).
:- mode launchWindow_PopulationSize(di, uo) is cc_multi.


:- pragma foreign_proc(
	"C",
	launchWindow_PopulationSize(IOdi::di, IOuo::uo),
	[will_not_call_mercury, thread_safe, promise_pure],
	"
		IOdi = IOuo;
		char *label[] = {""population size"", ""average energy"", ""average age""};
		unsigned int rangeIndex[] = {0, 1, 2};
		initGraphicsWindow (3, label, rangeIndex);
	"
	).



:- type plotData --->
	plotData(
		energy :: float,
		age    :: int
	).

:- pred print_PopulationSize(population(C, T), bool, io.state, io.state).
:- mode print_PopulationSize(in, out, di, uo) is det.

print_PopulationSize(Population, no, !IO) :-
	Add =
	(func(P, AC) = R :-
		R^energy = AC^energy + P^traits^energyTrait,
		R^age = AC^age + P^traits^ageTrait
	),
	PopulationSize = float(ebea.population.size(Population)),
	ebea.population.fold_players(Add, Population, plotData(0.0, 0)) = PlotData,
	PlotData^energy / PopulationSize = AverageEnergy,
	float(PlotData^age) / PopulationSize = AverageAge,
	addPoints_PopulationSize(PopulationSize, AverageEnergy, AverageAge, !IO).


:- pred addPoints_PopulationSize(float, float, float, io.state, io.state).
:- mode addPoints_PopulationSize(in, in, in, di, uo) is det.


:- pragma foreign_proc(
	"C",
	addPoints_PopulationSize(PopulationSize::in, AverageEnergy::in, AverageAge::in, IOdi::di, IOuo::uo),
	[will_not_call_mercury, thread_safe, promise_pure],
	"
		IOdi = IOuo;
		addPoints (PopulationSize, AverageEnergy, AverageAge);
	"
	).






:- pred run_flashTalk2014(data, io.state, io.state).
:- mode run_flashTalk2014(in, di, uo) is det.

run_flashTalk2014(Data, !IO) :-
	Config = Data^config,
	(if
		thread.can_spawn
	then
		Config^selectedGame = '2x2',
%		data.config.runInteractively('2x2'(startRun, print_PopulationSize, clickToContinue), Config, !IO),
		true
		;
		Config^selectedGame = battlesexes,
		promise_equivalent_solutions [!:IO] thread.spawn(launchWindow_flashTalk2014_battlesexes, !IO),
		io.print("Launched window\nPress ENTER to run", !IO),
		io.read_line_as_string(_, !IO),
		data.config.runInteractively(battlesexes(startRun, print_flashTalk2014_battlesexes, clickToContinue), Config, !IO)
		;
		Config^selectedGame = centipede,
%		data.config.runInteractively(centipede(startRun, print_PopulationSize, clickToContinue), Config, !IO)
		true
		;
		Config^selectedGame = givetake,
		promise_equivalent_solutions [!:IO] thread.spawn(launchWindow_flashTalk2014_givetake, !IO),
		io.print("Launched window\nPress ENTER to run", !IO),
		io.read_line_as_string(_, !IO),
		data.config.runInteractively(givetake(startRun, print_flashTalk2014_givetake, clickToContinue), Config, !IO)
		;
		Config^selectedGame = investment,
%		data.config.runInteractively(investment(startRun, print_PopulationSize, clickToContinue), Config, !IO)
		true
		;
		Config^selectedGame = pgp,
		promise_equivalent_solutions [!:IO] thread.spawn(launchWindow_flashTalk2014_pgp, !IO),
		io.print("Launched window\nPress ENTER to run", !IO),
		io.read_line_as_string(_, !IO),
		data.config.runInteractively(pgp(startRun, print_flashTalk2014_pgp, clickToContinue), Config, !IO)
		;
		Config^selectedGame = 'pgp+pa',
%		data.config.runInteractively('pgp+pa'(startRun, print_PopulationSize, clickToContinue), Config, !IO)
		true
		;
		Config^selectedGame = ultimatum,
%		data.config.runInteractively(ultimatum(startRun, print_PopulationSize, clickToContinue), Config, !IO)
		true
	else
		io.print(io.stderr_stream, "Could not launch window\n", !IO)
	).




:- pred launchWindow_flashTalk2014_battlesexes(io.state, io.state).
:- mode launchWindow_flashTalk2014_battlesexes(di, uo) is cc_multi.


:- pragma foreign_proc(
	"C",
	launchWindow_flashTalk2014_battlesexes(IOdi::di, IOuo::uo),
	[will_not_call_mercury, thread_safe, promise_pure],
	"
		IOdi = IOuo;
		char *label[] = {""homem nicho 1 futebol"", ""mulher nicho 1 opera"", ""homem nicho 2 futebol"", ""mulher nicho 2 opera""};
		unsigned int rangeIndex[] = {0, 0, 0, 0};
		initGraphicsWindow (4, label, rangeIndex);
	"
	).




:- pred print_flashTalk2014_battlesexes(population(gl.battlesexes.strategy.strategy, unit), bool, io.state, io.state).
:- mode print_flashTalk2014_battlesexes(in, out, di, uo) is det.

print_flashTalk2014_battlesexes(Population, no, !IO) :-
	ebea.population.fold_sites(ebea.player.foldChromosome, Population, ebea.player.initAc, Data),
	array.lookup(Data, 0) = ac(_, _, _, AC0),
	array.lookup(Data, 1) = ac(_, _, _, AC1),
	% io.print(AC0, !IO),
	% io.print("\t", !IO),
	% io.print(AC1, !IO),
	% io.nl(!IO),
	(if
		AC0^qtyMale \= 0
	then
		addPoint(0,  AC0^sumProbabilityMaleGoTennis / float(AC0^qtyMale), !IO)
	else
		addPoint(0,  0.0, !IO)
	),
	(if
		AC0^qtyFemale \= 0
	then
		addPoint(1,  AC0^sumProbabilityFemaleGoOpera / float(AC0^qtyFemale), !IO)
	else
		addPoint(1,  0.0, !IO)
	),
	(if
		AC1^qtyMale \= 0
	then
		addPoint(2,  AC1^sumProbabilityMaleGoTennis / float(AC1^qtyMale), !IO)
	else
		addPoint(2,  0.0, !IO)
	),
	(if
		AC1^qtyFemale \= 0
	then
		addPoint(3,  AC1^sumProbabilityFemaleGoOpera / float(AC1^qtyFemale), !IO)
	else
		addPoint(3,  0.0, !IO)
	)
	.





:- pred launchWindow_flashTalk2014_pgp(io.state, io.state).
:- mode launchWindow_flashTalk2014_pgp(di, uo) is cc_multi.

:- pragma foreign_proc(
	"C",
	launchWindow_flashTalk2014_pgp(IOdi::di, IOuo::uo),
	[will_not_call_mercury, thread_safe, promise_pure],
	"
		IOdi = IOuo;
		char *label[] = {""cooperantes nicho 1"", ""exploradores nicho 1"", ""cooperantes nicho 2"", ""exploradores nicho 2""};
		unsigned int rangeIndex[] = {0, 0, 0, 0};
		initGraphicsWindow (4, label, rangeIndex);
	"
	).

:- pred print_flashTalk2014_pgp(population(gl.pgp.strategy.strategy, unit), bool, io.state, io.state).
:- mode print_flashTalk2014_pgp(in, out, di, uo) is det.

print_flashTalk2014_pgp(Population, no, !IO) :-
	ebea.population.fold_sites(ebea.player.foldChromosome, Population, ebea.player.initAc, Data),
	array.lookup(Data, 0) = ac(_, _, _, AC0),
	array.lookup(Data, 1) = ac(_, _, _, AC1),
	addPoint(0,  float(AC0^qtyDeteYes), !IO),
	addPoint(1,  float(AC0^qtyDeteNo), !IO),
	addPoint(2,  float(AC1^qtyDeteYes), !IO),
	addPoint(3,  float(AC1^qtyDeteNo), !IO)
	.



:- pred launchWindow_flashTalk2014_givetake(io.state, io.state).
:- mode launchWindow_flashTalk2014_givetake(di, uo) is cc_multi.

:- pragma foreign_proc(
	"C",
	launchWindow_flashTalk2014_givetake(IOdi::di, IOuo::uo),
	[will_not_call_mercury, thread_safe, promise_pure],
	"
		IOdi = IOuo;
		char *label[] = {""tempo dar nicho 1"", ""tempo tirar nicho 1"", ""tempo dar nicho 2"", ""tempo tirar nicho 2""};
		unsigned int rangeIndex[] = {0, 0, 0, 0};
		initGraphicsWindow (4, label, rangeIndex);
	"
	).

:- pred print_flashTalk2014_givetake(population(gl.givetake.strategy.strategy, unit), bool, io.state, io.state).
:- mode print_flashTalk2014_givetake(in, out, di, uo) is det.

print_flashTalk2014_givetake(Population, no, !IO) :-
	ebea.population.fold_sites(ebea.player.foldChromosome, Population, ebea.player.initAc, Data),
	array.lookup(Data, 0) = ac(_, _, _, AC0),
	array.lookup(Data, 1) = ac(_, _, _, AC1),
	(if
		AC0^qtyTime \= 0.0
	then
		addPoint(0,  float(AC0^sumTimeGive) / AC0^qtyTime, !IO),
		addPoint(1,  float(AC0^sumTimeTake) / AC0^qtyTime, !IO)
	else
		addPoint(0,  0.0, !IO),
		addPoint(1,  0.0, !IO)
	),
	(if
		AC1^qtyTime \= 0.0
	then
		addPoint(2,  float(AC1^sumTimeGive) / AC1^qtyTime, !IO),
		addPoint(3,  float(AC1^sumTimeTake) / AC1^qtyTime, !IO)
	else
		addPoint(2,  0.0, !IO),
		addPoint(3,  0.0, !IO)
	)
	.






:- pred addPoint(int, float, io.state, io.state).
:- mode addPoint(in, in, di, uo) is det.


:- pragma foreign_proc(
	"C",
	addPoint(Data::in, Value::in, IOdi::di, IOuo::uo),
	[will_not_call_mercury, thread_safe, promise_pure],
	"
		IOdi = IOuo;
		addPoint (Data, Value);
	"
	).



:- pred startRun(population(C, T), io.state, io.state).
:- mode startRun(in, di, uo) is det.

startRun(_, !IO) :-
	clearGraphics(!IO).


:- pred clearGraphics(io.state, io.state).
:- mode clearGraphics(di, uo) is det.

:- pragma foreign_proc(
	"C",
	clearGraphics(IOdi::di, IOuo::uo),
	[will_not_call_mercury, thread_safe, promise_pure],
	"
		IOdi = IOuo;
		clearGraphics ();
	"
	).

:- pred clickToContinue(population(C, T), io.state, io.state).
:- mode clickToContinue(in, di, uo) is det.

clickToContinue(_, !IO) :-
	clickToContinue(!IO).

:- pred clickToContinue(io.state, io.state).
:- mode clickToContinue(di, uo) is det.

:- pragma foreign_proc(
	"C",
	clickToContinue(IOdi::di, IOuo::uo),
	[will_not_call_mercury, thread_safe, promise_pure],
	"
		IOdi = IOuo;
		clickToContinue ();
	"
	).


:- end_module 'EBEAtk_console'.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
