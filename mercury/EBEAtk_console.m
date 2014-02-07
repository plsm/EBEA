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

:- import_module  ebea.player.selection, ebea.player.energy, ebea.player.selection.pcv.

:- import_module bool, char, float, getopt, int, list, map, maybe, string, thread.

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
	 mi(label("Run background"),       actionDataIO('EBEAtk_console'.runBackground)),
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
		promise_equivalent_solutions [!:IO] thread.spawn(launchWindow, !IO),
		io.print("Launched window\nPress ENTER to run", !IO),
		io.read_line_as_string(_, !IO),
	Config = Data^config,
	(
		Config^selectedGame = '2x2',
		data.config.runInteractively('2x2'(startRun, printPopulationSize, clickToContinue), Config, !IO)
		;
		Config^selectedGame = battlesexes,
		data.config.runInteractively(battlesexes(startRun, printPopulationSize, clickToContinue), Config, !IO)
		;
		Config^selectedGame = centipede,
		data.config.runInteractively(centipede(startRun, printPopulationSize, clickToContinue), Config, !IO)
		;
		Config^selectedGame = pgp,
		data.config.runInteractively(pgp(startRun, printPopulationSize, clickToContinue), Config, !IO)
		;
		Config^selectedGame = 'pgp+pa',
		data.config.runInteractively('pgp+pa'(startRun, printPopulationSize, clickToContinue), Config, !IO)
		;
		Config^selectedGame = ultimatum,
		data.config.runInteractively(ultimatum(startRun, printPopulationSize, clickToContinue), Config, !IO)
	)
	else
		io.print(io.stderr_stream, "Could not launch window\n", !IO)
	).



:- pragma foreign_decl("C",
"
#include ""plot-population.h""
							 
").

:- pred launchWindow(io.state, io.state).
:- mode launchWindow(di, uo) is cc_multi.


:- pragma foreign_proc(
	"C",
	launchWindow(IOdi::di, IOuo::uo),
	[will_not_call_mercury, thread_safe, promise_pure],
	"
		IOdi = IOuo;
		initGraphicsWindow ();
	"
	).

:- type plotData --->
	plotData(
		energy :: float,
		age    :: int
	).

:- pred printPopulationSize(population(C, T), bool, io.state, io.state).
:- mode printPopulationSize(in, out, di, uo) is det.

printPopulationSize(Population, no, !IO) :-
	Add =
	(func(P, AC) = R :-
		R^energy = AC^energy + P^traits^energyTrait,
		R^age = AC^age + P^traits^ageTrait
	),
	PopulationSize = float(ebea.population.size(Population)),
	ebea.population.fold(Add, Population, plotData(0.0, 0)) = PlotData,
	PlotData^energy / PopulationSize = AverageEnergy,
	float(PlotData^age) / PopulationSize = AverageAge,
	addPoints(PopulationSize, AverageEnergy, AverageAge, !IO).


:- pred addPoints(float, float, float, io.state, io.state).
:- mode addPoints(in, in, in, di, uo) is det.


:- pragma foreign_proc(
	"C",
	addPoints(PopulationSize::in, AverageEnergy::in, AverageAge::in, IOdi::di, IOuo::uo),
	[will_not_call_mercury, thread_safe, promise_pure],
	"
		IOdi = IOuo;
		addPoints (PopulationSize, AverageEnergy, AverageAge);
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
