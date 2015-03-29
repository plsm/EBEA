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
:- import_module tools.
:- import_module tools.export_playerProfiles_graphviz.
:- import_module tools.'PCVNetwork'.
:- import_module tools.populationDynamics.
:- import_module tools.siteDynamics.
:- import_module tools.processPhenotype.

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
		config                   :: data.config.config,
		parameters_playerProfile :: tools.export_playerProfiles_graphviz.parameters,
		parameters_PCV           :: tools.'PCVNetwork'.parameters,
		parameters_PD            :: tools.populationDynamics.parameters,
		parameters_SD            :: tools.siteDynamics.parameters,
		parameters_PP            :: tools.processPhenotype.parameters,
		filename                 :: maybe(string)
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


:- pred run('EBEAtk_console'.runMode, maybe(string), io.state, io.state).
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
		data.config.runEBEA(background, Config, !IO)
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
			Data = initData(Config, Filename)
			;
			MConfig = error(Msg),
			io.print(Msg, !IO),
			io.nl(!IO),
			Data = initData(data.config.default, Filename)
		)
	),
	ui_console.show(menu, Data, _, !IO)
	.


:- func menu = userInterface(data).
:- mode menu = out(userInterface).

menu = m(
	[mi(label("Configuration"),  submenu(
		[mi(label("Edit"),     edit('new dialog'(config, set('config :='), data.config.dialog))),
		 mi(label("Load"),     updateDataIO(loadConfiguration)),
		 mi(label("Save"),     updateDataIO(saveConfiguration)),
		 mi(label("Save As"),  updateDataIO(saveAsConfiguration)),
		 mi(label("Print"),    actionDataIO(printConfiguration))
		])),
	 mi(label("Tools"), submenu(
		[mi(label("Player profiles movie"),   submenu(
			[mi(label("Run"),       actionDataIO(playerProfilesMovie)),
			 mi(label("Config"),    edit('new dialog'(parameters_playerProfile, set('parameters_playerProfile :='), tools.export_playerProfiles_graphviz.dialog_parameters)))
			])),
		 mi(label("Process players' phenotype"),   submenu(
			[mi(label("Run"),       actionDataIO(processPhenotype)),
			 mi(label("Config"),    edit('new dialog'(parameters_PP, set('parameters_PP :='), tools.processPhenotype.dialog_parameters)))
			])),
		 mi(label("Probability combination vectors movie"),   submenu(
			[mi(label("Run"),			actionDataIO('PCVMovie')),
			 mi(label("Config"),    edit('new dialog'(parameters_PCV, set('parameters_PCV :='), tools.'PCVNetwork'.dialog_parameters)))
			])),
		 mi(label("Site dynamics"),   submenu(
			[mi(label("Run"),			actionDataIO(siteDynamics)),
			 mi(label("Config"),    edit('new dialog'(parameters_SD, set('parameters_SD :='), tools.siteDynamics.dialog_parameters)))
			])),
		 mi(label("Population dynamics"),   submenu(
			[mi(label("Run"),			actionDataIO(populationDynamics)),
			 mi(label("Config"),    edit('new dialog'(parameters_PD, set('parameters_PD :='), tools.populationDynamics.dialog_parameters)))
			]))
		])),
	 mi(label("Run background"),       actionDataIO('EBEAtk_console'.runBackground))
	]).


:- func initData = data.

initData = data(
	data.config.default,
	tools.export_playerProfiles_graphviz.default_parameters,
	tools.'PCVNetwork'.default_parameters,
	tools.populationDynamics.default_parameters,
	tools.siteDynamics.default_parameters,
	tools.processPhenotype.default_parameters,
	no
).

:- func initData(data.config.config, string) = data.

initData(Config, Filename) = data(
	Config,
	tools.export_playerProfiles_graphviz.default_parameters,
	tools.'PCVNetwork'.default_parameters,
	tools.populationDynamics.default_parameters,
	tools.siteDynamics.default_parameters,
	tools.processPhenotype.default_parameters,
	yes(Filename)
).

:- func config(data) = data.config.config.
:- func 'config :='(data, data.config.config) = data.

:- func parameters_playerProfile(data) = tools.export_playerProfiles_graphviz.parameters.
:- func 'parameters_playerProfile :='(data, tools.export_playerProfiles_graphviz.parameters) = data.

:- func parameters_PCV(data) = tools.'PCVNetwork'.parameters.
:- func 'parameters_PCV :='(data, tools.'PCVNetwork'.parameters) = data.

:- func parameters_PD(data) = tools.populationDynamics.parameters.
:- func 'parameters_PD :='(data, tools.populationDynamics.parameters) = data.

:- func parameters_SD(data) = tools.siteDynamics.parameters.
:- func 'parameters_SD :='(data, tools.siteDynamics.parameters) = data.

:- func parameters_PP(data) = tools.processPhenotype.parameters.
:- func 'parameters_PP :='(data, tools.processPhenotype.parameters) = data.



% :- func get_config(data) = data.config.config.

% get_config(D) = D^config.

% :- func set_config(data, data.config.config) = setResult(data).

% set_config(D, C) = ok('config :='(D, C)).

:- pred loadConfiguration(data, data, io.state, io.state).
:- mode loadConfiguration(in, out, di, uo) is det.

loadConfiguration(!Data, !IO) :-
	io.print("File name? ", !IO),
	io.flush_output(io.stdout_stream, !IO),
	io.read_line_as_string(ILine, !IO),
	(if
		ILine = ok(Line)
	then
		Filename = string.strip(Line),
		data.config.io.read(Filename, MConfig, !IO),
		(
			MConfig = ok(Config),
			!:Data = initData(Config, Filename),
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
	io.flush_output(io.stdout_stream, !IO),
	io.read_line_as_string(ILine, !IO),
	(if
		ILine = ok(Line)
	then
		Filename = data.config.io.standardFileNameExtension(string.strip(Line)),
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
	data.config.runEBEA(background, Data^config, !IO).




/**
 * Menu option.
 */
:- pred playerProfilesMovie(data, io.state, io.state).
:- mode playerProfilesMovie(in, di, uo) is det.

playerProfilesMovie(Data, !IO) :-
	createPlayerProfilesNetworks(Data^config, Data^parameters_playerProfile, "./", Feedback, !IO),
	io.print(Feedback, !IO),
	io.nl(!IO)
	.

/**
 * Menu option.
 */
:- pred processPhenotype(data, io.state, io.state).
:- mode processPhenotype(in, di, uo) is det.

processPhenotype(Data, !IO) :-
	tools.processPhenotype.run(
		Data^config,
		Data^parameters_PP,
		"./",
		Feedback,
		!IO),
	io.print(Feedback, !IO),
	io.nl(!IO)
	.

/**
 * Menu option.
 */
:- pred 'PCVMovie'(data, io.state, io.state).
:- mode 'PCVMovie'(in, di, uo) is det.

'PCVMovie'(Data, !IO) :-
	tools.'PCVNetwork'.runTool(Data^config, Data^parameters_PCV, "./", Feedback, !IO),
	io.print(Feedback, !IO),
	io.nl(!IO)
	.

/**
 * Menu option.
 */
:- pred populationDynamics(data, io.state, io.state).
:- mode populationDynamics(in, di, uo) is det.

populationDynamics(Data, !IO) :-
	tools.populationDynamics.runTool(Data^config, Data^parameters_PD, "./", Feedback, !IO),
	io.print(Feedback, !IO),
	io.nl(!IO)
	.

/**
 * Menu option.
 */
:- pred siteDynamics(data, io.state, io.state).
:- mode siteDynamics(in, di, uo) is det.

siteDynamics(Data, !IO) :-
	tools.siteDynamics.runTool(Data^config, Data^parameters_SD, "./", Feedback, !IO),
	io.print(Feedback, !IO),
	io.nl(!IO)
	.


% :- type plotData --->
% 	plotData(
% 		energy :: float,
% 		age    :: int
% 	).


:- end_module 'EBEAtk_console'.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
