/**
 * Main module of the java version of the Energy Based Evolutionary
 * Algorithm Tool Kit.  In this application the user can edit the
 * parameters of an EBEA simulation.  These parameters are represented by
 * type {@code config} from module {@code file.config}.

 * @author Pedro Mariano
 * @version 1.0 2013/12/ 7
 */
:- module 'EBEAtk_swing'.

:- interface.

:- import_module io.

:- pred main(io.state, io.state).
:- mode main(di, uo) is det.

:- implementation.

:- import_module ui_swing, userInterface.
:- import_module data, data.config, data.config.io, data.config.pretty.
:- import_module tools, tools.export_playerProfiles_graphviz, tools.'PCVNetwork', tools.populationDynamics.
:- import_module int, dir, list, maybe, string, time.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

:- type data --->
	data(
		config                   :: data.config.config,
		parameters_playerProfile :: tools.export_playerProfiles_graphviz.parameters,
		parameters_PCV           :: tools.'PCVNetwork'.parameters,
		parameters_PD            :: tools.populationDynamics.parameters,
		filename                 :: maybe(string),
		fileChooser              :: fileChooser
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

:- type fileChooser.

:- pragma foreign_type("Java", fileChooser, "javax.swing.JFileChooser").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

main(!IO) :-
	initData(Data, !IO),
	ui_swing.show("EBEAtk", menu, Data, _, !IO).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

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
			[mi(label("Run"),			actionDataIO(playerProfilesMovie)),
			 mi(label("Config"),    edit('new dialog'(parameters_playerProfile, set('parameters_playerProfile :='), tools.export_playerProfiles_graphviz.dialog_parameters)))
			])),
		 mi(label("Probability combination vectors movie"),   submenu(
			[mi(label("Run"),			actionDataIO('PCVMovie')),
			 mi(label("Config"),    edit('new dialog'(parameters_PCV, set('parameters_PCV :='), tools.'PCVNetwork'.dialog_parameters)))
			])),
		 mi(label("Population dynamics"),   submenu(
			[mi(label("Run"),			actionDataIO(populationDynamics)),
			 mi(label("Config"),    edit('new dialog'(parameters_PD, set('parameters_PD :='), tools.populationDynamics.dialog_parameters)))
			]))
		])),
	 mi(label("Run background"),       actionDataIO('EBEAtk_swing'.runBackground))
	]).

:- pred initData(data, io.state, io.state).
:- mode initData(out, di, uo) is det.

initData(Data, !IO) :-
	dir.current_directory(RDir, !IO),
	(
		RDir = ok(Dir),
		Data = initData(setCurrentDirectoryFileChooser(Dir, initFileChooser))
		;
		RDir = error(Error),
		io.format(io.stderr_stream, "IO error obtaining current directory: %s", [s(io.error_message(Error))], !IO),
		Data = initData(initFileChooser)
	).

:- func initData(fileChooser) = data.

initData(FileChooser) = data(
	data.config.default,
	tools.export_playerProfiles_graphviz.default_parameters,
	tools.'PCVNetwork'.default_parameters,
	tools.populationDynamics.default_parameters,
	no,
	FileChooser).

:- func config(data) = data.config.config.
:- func 'config :='(data, data.config.config) = data.

:- func parameters_playerProfile(data) = tools.export_playerProfiles_graphviz.parameters.
:- func 'parameters_playerProfile :='(data, tools.export_playerProfiles_graphviz.parameters) = data.

:- func parameters_PCV(data) = tools.'PCVNetwork'.parameters.
:- func 'parameters_PCV :='(data, tools.'PCVNetwork'.parameters) = data.

:- func parameters_PD(data) = tools.populationDynamics.parameters.
:- func 'parameters_PD :='(data, tools.populationDynamics.parameters) = data.



/**
 * Menu option.
 */
:- pred loadConfiguration(data, data, io.state, io.state).
:- mode loadConfiguration(in, out, di, uo) is det.

loadConfiguration(!Data, !IO) :-
	selectOpenFile(!.Data^fileChooser) = MFilename,
	(
		MFilename = yes(Filename),
		(if
			string.remove_suffix(Filename, ".txt", _)
		then
			data.config.io.read(mercury, Filename, MConfig, !IO)
		else if
			string.remove_suffix(Filename, ".ebea", _)
		then
			data.config.io.read(binary, Filename, MConfig, !IO)
		else
			data.config.io.read(Filename, MConfig, !IO)
		),
		(
			MConfig = ok(Config),
			!:Data = 'config :='(
				'filename :='(!.Data, yes(Filename)),
				Config)
			;
			MConfig = error(Msg),
			io.print(Msg, !IO),
			io.nl(!IO)
		)
		;
		MFilename = no
	).

/**
 * Menu option.
 */
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

/**
 * Menu option.
 */
:- pred saveAsConfiguration(data, data, io.state, io.state).
:- mode saveAsConfiguration(in, out, di, uo) is det.

saveAsConfiguration(!Data, !IO) :-
	selectSaveFile(!.Data^fileChooser) = MFilename,
	(
		MFilename = yes(CheckFilename),
		(if
			string.remove_suffix(CheckFilename, ".ebea", _)
		then
			FinalFilename = CheckFilename,
			data.config.io.write(binary, FinalFilename, !.Data^config, MErrors, !IO)
		else if
			string.remove_suffix(CheckFilename, ".txt", _)
		then
			FinalFilename = CheckFilename,
			data.config.io.write(mercury, FinalFilename, !.Data^config, MErrors, !IO)
		else
			FinalFilename = CheckFilename ++ ".ebea",
			data.config.io.write(binary, FinalFilename, !.Data^config, MErrors, !IO)
		),
		(
			MErrors = no
			;
			MErrors = yes(Errors),
			io.print(Errors, !IO),
			io.nl(!IO)
		),
		!:Data = 'filename :='(!.Data, yes(FinalFilename))
		;
		MFilename = no
	)
	.


/**
 * Menu option.
 */
:- pred playerProfilesMovie(data, io.state, io.state).
:- mode playerProfilesMovie(in, di, uo) is det.

playerProfilesMovie(Data, !IO) :-
	% time.time(Time, !IO),
	% TM = time.localtime(Time),
	% FilenamePrefix = string.format("movie_%4d-%02d-%02d_%02d:%02d",
	% 	[i(TM^tm_year + 1900), i(TM^tm_mon + 1), i(TM^tm_mday),
	% 	 i(TM^tm_hour), i(TM^tm_sec)]),
	createPlayerProfilesNetworks(Data^config, Data^parameters_playerProfile, "./", Feedback, !IO),
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
:- pred runBackground(data, io.state, io.state).
:- mode runBackground(in, di, uo) is det.

runBackground(Data, !IO) :-
	time.time(Time, !IO),
	TM = time.localtime(Time),
	Filename = string.format("config_run_%4d-%02d-%02d_%02d:%02d.ebea",
		[i(TM^tm_year + 1900), i(TM^tm_mon + 1), i(TM^tm_mday),
		 i(TM^tm_hour), i(TM^tm_sec)]),
	data.config.io.write(binary, Filename, Data^config, MErrors, !IO),
	(
		MErrors = yes(Msg),
		io.format(io.stderr_stream, "Could not write configuration run file:\n%s\n", [s(Msg)], !IO)
		;
		MErrors = no
	),
	data.config.runBackground(Data^config, !IO).



/**
 * Menu option.
 */
:- pred printConfiguration(data, io.state, io.state).
:- mode printConfiguration(in, di, uo) is det.

printConfiguration(Data, !IO) :-
	data.config.pretty.print(io.stdout_stream, plain, Data^config, !IO)
	.

:- func setCurrentDirectoryFileChooser(string, fileChooser) = fileChooser.

:- pragma foreign_proc(
	"Java",
	setCurrentDirectoryFileChooser(CurrentDirectory::in, FileChooser::in) = (Result::out),
	[will_not_call_mercury, promise_pure],
	"
	FileChooser.setCurrentDirectory (new java.io.File (CurrentDirectory));
	Result = FileChooser;
	").


:- func initFileChooser = fileChooser.

:- pragma foreign_proc(
	"Java",
	initFileChooser = (Result::out),
	[will_not_call_mercury, promise_pure],
	"
	Result = new javax.swing.JFileChooser ();
	Result.setAcceptAllFileFilterUsed (false);
	Result.addChoosableFileFilter (new javax.swing.filechooser.FileFilter () {
		@Override
		public boolean accept (java.io.File f) {
			return f.isDirectory () || f.getName ().endsWith ("".ebea"");
		}

		@Override
		public String getDescription () {
			return ""EBEA configuration file (*.ebea)"";
		}
	});
	Result.addChoosableFileFilter (new javax.swing.filechooser.FileFilter () {

		@Override
		public boolean accept (java.io.File f) {
			return f.isDirectory () || f.getName ().endsWith ("".txt"");
		}

		@Override
		public String getDescription () {
			return ""Mercury term file (*.txt)"";
		}
	});
	"
	).

:- func selectOpenFile(fileChooser) = maybe(string).

:- pragma foreign_proc(
	"Java",
	selectOpenFile(FileChooser::in) = (Result::out),
	[will_not_call_mercury, promise_pure],
	"
	switch (FileChooser.showOpenDialog (null)) {
		case javax.swing.JFileChooser.APPROVE_OPTION:
			Result = new maybe.Maybe_1.Yes_1<String> (FileChooser.getSelectedFile ().getPath ());
			break;
		case javax.swing.JFileChooser.CANCEL_OPTION:
			Result = new maybe.Maybe_1.No_0<String> ();
			break;
		case javax.swing.JFileChooser.ERROR_OPTION:
			Result = new maybe.Maybe_1.No_0<String> ();
			break;
		default:
			throw new IllegalStateException (""never reached"");
	}
	"
	).
:- func selectSaveFile(fileChooser) = maybe(string).

:- pragma foreign_proc(
	"Java",
	selectSaveFile(FileChooser::in) = (Result::out),
	[will_not_call_mercury, promise_pure],
	"
	switch (FileChooser.showSaveDialog (null)) {
		case javax.swing.JFileChooser.APPROVE_OPTION:
			Result = new maybe.Maybe_1.Yes_1<String> (FileChooser.getSelectedFile ().getPath ());
			break;
		case javax.swing.JFileChooser.CANCEL_OPTION:
			Result = new maybe.Maybe_1.No_0<String> ();
			break;
		case javax.swing.JFileChooser.ERROR_OPTION:
			Result = new maybe.Maybe_1.No_0<String> ();
			break;
		default:
			throw new IllegalStateException (""never reached"");
	}
	"
	).

:- end_module 'EBEAtk_swing'.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
