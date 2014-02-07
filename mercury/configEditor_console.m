/**
 * Main module of the editor of a EBEA simulation parameters.  These
 * parameters are represented by type {@code config} defined in module
 * {@code file.config}.

 * @author Pedro Mariano
 * @version 1.0 2013/12/ 7
 */
:- module configEditor_console.

:- interface.

:- import_module io.

:- pred main(io.state, io.state).
:- mode main(di, uo) is cc_multi.

:- implementation.

:- import_module data, data.config, data.config.io.
:- import_module userInterface.
:- import_module ui_console.

:- import_module maybe, thread, list.

:- import_module random, ebea, ebea.core, ebea.population, ebea.player, ebea.player.selection, ebea.player.selection.opinion.
:- import_module bool, float, int.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

main(!IO) :-
	io.command_line_arguments(Args, !IO),
	(if
		Args = [Filename]
	then
		data.config.io.read(Filename, MConfig, !IO),
		(
			MConfig = ok(Config),
			runInteractively(Config, !IO)
			;
			MConfig = error(Msg),
			io.print(Msg, !IO),
			io.nl(!IO)
		)
	else
		
	% random.init(234, Rnd),
	% ebea.player.selection.opinion.debug(Rnd, _, !IO),
	
		data.config.io.read("config.txt", MConfig, !IO),
		(
			MConfig = ok(Config)
			;
			MConfig = error(Msg),
			io.print(Msg, !IO),
			io.nl(!IO),
			Config = data.config.default
		),
		ui_console.show(d(data.config.dialog), Config, Result2, !IO),
		data.config.io.write("config.txt", Result2, MErrors, !IO),
		(
			MErrors = no
			;
			MErrors = yes(Errors),
			io.print(Errors, !IO),
			io.nl(!IO)
		),
		data.config.runBackground(Result2, !IO)
	)
	.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- pred runInteractively(data.config.config, io.state, io.state).
:- mode runInteractively(in, di, uo) is cc_multi.

runInteractively(Config, !IO) :-
	(if
		thread.can_spawn
	then
		thread.spawn(launchWindow, !IO),
		io.print("Launched window\nPress ENTER to run", !IO),
		io.read_line_as_string(_, !IO),
		data.config.runInteractively(pgp(startRun, printPopulationSize, clickToContinue), Config, !IO)
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

:- end_module configEditor_console.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
