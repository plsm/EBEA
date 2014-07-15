/**
 * Create a text file with population dynamics: population size, number of
 * births, number of deaths per cause, strategy parameters
 * characterisation.  The 

 * @author Pedro Mariano
 * @version 1.0 2014/03/19
 */
:- module tools.populationDynamics.

:- interface.

:- import_module data, data.config.
:- import_module userInterface.
:- import_module io, list.

:- type parameters --->
	parameters(
		runs                  :: list(int) ,
		fileNamePrefix        :: string
	).

/**
 * Return a default value of {@code parameters}.
 */
:- func default_parameters = tools.populationDynamics.parameters.

/**
 * dialog_parameters = Parameters
  
 * The logical specification of the user dialog to edit the parameters of
 * the tool that creates gnuplot and text files with population dynamics.
  
 */
:- func dialog_parameters = list(dialogItem(tools.populationDynamics.parameters)).

:- pred runTool(data.config.config, tools.populationDynamics.parameters, string, io.state, io.state).
:- mode runTool(in, in, out, di, uo) is det.

:- func runs(tools.populationDynamics.parameters) = list(int).

:- func fileNamePrefix(tools.populationDynamics.parameters) = string.
:- func 'fileNamePrefix :='(tools.populationDynamics.parameters, string) = tools.populationDynamics.parameters.

:- implementation.

:- import_module parseable, parseable.iou.
:- import_module game.
:- import_module data.util.
:- import_module ebea, ebea.player, ebea.player.selection, ebea.player.selection.pcv, ebea.population, ebea.population.parameters, ebea.streams, ebea.streams.birth, ebea.streams.phenotype.
:- import_module array, exception, int, maybe, set, solutions, string.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

default_parameters = parameters(
	[],
	"population-dynamics"
	).

dialog_parameters =
	[
	di(label("runs to process"),          updateListFieldInt(  runs,                   set_runs)),
	di(label("filename prefix"),          updateFieldString(   fileNamePrefix,         set('fileNamePrefix :=')))
	].

runTool(Data, Parameters, Feedback, !IO) :-
	data.util.gameConfig(Data) = data.util.gcex(Game, _, InitialPopulation),
	Level = Data^level,
	(	%
		Level = detailedBin,
		Runs = Parameters^runs,
		(
			Runs = [],
			RunIndexes = set.from_list(1..Data^numberRuns)
			;
			Runs = [_ | _],
			RunIndexes = set.intersect(set.from_list(1..Data^numberRuns), set.from_list(Runs))
		),
		set.fold2(
			create_populationDynamics_forRun_s1(Data, Game, InitialPopulation, Parameters),
			RunIndexes,
			[], FeedbackAsList,
			!IO),
		(
			FeedbackAsList = [],
			Feedback = "ok"
			;
			FeedbackAsList = [_|_],
			Feedback = string(FeedbackAsList)
		)
		;
		Level = detailedTxt,
		Feedback = "Parsing of detailed text files is not supported"
		;
		Level = dynamics,
		Feedback = "The simulation runs already produced population dynamics"
		;
		Level = summary,
		Feedback = "The simulation runs did not produce data to recreate population dynamics"
	).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

/**
 * opens the streams for reading and if no error occurs, continues to predicate
 * {@code createProbabilityCombinationVectorsNetworksForRun_s2/9}
 
 */
:- pred create_populationDynamics_forRun_s1(
	data.config.config            :: in,
	G                             :: in,
	ebea.population.parameters.parameters(C) :: in,
	tools.populationDynamics.parameters :: in,
	int                           :: in,
	list(string) :: in, list(string) :: out,
	io.state     :: di, io.state     :: uo
) is det <= (
	asymmetricGame(G, C),
	parseable(C),
	printable(C)
).

create_populationDynamics_forRun_s1(
	Data,
	Game,
	InitialPopulation,
	Parameters,
	RunIndex,
	!FeedbackAsList,
	!IO
) :-
	ebea.streams.openInputStreams(Data^level, yes(string.format("_R%d", [i(RunIndex)])), IMStreams, !IO),
	(
		IMStreams = ok(S),
		(if
			S = detailedBin(_, _, _, _)
		then
			Streams = S
		else
			throw("Never reached")
		),
		io.format("Run %d\n", [i(RunIndex)], !IO),
		create_populationDynamics_forRun_s2(
			Data,
			Game,
			InitialPopulation,
			Parameters,
			Streams,
			RunIndex,
			!FeedbackAsList,
			!IO),
		ebea.streams.closeInputStreams(Streams, !IO),
		io.print("\r                      \n", !IO)
		;
		IMStreams = error(ErrorMsg),
		list.cons(ErrorMsg, !FeedbackAsList)
	)
	.




:- pred create_populationDynamics_forRun_s2(
	data.config.config                       :: in,
	G                                        :: in,
	ebea.population.parameters.parameters(C) :: in,
	tools.populationDynamics.parameters            :: in,
	ebea.streams.inStreams                   :: in(detailedBin),
	int                                      :: in,
	list(string)           :: in, list(string) :: out,
	io.state               :: di, io.state     :: uo
) is det <= (
	asymmetricGame(G, C),
	parseable(C),
	printable(C)
).

create_populationDynamics_forRun_s2(
	Data,
	Game,
	InitialPopulation,
	Parameters,
	BinStreams,
	RunIndex,
	!FeedbackAsList,
	!IO
) :-
	FileName = string.format("%s_R%d.txt",
		[s(Parameters^fileNamePrefix),
		 i(RunIndex)]),
	io.open_output(FileName, IStream, !IO),
	(	% switch IStream
		IStream = ok(TextDataStream),
		create_populationDynamics_forRun_s3(
			Data,
			Game,
			InitialPopulation,
			Parameters,
			BinStreams,
			TextDataStream,
			RunIndex,
			!FeedbackAsList,
			!IO),
		io.close_output(TextDataStream, !IO)
	;	
		IStream = error(Error),
		list.cons(string.format("IO error opening `%s` file: %s", [s(FileName), s(io.error_message(Error))]), !FeedbackAsList)
	).



:- pred create_populationDynamics_forRun_s3(
	data.config.config                       :: in,
	G                                        :: in,
	ebea.population.parameters.parameters(C) :: in,
	tools.populationDynamics.parameters      :: in,
	ebea.streams.inStreams                   :: in(detailedBin),
	io.output_stream                         :: in,
	int                                      :: in,
	list(string) :: in, list(string) :: out,
	io.state     :: di, io.state     :: uo
) is det <= (
	asymmetricGame(G, C),
	parseable(C),
	printable(C)
).

create_populationDynamics_forRun_s3(
	Data,
	Game,
	InitialPopulation,
	Parameters,
	BinStreams,
	TextDataStream,
	RunIndex,
	!FeedbackAsList,
	!IO
) :-
	true.








:- func set_runs(tools.populationDynamics.parameters, list(int)) = userInterface.setResult(tools.populationDynamics.parameters).

set_runs(P, V) = Result :-
	(if
		list.append(_, [H | T], V),
		list.append(_, [H | _], T)
	then
		Result = error("Duplicate run number")
	else if
		list.member(Run, V),
		Run =< 0
	then
		Result = error("Runs indexes must be positive")
	else
		Result = ok('runs :='(P, V))
	).

:- end_module tools.populationDynamics.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
