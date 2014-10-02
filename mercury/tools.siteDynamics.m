/**
 * This tool reads the site's state and produce text files with site's
 * state.

 * @author Pedro Mariano
 * @version 1.0 2014/10/ 2
 */
:- module tools.siteDynamics.

:- interface.

:- import_module data, data.config.
:- import_module userInterface.
:- import_module io, list.

%% **********************************************************
%% Parameters that control the behaviour of state dynamics tool.
:- type parameters --->
	parameters(
		runs                  :: list(int) ,
		fileNamePrefix        :: string
	).

%% **************************************************************************
%% default_parameters = Result
%%
%% Return a default value of {@code parameters}.
%%
:- func default_parameters = tools.siteDynamics.parameters.

%% **************************************************************************
%% dialog_parameters = Parameters
%%
%% The logical specification of the user dialog to edit the parameters of
%% the tool that creates text files with state dynamics.
%%
:- func dialog_parameters = list(dialogItem(tools.siteDynamics.parameters)).

%% **************************************************************************
%% runTool(Config, Parameters, Directory, Feedback, !IO)
%%
%% Run the state dynamics tool.
%%
:- pred runTool(
	data.config.config             :: in,
	tools.siteDynamics.parameters :: in,
	string                         :: in,
	string :: out,
	io.state :: di,  io.state :: uo
) is det.


%% **************************************************************************
%%
%%
%% Field {@code runs} get function.
%%
:- func runs(tools.siteDynamics.parameters) = list(int).

%% **************************************************************************
%%
%%
%% Field {@code fileNamePrefixruns} get function.
%%
:- func fileNamePrefix(tools.siteDynamics.parameters) = string.

%% **************************************************************************
%%
%%
%% Field {@code fileNamePrefix} set function.
%%
:- func 'fileNamePrefix :='(tools.siteDynamics.parameters, string) = tools.siteDynamics.parameters.

:- implementation.

:- import_module ebea, ebea.population, ebea.population.site, ebea.streams, ebea.streams.siteState.
:- import_module parseable, parseable.iou.
:- import_module exception, int, maybe, set, string.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

default_parameters = parameters(
	[],
	"state-dynamics"
	).

dialog_parameters =
	[
	di(label("runs to process"),          updateListFieldInt(  runs,                   set_runs)),
	di(label("filename prefix"),          updateFieldString(   fileNamePrefix,         set('fileNamePrefix :=')))
	].

runTool(Data, Parameters, Directory, Feedback, !IO) :-
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
			create_siteDynamics_forRun_s1(Data, Parameters, Directory),
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


%% **************************************************************************
%% Opens the files generated associated with the given run number.
%%
%% <p> First step for creating the site dynamics text file.
%%
:- pred create_siteDynamics_forRun_s1(
	data.config.config                                 :: in,
	tools.siteDynamics.parameters                     :: in,
	string                                             :: in,
	int                                                :: in,
	list(string) :: in, list(string) :: out,
	io.state     :: di, io.state     :: uo
) is det.

create_siteDynamics_forRun_s1(
	Data,
	Parameters,
	Directory,
	RunIndex,
	!FeedbackAsList,
	!IO
) :-
	MSuffix = yes(string.format("_R%d", [i(RunIndex)])),
	ebea.streams.openInputStreams(Directory, Data^level, MSuffix, IMStreams, !IO),
	(
		IMStreams = ok(S),
		(if
			S = detailedBin(_, _, _, _, _)
		then
			Streams = S
		else
			throw("Never reached")
		),
		io.format("Run %d\n", [i(RunIndex)], !IO),
		create_siteDynamics_forRun_s2(
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

%% **************************************************************************
%% Opens in write mode the text file with the site dynamics.
%%
%% <p> Second step for creating the site dynamics text file.
%%
:- pred create_siteDynamics_forRun_s2(
	tools.siteDynamics.parameters :: in,
	ebea.streams.inStreams         :: in(detailedBin),
	int                            :: in,
	list(string) :: in, list(string) :: out,
	io.state     :: di, io.state     :: uo
) is det.

create_siteDynamics_forRun_s2(
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
		create_populationDynamics_forRunIteration(
			BinStreams,
			TextDataStream,
			0,
			parseable.iou.cacheInit,  FinalSiteStateCache,
			!FeedbackAsList,
			!IO
		),
		io.nl(!IO),
		io.print(FinalSiteStateCache, !IO),
		io.nl(!IO),
		io.close_output(TextDataStream, !IO)
	;	
		IStream = error(Error),
		list.cons(
			string.format("IO error opening `%s` file: %s", [s(FileName), s(io.error_message(Error))]),
			!FeedbackAsList)
	).


:- pred create_populationDynamics_forRunIteration(
	ebea.streams.inStreams :: in(detailedBin),
	io.output_stream       :: in,
	int                    :: in,
	parseable.iou.cache :: in, parseable.iou.cache :: out,
	list(string)        :: in, list(string)        :: out,
	io.state            :: di, io.state            :: uo
) is det.

create_populationDynamics_forRunIteration(
	BinStreams,
	TextDataStream,
	IterationIndex,
	!SiteStateCache,
	!FeedbackAsList,
	!IO
) :-
	ebea.streams.siteState.read(
		BinStreams^bisSiteState,
		RIIterationSiteStateRecord,
		!SiteStateCache,
		!IO),
	(	%
		RIIterationSiteStateRecord = parseError,
		list.cons("Parse error occurred while reading site state dynamics file", !FeedbackAsList)
	;
		RIIterationSiteStateRecord = ok(error(Error)),
		list.cons(
			string.format("IO error reading site state dynamics file: %s", [s(io.error_message(Error))]),
			!FeedbackAsList)
	;
		RIIterationSiteStateRecord = ok(eof)
	;
		RIIterationSiteStateRecord = ok(ok(IterationSiteStateRecord)),
		io.print(TextDataStream, IterationIndex, !IO),
		PredPrint =
		(pred(SiteState::in, !.IO1::di, !:IO1::uo) is det :-
			io.print(TextDataStream, '\t', !IO1),
			io.print(TextDataStream, SiteState^state^carryingCapacity, !IO1)
		),
		list.foldl(
			PredPrint,
			IterationSiteStateRecord^sitesState,
			!IO),
		io.nl(TextDataStream, !IO),
		create_populationDynamics_forRunIteration(
			BinStreams,
			TextDataStream,
			IterationIndex + 1,
			!SiteStateCache,
			!FeedbackAsList,
			!IO)
	)
	.

%% **************************************************************************
%% Field {@code runs} set function used by the user interface.
%%
:- func set_runs(tools.siteDynamics.parameters, list(int)) = userInterface.setResult(tools.siteDynamics.parameters).

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

:- end_module tools.siteDynamics.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
