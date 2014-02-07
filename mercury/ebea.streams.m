/**
 * Provides a type that represent the streams where data of Energy Based
 * Evolutionary Algorithm runs are saved.

 * @author Pedro Mariano
 * @version 1.0 2013/05/21
 */
:- module ebea.streams.

:- interface.

:- import_module userInterface, parseable.
:- import_module io, list, maybe.

/**
 * Streams where data is written:
  
 * <ul>

 * <li>{@code sbirth} contains information about player births namely round
 * number, player id and his chromosome.</li>

 * <li>{@code sdeath} contains information about player deaths namely round
 * number and player id.</li>

 * <li>{@code sphenotype} contains information about the current state of
 * each player namely round number, player id and player phenotype.</li>

 * </ul>
 */
:- type outStreams --->
	detailed(
		sobirth         :: io.output_stream,
		sodeath         :: io.output_stream,
		sophenotype     :: io.output_stream,
		soplayerProfile :: io.output_stream
	) ;
	dynamics(
		sopopulation :: io.output_stream
	) ;
	summary(
		sosummary    :: io.output_stream
	).

:- type inStreams --->
	detailed(
		sibirth      :: io.input_stream,
		sideath      :: io.input_stream,
		siphenotype  :: io.input_stream
	) ;
	dynamics(
		sipopulation :: io.input_stream
	) ;
	summary(
		sisummary    :: io.input_stream
	).

:- inst detailed == bound(detailed(ground, ground, ground)).

:- inst dynamics == bound(dynamics(ground)).

:- inst summary == bound(summary(ground)).

/**
 * Represents the level of detail of the output streams used by an Energy
 * Based Evolutionary Algorithm run.
 */
:- type level --->
	detailed ;
	dynamics ;
	summary.

:- instance parseable(level).

% /**
%  * scanStreams(Stream, IMStreams, !IO)
  
%  * Open the streams where statistical is going to be recorded.  If there is
%  * an IO error while opening the streams, parameter {@code MStreams} is
%  * unified with {@code no}.
  
%  */
% :- pred scanStreams(io.input_stream, scanable.result(outStreams), io.state, io.state).
% :- mode scanStreams(in, out, di, uo) is det.

% /**
%  * Scans the text file for the level of detail of the streams used or to be
%  * used by an EBEA run.
%  */

% :- pred scanLevel(io.input_stream, scanable.result(level), io.state, io.state).
% :- mode scanLevel(in, out, di, uo) is det.

/**
 * openOutputStreams(Level, MSuffix, IMStreams, !IO)
  
 * The streams used by EBEA can only be open for output through reading a
 * configuration file.  Flag {@code Level} indicates if every information
 * should be recorded, only a resume per iteration or just a summary of the
 * entire EBEA run.
 */

:- pred openOutputStreams(level, maybe(string), maybe_error(outStreams), io.state, io.state).
:- mode openOutputStreams(in, in, out, di, uo) is det.

/**
 * Closes the streams used by an Energy Based Evolutionary Algorithm run.
 */
:- pred closeOutputStreams(outStreams, io, io).
:- mode closeOutputStreams(in, di, uo) is det.

/**
 * Opens streams in input mode used by EBEA at the specified level.
 */
:- pred openInputStreams(level, maybe_error(inStreams), io.state, io.state).
:- mode openInputStreams(in, out, di, uo) is det.

/**
 * Closes the streams used by an Energy Based Evolutionary Algorithm run.
 */
:- pred closeInputStreams(inStreams, io, io).
:- mode closeInputStreams(in, di, uo) is det.

/**
 * Return a user dialog where an user can select the amount of data that is
 * written by an EBEA run.
 */
:- func dialog = dialog(level).

:- pred parse(level, list(int), list(int)).
:- mode parse(in, out, in) is det.
:- mode parse(out, in, out) is semidet.

:- implementation.

:- import_module list, maybe, string.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

:- instance parseable(level)
	where
[
	pred(parse/3) is ebea.streams.parse
].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

:- type fileType --->
	csv.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

openInputStreams(Level, IMStreams, !IO) :-
	Level = detailed,
	io.open_input("birth.csv", RStreamBirth, !IO),
	(
		RStreamBirth = ok(SBirth),
		io.open_input("death.csv", RStreamDeath, !IO),
		(
			RStreamDeath = ok(SDeath),
			io.open_input("phenotype.csv", RStreamPhenotype, !IO),
			(
				RStreamPhenotype = ok(Streams^siphenotype),
				Streams^sibirth = SBirth,
				Streams^sideath = SDeath,
				IMStreams = ok(Streams)
				;
				RStreamPhenotype = error(Error1),
				IMStreams = error(string.format("IO error opening `phenotype.csv` file: %s", [s(io.error_message(Error1))])),
				io.close_input(SDeath, !IO),
				io.close_input(SBirth, !IO)
			)
			;
			RStreamDeath = error(Error2),
			IMStreams = error(string.format("IO error opening `death.csv` file: %s", [s(io.error_message(Error2))])),
			io.close_input(SBirth, !IO)
		)
		;
		RStreamBirth = error(Error3),
		IMStreams = error(string.format("IO error opening `birth.csv` file: %s", [s(io.error_message(Error3))]))
	)
	;
	Level = dynamics,
	io.open_input("population.csv", RStreamPopulation, !IO),
	(
		RStreamPopulation = ok(Streams^sipopulation),
		IMStreams = ok(Streams)
		;
		RStreamPopulation = error(Error),
		IMStreams = error(string.format("IO error opening `population.csv` file: %s", [s(io.error_message(Error))]))
	)
	;
	Level = summary,
	io.open_input("summary.csv", RStreamSummary, !IO),
	(
		RStreamSummary = ok(Streams^sisummary),
		IMStreams = ok(Streams)
		;
		RStreamSummary = error(Error),
		IMStreams = error(string.format("IO error opening `summary.csv` file: %s", [s(io.error_message(Error))]))
	).


% scanStreams(Stream, IMStreams, !IO) :-
% 	io.read_line_as_string(Stream, ILine, !IO),
% 	(if
% 		ILine = ok(ULine)
% 	then
% 		(if
% 			levelDetail(string.to_lower(string.strip(ULine)), Level)
% 		then
% 			openOutputStreams(Level, IMStreams, !IO)
% 		else
% 			IMStreams = ok(error("Expecting a line with level of detail for saving EBEA data"))
% 		)
% 	else
% 		IMStreams = scanable.noOkToResult("level of detail for saving EBEA data", ILine)
% 	).

% scanLevel(Stream, IMLevel, !IO) :-
% 	io.read_line_as_string(Stream, ILine, !IO),
% 	(if
% 		ILine = ok(ULine)
% 	then
% 		(if
% 			levelDetail(string.to_lower(string.strip(ULine)), Level)
% 		then
% 			IMLevel = ok(ok(Level))
% 		else
% 			IMLevel = ok(error("Expecting a line with level of detail for saving EBEA data"))
% 		)
% 	else
% 		IMLevel = scanable.noOkToResult("level of detail for saving EBEA data", ILine)
% 	).

closeInputStreams(Streams, !IO) :-
	Streams = detailed(_, _, _),
	io.close_input(Streams^sibirth, !IO),
	io.close_input(Streams^sideath, !IO),
	io.close_input(Streams^siphenotype, !IO)
	;
	Streams = dynamics(_),
	io.close_input(Streams^sipopulation, !IO)
	;
	Streams = summary(_),
	io.close_input(Streams^sisummary, !IO).

closeOutputStreams(Streams, !IO) :-
	Streams = detailed(_, _, _, _),
	io.close_output(Streams^sobirth, !IO),
	io.close_output(Streams^sodeath, !IO),
	io.close_output(Streams^sophenotype, !IO),
	io.close_output(Streams^soplayerProfile, !IO)
	;
	Streams = dynamics(_),
	io.close_output(Streams^sopopulation, !IO)
	;
	Streams = summary(_),
	io.close_output(Streams^sosummary, !IO).

dialog = dialog(
	[di(label("detailed"),         newValue(detailed)),
	 di(label("only population"),  newValue(dynamics)),
	 di(label("summary"),          newValue(summary))]).

parse(detailed) --> [0].
parse(dynamics) --> [1].
parse(summary) --> [2].


openOutputStreams(Level, MSuffix, IMStreams, !IO) :-
	Level = detailed,
	io.open_output(filename("birth", MSuffix, csv), RStreamBirth, !IO),
	(
		RStreamBirth = ok(SBirth),
		io.open_output(filename("death", MSuffix, csv), RStreamDeath, !IO),
		(
			RStreamDeath = ok(SDeath),
			io.open_output(filename("phenotype", MSuffix, csv), RStreamPhenotype, !IO),
			(
				RStreamPhenotype = ok(SPhenotype),
				io.open_output(filename("player-profile", MSuffix, csv), RStreamPlayerProfile, !IO),
				(
					RStreamPlayerProfile = ok(SPlayerProfile),
					Streams^sobirth = SBirth,
					Streams^sodeath = SDeath,
					Streams^sophenotype = SPhenotype,
					Streams^soplayerProfile = SPlayerProfile,
					IMStreams = ok(Streams)
					;
					RStreamPlayerProfile = error(Error0),
					IMStreams = error(string.format("IO error opening `%s` file: %s", [s(filename("player-profile", MSuffix, csv)), s(io.error_message(Error0))])),
					io.close_output(SPhenotype, !IO),
					io.close_output(SDeath, !IO),
					io.close_output(SBirth, !IO)
				)
				;
				RStreamPhenotype = error(Error1),
				IMStreams = error(string.format("IO error opening `phenotype.csv` file: %s", [s(io.error_message(Error1))])),
				io.close_output(SDeath, !IO),
				io.close_output(SBirth, !IO)
			)
			;
			RStreamDeath = error(Error2),
			IMStreams = error(string.format("IO error opening `death.csv` file: %s", [s(io.error_message(Error2))])),
			io.close_output(SBirth, !IO)
		)
		;
		RStreamBirth = error(Error3),
		IMStreams = error(string.format("IO error opening `birth.csv` file: %s", [s(io.error_message(Error3))]))
	)
	;
	Level = dynamics,
	io.open_output(filename("population", MSuffix, csv), RStreamPopulation, !IO),
	(
		RStreamPopulation = ok(Streams^sopopulation),
		IMStreams = ok(Streams)
		;
		RStreamPopulation = error(Error),
		IMStreams = error(string.format("IO error opening `population.csv` file: %s", [s(io.error_message(Error))]))
	)
	;
	Level = summary,
	io.open_append("summary.csv", RStreamSummary, !IO),
	(
		RStreamSummary = ok(Streams^sosummary),
		IMStreams = ok(Streams)
		;
		RStreamSummary = error(Error),
		IMStreams = error(string.format("IO error opening `summary.csv` file: %s", [s(io.error_message(Error))]))
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- pred levelDetail(string, level).
:- mode levelDetail(in, out) is semidet.
:- mode levelDetail(out, in) is det.

levelDetail("detailed", detailed).
levelDetail("dynamics", dynamics).
levelDetail("summary", summary).

/**
 * Return the file name to open either for reading or writing.
 */
:- func filename(string, maybe(string), fileType) = string.

filename(Base, no, csv) = Base ++ ".csv".
filename(Base, yes(Suffix), csv) = Base ++ Suffix ++ ".csv".


:- end_module ebea.streams.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
