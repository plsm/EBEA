/**
 * Provides a type that represent the streams where data of Energy Based
 * Evolutionary Algorithm runs are saved.

 * @author Pedro Mariano
 * @version 1.0 2013/05/21
 */
:- module ebea.streams.

:- interface.

:- include_module birth, death, phenotype, playerProfile, siteState.
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
	detailedTxt(
		tosBirth         :: io.output_stream,
		tosDeath         :: io.output_stream,
		tosPhenotype     :: io.output_stream,
		tosPlayerProfile :: io.output_stream
	) ;
	detailedBin(
		bosSiteState     :: io.binary_output_stream,
		bosBirth         :: io.binary_output_stream,
		bosDeath         :: io.binary_output_stream,
		bosPhenotype     :: io.binary_output_stream,
		bosPlayerProfile :: io.binary_output_stream
	) ;
	dynamics(
		sopopulation :: io.output_stream
	) ;
	summary(
		sosummary    :: io.output_stream
	).

:- type inStreams --->
	detailedTxt(
		tisBirth         :: io.input_stream,
		tisDeath         :: io.input_stream,
		tisPhenotype     :: io.input_stream,
		tisPlayerProfile :: io.input_stream
	) ;
	detailedBin(
		bisSiteState     :: io.binary_input_stream,
		bisBirth         :: io.binary_input_stream,
		bisDeath         :: io.binary_input_stream,
		bisPhenotype     :: io.binary_input_stream,
		bisPlayerProfile :: io.binary_input_stream
	) ;
	dynamics(
		sipopulation :: io.input_stream
	) ;
	summary(
		sisummary    :: io.input_stream
	).

:- inst detailedTxt == bound(detailedTxt(ground, ground, ground, ground)).

:- inst detailedBin == bound(detailedBin(ground, ground, ground, ground, ground)).

:- inst detailed == bound(
	detailedBin(ground, ground, ground, ground, ground) ;
	detailedTxt(ground, ground, ground, ground)
	).

:- inst dynamics == bound(dynamics(ground)).

:- inst summary == bound(summary(ground)).

/**
 * Represents the level of detail of the output streams used by an Energy
 * Based Evolutionary Algorithm run.
 */
:- type level --->
	detailedTxt ;
	detailedBin ;
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

:- pred openOutputStreams(string, level, maybe(string), maybe_error(outStreams), io.state, io.state).
:- mode openOutputStreams(in, in, in, out, di, uo) is det.

/**
 * Closes the streams used by an Energy Based Evolutionary Algorithm run.
 */
:- pred closeOutputStreams(outStreams, io, io).
:- mode closeOutputStreams(in, di, uo) is det.

% /**
%  * openInputStreams(Level, MFileNameSuffix, MStreams, !IO)
  
%  * Opens streams in input mode used by EBEA at the specified level.
%  */
% :- pred openInputStreams(level, maybe(string), maybe_error(inStreams), io.state, io.state).
% :- mode openInputStreams(in, in, out, di, uo) is det.

/**
 * openInputStreams(Directory, Level, MFileNameSuffix, MStreams, !IO)
  
 * Opens streams in input mode used by EBEA at the specified level at the given directory.
 */
:- pred openInputStreams(string, level, maybe(string), maybe_error(inStreams), io.state, io.state).
:- mode openInputStreams(in, in, in, out, di, uo) is det.

/**
 * Closes the streams used by an Energy Based Evolutionary Algorithm run.
 */
:- pred closeInputStreams(inStreams, io, io).
:- mode closeInputStreams(in, di, uo) is det.

/**
 * Return a user dialog where an user can select the amount of data that is
 * written by an EBEA run.
 */
:- func dialog = list(dialogItem(level)).

:- pred parse(level, parseable.state, parseable.state).
:- mode parse(in, out, in) is det.
:- mode parse(out, in, out) is semidet.

:- implementation.

:- import_module exception, list, maybe, string.

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
	csv ;
	bin.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

openInputStreams(Directory, Level, MSuffix, IMStreams, !IO) :-
	Level = detailedTxt,
	io.open_input(filename("birth", MSuffix, csv), RStreamBirth, !IO),
	(
		RStreamBirth = ok(SBirth),
		io.open_input(filename("death", MSuffix, csv), RStreamDeath, !IO),
		(
			RStreamDeath = ok(SDeath),
			io.open_input(filename("phenotype", MSuffix, csv), RStreamPhenotype, !IO),
			(
				RStreamPhenotype = ok(SPhenotype),
				io.open_input(filename("player-profile", MSuffix, csv), RStreamPlayerProfile, !IO),
				(
					RStreamPlayerProfile = ok(StreamPlayerProfile),
					Streams = detailedTxt(SBirth, SDeath, SPhenotype, StreamPlayerProfile),
					IMStreams = ok(Streams)
					;
					RStreamPlayerProfile = error(Error4),
					IMStreams = error(string.format("IO error opening `%s` file: %s", [s(filename("player-profile", MSuffix, csv)), s(io.error_message(Error4))])),
					io.close_input(SPhenotype, !IO),
					io.close_input(SDeath, !IO),
					io.close_input(SBirth, !IO)
				)
				;
				RStreamPhenotype = error(Error3),
				IMStreams = error(string.format("IO error opening `phenotype.csv` file: %s", [s(io.error_message(Error3))])),
				io.close_input(SDeath, !IO),
				io.close_input(SBirth, !IO)
			)
			;
			RStreamDeath = error(Error2),
			IMStreams = error(string.format("IO error opening `death.csv` file: %s", [s(io.error_message(Error2))])),
			io.close_input(SBirth, !IO)
		)
		;
		RStreamBirth = error(Error1),
		IMStreams = error(string.format("IO error opening `birth.csv` file: %s", [s(io.error_message(Error1))]))
	)
	;
	Level = detailedBin,
	FileNameBirth = filename(Directory, "birth", MSuffix, bin),
	io.open_binary_input(FileNameBirth, RStreamBirth, !IO),
	(
		RStreamBirth = error(Error1),
		IMStreams = error(string.format("IO error opening `%s` file: %s", [s(FileNameBirth), s(io.error_message(Error1))]))
		;
		RStreamBirth = ok(SBirth),
		FileNameDeath = filename(Directory, "death", MSuffix, bin),
		io.open_binary_input(FileNameDeath, RStreamDeath, !IO),
		(
			RStreamDeath = error(Error2),
			IMStreams = error(string.format("IO error opening `%s` file: %s", [s(FileNameDeath), s(io.error_message(Error2))])),
			io.close_binary_input(SBirth, !IO)
			;
			RStreamDeath = ok(SDeath),
			FileNamePhenotype = filename(Directory, "phenotype", MSuffix, bin),
			io.open_binary_input(FileNamePhenotype, RStreamPhenotype, !IO),
			(
				RStreamPhenotype = error(Error3),
				IMStreams = error(string.format("IO error opening `%s` file: %s", [s(FileNamePhenotype), s(io.error_message(Error3))])),
				io.close_binary_input(SDeath, !IO),
				io.close_binary_input(SBirth, !IO)
				;
				RStreamPhenotype = ok(SPhenotype),
				FileNamePlayerProfile = filename(Directory, "player-profile", MSuffix, bin),
				io.open_binary_input(FileNamePlayerProfile, RStreamPlayerProfile, !IO),
				(	%
					RStreamPlayerProfile = error(Error4),
					IMStreams = error(string.format("IO error opening `%s` file: %s", [s(FileNamePlayerProfile), s(io.error_message(Error4))])),
					io.close_binary_input(SPhenotype, !IO),
					io.close_binary_input(SDeath, !IO),
					io.close_binary_input(SBirth, !IO)
				;
					RStreamPlayerProfile = ok(StreamPlayerProfile),
					FileNameSiteState = filename(Directory, "site-state", MSuffix, bin),
					io.open_binary_input(FileNameSiteState, RStreamSiteState, !IO),
					(	%
						RStreamSiteState = error(Error5),
						IMStreams = error(string.format("IO error opening `%s` file: %s", [s(FileNameSiteState), s(io.error_message(Error5))])),
						io.close_binary_input(SPhenotype, !IO),
						io.close_binary_input(SDeath, !IO),
						io.close_binary_input(SBirth, !IO),
						io.close_binary_input(StreamPlayerProfile, !IO)
					;
						RStreamSiteState = ok(SSiteState),
						Streams = detailedBin(SSiteState, SBirth, SDeath, SPhenotype, StreamPlayerProfile),
						IMStreams = ok(Streams)
					)
				)
			)
		)
	)
	;
	Level = dynamics,
	io.open_input(filename("population", MSuffix, csv), RStreamPopulation, !IO),
	(
		RStreamPopulation = ok(Streams^sipopulation),
		IMStreams = ok(Streams)
		;
		RStreamPopulation = error(Error),
		IMStreams = error(string.format("IO error opening `population.csv` file: %s", [s(io.error_message(Error))]))
	)
	;
	Level = summary,
	io.open_input(filename("summary", MSuffix, csv), RStreamSummary, !IO),
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
	Streams = detailedTxt(_, _, _, _),
	io.close_input(Streams^tisBirth, !IO),
	io.close_input(Streams^tisDeath, !IO),
	io.close_input(Streams^tisPhenotype, !IO),
	io.close_input(Streams^tisPlayerProfile, !IO)
	;
	Streams = detailedBin(_, _, _, _, _),
	io.close_binary_input(Streams^bisSiteState, !IO),
	io.close_binary_input(Streams^bisBirth, !IO),
	io.close_binary_input(Streams^bisDeath, !IO),
	io.close_binary_input(Streams^bisPhenotype, !IO),
	io.close_binary_input(Streams^bisPlayerProfile, !IO)
	;
	Streams = dynamics(_),
	io.close_input(Streams^sipopulation, !IO)
	;
	Streams = summary(_),
	io.close_input(Streams^sisummary, !IO).

closeOutputStreams(Streams, !IO) :-
	Streams = detailedTxt(_, _, _, _),
	io.close_output(Streams^tosBirth, !IO),
	io.close_output(Streams^tosDeath, !IO),
	io.close_output(Streams^tosPhenotype, !IO),
	io.close_output(Streams^tosPlayerProfile, !IO)
	;
	Streams = detailedBin(_, _, _, _, _),
	io.close_binary_output(Streams^bosSiteState, !IO),
	io.close_binary_output(Streams^bosBirth, !IO),
	io.close_binary_output(Streams^bosDeath, !IO),
	io.close_binary_output(Streams^bosPhenotype, !IO),
	io.close_binary_output(Streams^bosPlayerProfile, !IO)
	;
	Streams = dynamics(_),
	io.close_output(Streams^sopopulation, !IO)
	;
	Streams = summary(_),
	io.close_output(Streams^sosummary, !IO).

dialog =
	[di(label("streams"), selectOneOf(
		selectedChoice,
		selectChoice,
		[
		 ci(label("everything (text)"),   []),
		 ci(label("everything (binary)"), []),
		 ci(label("population dynamics"), []),
		 ci(label("summary"),             [])
		]))].

parse(detailedTxt) --> [0].
parse(dynamics)    --> [1].
parse(summary)     --> [2].
parse(detailedBin) --> [3].


openOutputStreams(Directory, Level, MSuffix, IMStreams, !IO) :-
	Level = detailedTxt,
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
					Streams^tosBirth = SBirth,
					Streams^tosDeath = SDeath,
					Streams^tosPhenotype = SPhenotype,
					Streams^tosPlayerProfile = SPlayerProfile,
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
	Level = detailedBin,
	io.open_binary_output(filename(Directory, "birth", MSuffix, bin), RStreamBirth, !IO),
	(
		RStreamBirth = ok(SBirth),
		io.open_binary_output(filename(Directory, "death", MSuffix, bin), RStreamDeath, !IO),
		(
			RStreamDeath = ok(SDeath),
			io.open_binary_output(filename(Directory, "phenotype", MSuffix, bin), RStreamPhenotype, !IO),
			(
				RStreamPhenotype = ok(SPhenotype),
				FileNamePlayerProfile = filename(Directory, "player-profile", MSuffix, bin),
				io.open_binary_output(FileNamePlayerProfile, RStreamPlayerProfile, !IO),
				(	%
					RStreamPlayerProfile = ok(SPlayerProfile),
					FileNameSiteState = filename(Directory, "site-state", MSuffix, bin),
					io.open_binary_output(FileNameSiteState, RStreamSiteState, !IO),
					(	%
						RStreamSiteState = ok(SStreamSiteState),
						Streams^bosSiteState = SStreamSiteState,
						Streams^bosBirth = SBirth,
						Streams^bosDeath = SDeath,
						Streams^bosPhenotype = SPhenotype,
						Streams^bosPlayerProfile = SPlayerProfile,
						IMStreams = ok(Streams)
					;
						RStreamSiteState = error(Error4),
						IMStreams = error(string.format("IO error opening `%s` file: %s", [s(FileNameSiteState), s(io.error_message(Error4))])),
						io.close_binary_output(SPlayerProfile, !IO),
						io.close_binary_output(SPhenotype, !IO),
						io.close_binary_output(SDeath, !IO),
						io.close_binary_output(SBirth, !IO)
					)
				;
					RStreamPlayerProfile = error(Error0),
					IMStreams = error(string.format("IO error opening `%s` file: %s", [s(FileNamePlayerProfile), s(io.error_message(Error0))])),
					io.close_binary_output(SPhenotype, !IO),
					io.close_binary_output(SDeath, !IO),
					io.close_binary_output(SBirth, !IO)
				)
				;
				RStreamPhenotype = error(Error1),
				IMStreams = error(string.format("IO error opening `phenotype.csv` file: %s", [s(io.error_message(Error1))])),
				io.close_binary_output(SDeath, !IO),
				io.close_binary_output(SBirth, !IO)
			)
			;
			RStreamDeath = error(Error2),
			IMStreams = error(string.format("IO error opening `death.csv` file: %s", [s(io.error_message(Error2))])),
			io.close_binary_output(SBirth, !IO)
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

:- func selectedChoice(level) = maybe(int).

selectedChoice(detailedTxt) = yes(0).
selectedChoice(detailedBin) = yes(1).
selectedChoice(dynamics)    = yes(2).
selectedChoice(summary)     = yes(3).

:- func selectChoice(level, int) = setResult(level).

selectChoice(_, Index) = ok(Level) :-
	(if
		Index = 0, L = detailedTxt ;
		Index = 1, L = detailedBin ;
		Index = 2, L = dynamics ;
		Index = 3, L = summary
	then
		Level = L
	else
		throw("ebea.streams.selectChoice/2: Never reached")
	).

% :- pred levelDetail(string, level).
% :- mode levelDetail(in, out) is semidet.
% :- mode levelDetail(out, in) is det.

% levelDetail("detailed", detailed).
% levelDetail("dynamics", dynamics).
% levelDetail("summary", summary).

/**
 * Return the file name to open either for reading or writing.
 */
:- func filename(string, maybe(string), fileType) = string.

filename(Base, no, csv) = Base ++ ".csv".
filename(Base, yes(Suffix), csv) = Base ++ Suffix ++ ".csv".

filename(Base, no,          bin) = Base           ++ ".bin".
filename(Base, yes(Suffix), bin) = Base ++ Suffix ++ ".bin".

/**
 * Return the file name to open either for reading or writing.
 */
:- func filename(string, string, maybe(string), fileType) = string.

filename(Directory, Base, no,          csv) = Directory ++ Base           ++ ".csv".
filename(Directory, Base, yes(Suffix), csv) = Directory ++ Base ++ Suffix ++ ".csv".
filename(Directory, Base, no,          bin) = Directory ++ Base           ++ ".bin".
filename(Directory, Base, yes(Suffix), bin) = Directory ++ Base ++ Suffix ++ ".bin".


:- end_module ebea.streams.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
