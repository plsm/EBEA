/**
 * 

 * @author Pedro Mariano
 * @version 1.0 2014/03/ 6
 */
:- module tools.export_playerProfiles_gefx.

:- interface.

:- import_module data, data.config.
:- import_module io, string.

:- pred createPlayerProfilesNetworks(data.config.config, string, string, io.state, io.state).
:- mode createPlayerProfilesNetworks(in, in, out, di, uo) is det.

:- implementation.

:- import_module ebea, ebea.streams.
:- import_module exception, int, list, maybe.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

createPlayerProfilesNetworks(Data, FilenamePrefix, Feedback, !IO) :-
	Data^level = detailed,
	int.fold_up2(createPlayerProfilesNetworkForRun(Data, FilenamePrefix), 1, Data^numberRuns, [], FeedbackAsList, !IO),
	(
		FeedbackAsList = [],
		Feedback = "ok"
		;
		FeedbackAsList = [_|_],
		Feedback = string(FeedbackAsList)
	)
	;
	Data^level = dynamics,
	Feedback = "The simulation runs did not produce player profile data"
	;
	Data^level = summary,
	Feedback = "The simulation runs did not produce player profile data"
	.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- pred createPlayerProfilesNetworkForRun(data.config.config, string, int, list(string), list(string), io.state, io.state).
:- mode createPlayerProfilesNetworkForRun(in, in, in, in, out, di, uo) is det.

createPlayerProfilesNetworkForRun(Data, FilenamePrefix, RunIndex, !FeedbackAsList, !IO) :-
	ebea.streams.openInputStreams(Data^level, yes(string.format("_R%d", [i(RunIndex)])), IMStreams, !IO),
	(
		IMStreams = ok(Streams),
		Streams = detailed(_, _, _, _),
		FileName = string.format("%s_R%d.gefx", [s(FilenamePrefix), i(RunIndex)]),
		io.open_output(FileName, IStream, !IO),
		(
			IStream = ok(GefxStream),
			printHeader(GefxStream, !IO),
			io.close_output(GefxStream, !IO),
			ebea.streams.closeInputStreams(Streams, !IO)
			;
			IStream = error(Error),
			list.cons(string.format("IO error opening `%s` file: %s", [s(FileName), s(io.error_message(Error))]), !FeedbackAsList)
		)
		;
		IMStreams = ok(Streams),
		Streams = dynamics(_),
		throw("createPlayerProfilesNetworkForRun/7: never reached")
		;
		IMStreams = ok(Streams),
		Streams = summary(_),
		throw("createPlayerProfilesNetworkForRun/7: never reached")
		;
		IMStreams = error(ErrorMsg),
		list.cons(ErrorMsg, !FeedbackAsList)
	).

:- func gefxPreface = string.

gefxPreface = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<gexf xmlns=\"http://www.gexf.net/1.2draft\" version=\"1.2\">
    <meta lastmodifieddate=\"2009-03-20\">
        <creator>Player profiles exporter</creator>
        <description>A hello world! file</description>
    </meta>
    <graph mode=\"dynamic\" defaultedgetype=\"directed\">".


:- pred printHeader(io.output_stream, io.state, io.state).
:- mode printHeader(in, di, uo) is det.

printHeader(Stream, !IO) :-
	io.print(Stream, "<?xml version=\"1.0\" encoding=\"UTF−8\"?>
<gexf xmlns=\"http://www.gexf.net/1.2draft\"
  xmlns:xsi=\"http://www.w3.org/2001/XMLSchema−instance\"
  xsi:schemaLocation=\"http://www.gexf.net/1.2draft
   http://www.gexf.net/1.2draft/gexf.xsd\"
  version=\"1.2\">
			  ", !IO),
	io.format(Stream, "<meta lastmodifieddate=\"2009-03-20\">", [], !IO),
	io.print(Stream, "
<creator>tools.export_playerProfiles_gefx</creator>
<description>Player profiles dynamics</description>
</meta>
    <graph mode=\"dynamic\" defaultedgetype=\"directed\">
			  ", !IO)
	.


:- end_module tools.export_playerProfiles_gefx.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
