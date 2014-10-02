/**
 * Provides predicates to read and write site's state to streams.

 * @author Pedro Mariano
 * @version 1.0 2014/09/ 1
 */
:- module ebea.streams.siteState.

:- interface.

:- import_module ebea.population, ebea.population.site.
:- import_module parseable.iou.

:- type iterationSitesStateRecord --->
	issr(
		iteration  :: int,
		sitesState :: list(siteStateRecord)
	).

:- type siteStateRecord --->
	ssr(
		state :: ebea.population.site.state
	).

:- instance parseable(iterationSitesStateRecord).

:- pred read(
	io.binary_input_stream :: in,
	parseable.iou.result(io.result(iterationSitesStateRecord)) :: out,
	parseable.iou.cache :: in,  parseable.iou.cache :: out,
	io.state :: di,  io.state :: uo
) is det.


:- pred write(
	io.binary_output_stream :: in,
	int                     :: in,
	population(C, P)        :: in,
	io.state :: di,  io.state :: uo
	) is det.

:- pred parse(iterationSitesStateRecord, list(int), list(int)).
:- mode parse(in, out, in) is det.
:- mode parse(out, in, out) is semidet.

:- implementation.

:- import_module parseable.iou.
:- import_module array.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

:- instance parseable(iterationSitesStateRecord) where
[
	pred(parse/3) is ebea.streams.siteState.parse
].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

:- instance parseable(siteStateRecord) where
[
	pred(parse/3) is ebea.streams.siteState.parse_siteStateRecord
].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

read(Stream, RIResult, !Cache, !IO) :-
	parseable.iou.read(Stream, 4096, no, !Cache, RIResult, !IO).

write(Stream, Iteration, Population, !IO) :-
	IterationSitesStateRecord = issr(
		Iteration,
		array.foldr(extractSiteState, Population^sites, [])
	),
	parseable.iou.write(Stream, IterationSitesStateRecord, !IO)
	.

parse(issr(Iteration, SitesState)) -->
	parseable.int32(Iteration),
	parseable.parseList(withLength, SitesState).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- func extractSiteState(site, list(siteStateRecord)) = list(siteStateRecord).

extractSiteState(Site, AC) = [ssr(Site^currentState) | AC].


:- pred parse_siteStateRecord(siteStateRecord, list(int), list(int)).
:- mode parse_siteStateRecord(in, out, in) is det.
:- mode parse_siteStateRecord(out, in, out) is semidet.

parse_siteStateRecord(ssr(state(CarryingCapacity))) -->
	parseable.float32(CarryingCapacity)
	.

:- end_module ebea.streams.siteState.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
