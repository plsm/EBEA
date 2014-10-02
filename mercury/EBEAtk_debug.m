/**
 * A debugger of EBEA simulations.

 * @author Pedro Mariano
 * @version 1.0 2014/09/26
 */
:- module 'EBEAtk_debug'.

:- interface.

:- import_module io.

:- pred main(io.state, io.state).
:- mode main(di, uo) is det.

:- implementation.

:- import_module data, data.config, data.config.io.
:- import_module data.config.pretty.
:- import_module ebea, ebea.core, ebea.population, ebea.population.players, ebea.player.
:- import_module ebea.player.chromosome, ebea.player.selection, ebea.player.selection.chromosome, ebea.player.energy, ebea.player.selection.pcv, ebea.player.selection.wv.
:- import_module userInterface, ui_console.
:- import_module bool, list, maybe, string.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

main(!IO) :-
	io.command_line_arguments(Arguments, !IO),
	(if
		Arguments = [Filename]
	then
		data.config.io.read(Filename, MConfig, !IO),
		(
			MConfig = ok(Config0),
			Config = 'numberRuns :='(Config0, 1),
			data.config.runEBEA(runMode(Config), Config, !IO)
			;
			MConfig = error(Msg),
			io.print(Msg, !IO),
			io.nl(!IO)
		)
	else
		io.print("Missing filename\n", !IO)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- func runMode(data.config.config) = data.config.runMode.
:- mode runMode(in) = out(data.config.runMode).

runMode(Config) = Result :-
	Config^selectedGame = '2x2',
	Result = '2x2'(       init(Config), step(Config), ende)
	;
	Config^selectedGame = battlesexes,
	Result = battlesexes( init(Config), step(Config), ende)
	;
	Config^selectedGame = centipede,
	Result = centipede(   init(Config), step(Config), ende)
	;
	Config^selectedGame = givetake,
	Result = givetake(    init(Config), step(Config), ende)
	;
	Config^selectedGame = investment,
	Result = investment(  init(Config), step(Config), ende)
	;
	Config^selectedGame = pgp,
	Result = pgp(         init(Config), step(Config), ende)
	;
	Config^selectedGame = 'pgp+pa',
	Result = 'pgp+pa'(    init(Config), step(Config), ende)
	;
	Config^selectedGame = ultimatum,
	Result = ultimatum(   init(Config), step(Config), ende)
.
	
	
:- pred init(
	data.config.config               :: in,
	ebea.population.population(C, T) :: in,
	io.state :: di,  io.state :: uo
) is det.

init(Config, Population, !IO) :-
	data.config.pretty.print(io.stdout_stream, plain, Config, !IO),
	menuLoop(Population, !IO).

:- pred step(
	data.config.config               :: in,
	ebea.population.population(C, T) :: in,
	int                              :: in,
	bool                      :: out,
	io.state :: di,  io.state :: uo
) is det.

step(Config, Population, Iteration, no, !IO) :-
	io.format("Iteration #%d\n\n", [i(Iteration)], !IO),
	menuLoop(Population, !IO).



:- pred ende(
	ebea.population.population(C, T) :: in,
	io.state :: di,  io.state :: uo
) is det.

ende(Population, !IO) :-
	true.



:- pred menuLoop(
	ebea.population.population(C, T) :: in,
	io.state :: di,  io.state :: uo
) is det.

menuLoop(Population, !IO) :-
	Menu = m([
		mi(label("Player info"), actionDataIO( playerInfo )),
		mi(label("Players' weight vector"), actionDataIO( weightVectors ))
		 ]),
	ui_console.show(Menu, Population, _, !IO).



:- pred playerInfo(
	ebea.population.population(C, T) :: in,
	io.state :: di,  io.state :: uo
) is det.


playerInfo(Population, !IO) :-
	io.print("Player key? ", !IO),
	io.flush_output(io.stdout_stream, !IO),
	io.read_line_as_string(ILine, !IO),
	(if
		ILine = ok(Line),
		string.to_int(string.strip(Line), Key),
		ebea.population.players.search(Population^players, Key, Player)
	then
		io.format("age: %30i\nenergy: %30f\nstrategy: %30s\n",
			[i(Player^traits^ageTrait),
			 f(Player^traits^energyTrait),
			 s(string(Player^chromosome^strategyGenes))], !IO),
		(
			Player^chromosome^selectionGenes = random,
			io.print("Performs random selection of partners\n", !IO)
		;
			Player^chromosome^selectionGenes = normalPartnerSelection(_),
			io.print("Performs partner selection based on probability and combination vectors\n", !IO)
		;
			Player^chromosome^selectionGenes = weightedPartnerSelection(_),
			io.print("Performs partner selection based on probability and combination vectors and weight vector\n", !IO)
		;
			Player^chromosome^selectionGenes = opinion_old(_, _),
			io.print("Performs partner selection based on opinion/2\n", !IO)
		;
			Player^chromosome^selectionGenes = opinion_old(_, _, _, _),
			io.print("Performs partner selection based on opinion/2\n", !IO)
		),
		(
			Player^traits^selectionTrait = random
		;
			Player^traits^selectionTrait = partnerSelection(PCV, MWV),
			(
				MWV = no
			;
				MWV = yes(WV),
				PredPrint =
				(pred(K::in, W::in, !.IOx::di, !:IOx::uo) is det :-
					io.format("Partner %30s has weight %30f\n", [s(string(K)), f(W)], !IOx)
				),
				ebea.player.selection.wv.fold(PredPrint, WV, !IO)
			)
		;
			Player^traits^selectionTrait = opinion(Value, Uncertainty),
			io.format("Opinion value: %30f\nOpinion uncertainty: %30f\n", [f(Value), f(Uncertainty)], !IO)
		)
	else
		io.print("There is no such player\n", !IO)
	).

:- pred weightVectors(
	ebea.population.population(C, T) :: in,
	io.state :: di,  io.state :: uo
) is det.

weightVectors(Population, !IO) :-
	PredPrint =
	(pred(P::in, !.IOx::di, !:IOx::uo) is det :-
		io.format("%10s   =»    ", [s(string(P^id))], !IOx),
		(if
			P^traits^selectionTrait = partnerSelection(_, yes(WV))
		then
			PredPrintWeight =
			(pred(K::in, W::in, !.IOy::di, !:IOy::uo) is det :-
				io.format("  %5i %5.3f", [i(ebea.population.players.key2int(K)), f(W)], !IOy)
			),
			ebea.player.selection.wv.fold(PredPrintWeight, WV, !IOx)
		else
			io.print("NA", !IOx)
		),
		io.nl(!IOx)
	),
	ebea.population.fold_players(PredPrint, Population, !IO).


:- end_module 'EBEAtk_debug'.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
