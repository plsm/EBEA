/**
 * 

 * @author Pedro Mariano
 * @version 1.0 2014/10/ 4
 */
:- module simulations.

:- interface.

:- import_module io.

:- pred main(io.state, io.state).
:- mode main(di, uo) is cc_multi.

:- implementation.

:- import_module ebea, ebea.streams, ebea.population, ebea.population.configuration, ebea.population.site, ebea.population.site.parameters.
:- import_module data, data.config, data.config.io, data.config.pretty, data.seed, data.prng.
:- import_module gl, gl.pgp, gl.pgp.game.
:- import_module dir, list, maybe, solutions, string.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

:- type result --->
	result(
		directory :: string,
		config    :: data.config.config
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

main(!IO) :-
	io.command_line_arguments(Args, !IO),
	(if
		Args = [F]
	then
		F = Filename
	else
		Filename = "batch.txt"
	),
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
	(	%
		MConfig = ok(Config),
		solutions.unsorted_aggregate(config(Config), simulations.run, !IO)
	;	
		MConfig = error(Msg),
		io.print(Msg, !IO),
		io.nl(!IO)
	)
	.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- pred config(data.config.config, simulations.result).
:- mode config(in, out) is nondet.

config(Config, Result) :-
	Config^selectedGame = pgp,
	list.member(Game^players, [3, 4, 5, 6, 7, 8]),
	Game^good = 1.0,
	list.member(Game^provisionCost, [0.1, 0.3, 0.5, 0.7, 0.9]),
	GameConfig0 = 'game :='(Config^pgp, Game),
	GameConfig1 = 'initialPopulation :='(
		GameConfig0,
		'sites :='(
			GameConfig0^initialPopulation,
			list.map(changeRandomPartnerSelection, GameConfig0^initialPopulation^sites))),
	Cfg0 = 'pgp :='(Config, GameConfig0),
	Cfg1 = 'level :='(Cfg0, ebea.streams.summary),
	Cfg2 = 'random :='(Cfg1, lcg(clock)),
	Directory = string.format("D%i-%f", [i(Game^players), f(Game^provisionCost)]),
	Result = result(Directory, Cfg2)
	.

:- func changeRandomPartnerSelection(ebea.population.site.parameters.parameters(CS)) = ebea.population.site.parameters.parameters(CS).

changeRandomPartnerSelection(V) = V.

:- pred run(simulations.result, io.state, io.state).
:- mode run(in, di, uo) is det.

run(Result, !IO) :-
	dir.make_directory(Result^directory, IResMD, !IO),
	(	%
		IResMD = ok,
		data.config.runEBEA(background, Result^config, !IO),
		io.call_system(string.format("mv summary.csv %s", [s(Result^directory)]), IResCS, !IO),
		(	%
			IResCS = ok(Code),
			(if
				Code = 0
			then
				io.print("\n\nProcessed\n", !IO),
				data.config.pretty.print(io.stdout_stream, plain, Result^config, !IO)
			else
				io.print("Moving failed\n", !IO)
			)
		;
			IResCS = error(ErrorCS),
			io.format(io.stderr_stream, "error moving files: %s\n", [s(io.error_message(ErrorCS))], !IO)
		)
	;
		IResMD = error(ErrorMD),
		io.format(io.stderr_stream, "error creating directory: %s\n", [s(io.error_message(ErrorMD))], !IO)
	).

:- pred write(simulations.result, io.state, io.state).
:- mode write(in, di, uo) is det.

write(Result, !IO) :-
	dir.make_directory(Result^directory, IResMD, !IO),
	(	%
		IResMD = ok,
		Filename = string.format("%s%cconfig.ebea",
			[s(Result^directory), c(dir.directory_separator)]),
		data.config.io.write(binary, Filename, Result^config, MErrors, !IO),
		(	%
			MErrors = yes(Errors),
			io.format("An error occurred while writing: %s\n", [s(Errors)], !IO)
		;
			MErrors = no,
			io.print("\n\nProcessed\n", !IO),
			data.config.pretty.print(io.stdout_stream, plain, Result^config, !IO)
		)
	;
		IResMD = error(ErrorMD),
		io.format(io.stderr_stream, "error creating directory: %s\n", [s(io.error_message(ErrorMD))], !IO)
	).

:- end_module simulations.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
