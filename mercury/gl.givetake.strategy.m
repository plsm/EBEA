/**
 * 

 * @author Pedro Mariano
 * @version 1.0 2013/12/ 5
 */
:- module gl.givetake.strategy.

:- interface.

:- type strategy --->
	time(
		timeGive :: int,
		timeTake :: int
	)
	;
	history(
		gt  :: bool,
		gn  :: bool,
		nt  :: bool,
		nny :: bool,
		tg  :: bool,
		ng  :: bool,
		tn  :: bool,
		nnn :: bool,
		fg  :: bool,
		ft  :: bool
	)
	.

/**
 * The accumulator used to reduce a set of strategies.
 */
%:- type accumulator.
:- type accumulator --->
	ac(
		sumTimeGive :: int,
		sumTimeTake :: int,
		qtyTime     :: float
%		qtyHistory  :: array(int)
	).

:- inst history == bound(history(ground,ground,ground,ground,ground,ground,ground,ground,ground,ground)).

:- instance parseable(strategy).

:- instance printable(strategy).

:- instance printable(accumulator).

:- instance foldable(strategy, accumulator).

/**
 * Return a default value of {@code strategy}.
 */
:- func default = strategy.

:- func dialog = list(dialogItem(strategy)).


/**
 * Returns the number of parameters or genes of the given strategy.  This
 * depends on the constructor.

 * <p> This function is part of type class {@code
 * chromosome(strategy,unit)}.
 */

:- func numberParameters(strategy) = int.

/**
 * mutateGene(Parameters, Index, !Distribution, !Random, Strategy, Result)
 
 * Mutate the single gene of a Give-Take chromosome.  Currently
 * we sum an uniform random variable.

 * <p> This predicate is part of type class {@code chromosome(strategy,strategy)}.
 *
 */

:- pred mutateGene(parameters, int, distribution, distribution, R, R, strategy, strategy)
	<= ePRNG(R).
:- mode mutateGene(in, in, in, out, in, out, in, out) is det.

% :- func timeGive(strategy) = int.
% :- func timeTake(strategy) = int.

:- func gt(strategy) = bool.
:- func gn(strategy) = bool.
:- func nt(strategy) = bool.
:- func nny(strategy) = bool.
:- func tg(strategy) = bool.
:- func ng(strategy) = bool.
:- func tn(strategy) = bool.
:- func nnn(strategy) = bool.
:- func fg(strategy) = bool.
:- func ft(strategy) = bool.

:- mode gt(in(history)) = out is det.
:- mode gn(in(history)) = out is det.
:- mode nt(in(history)) = out is det.
:- mode nny(in(history)) = out is det.
:- mode tg(in(history)) = out is det.
:- mode ng(in(history)) = out is det.
:- mode tn(in(history)) = out is det.
:- mode nnn(in(history)) = out is det.
:- mode fg(in(history)) = out is det.
:- mode ft(in(history)) = out is det.

:- func 'gt :='(strategy, bool) = strategy.
:- func 'gn :='(strategy, bool) = strategy.
:- func 'nt :='(strategy, bool) = strategy.
:- func 'nny :='(strategy, bool) = strategy.
:- func 'tg :='(strategy, bool) = strategy.
:- func 'ng :='(strategy, bool) = strategy.
:- func 'tn :='(strategy, bool) = strategy.
:- func 'nnn :='(strategy, bool) = strategy.
:- func 'fg :='(strategy, bool) = strategy.
:- func 'ft :='(strategy, bool) = strategy.


:- mode 'gt :='(in(history), in) = out(history) is det.
:- mode 'gn :='(in(history), in) = out(history) is det.
:- mode 'nt :='(in(history), in) = out(history) is det.
:- mode 'nny :='(in(history), in) = out(history) is det.
:- mode 'tg :='(in(history), in) = out(history) is det.
:- mode 'ng :='(in(history), in) = out(history) is det.
:- mode 'tn :='(in(history), in) = out(history) is det.
:- mode 'nnn :='(in(history), in) = out(history) is det.
:- mode 'fg :='(in(history), in) = out(history) is det.
:- mode 'ft :='(in(history), in) = out(history) is det.

:- implementation.

:- include_module encodecode.
:- import_module gl.givetake.strategy.encodecode.
:- import_module util.
:- import_module exception, float, int.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

% :- type accumulator --->
% 	ac(
% 		sumTimeGive :: int,
% 		sumTimeTake :: int,
% 		qtyTime     :: float,
% 		qtyHistory  :: array(int)
% 	).

:- instance parseable(strategy) where
[
	pred(parse/3) is gl.givetake.strategy.parse
].

:- instance printable(strategy) where
[
	pred(print/4) is gl.givetake.strategy.printStrategy
].

:- instance foldable(strategy, accumulator) where
[
	func(fold/2)   is gl.givetake.strategy.fold,
	func(initAC/0) is gl.givetake.strategy.init
].

:- instance printable(accumulator) where
[
	pred(print/4) is gl.givetake.strategy.printAccumulator
].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

default = Result :-
	Result^timeGive = default_timeGive,
	Result^timeTake = default_timeTake.

dialog =
	[
	di(label("time"),     subdialog( [
		di(label("time give"),  updateFieldInt(      get_timeGive,  checkInt(   "time give",  bounded(1, yes), unbound, set_timeGive))),
		di(label("time take"),  updateFieldInt(      get_timeTake,  checkInt(   "time take",  bounded(1, yes), unbound, set_timeTake)))
		])),
	di(label("history"),  subdialog( [
		di(label("gt"),   updateFieldBool(     get_gt,   set(set_gt))),
		di(label("gn"),   updateFieldBool(     get_gn,   set(set_gn))),
		di(label("nt"),   updateFieldBool(     get_nt,   set(set_nt))),
		di(label("nny"),  updateFieldBool(     get_nny,  set(set_nny))),
		di(label("tg"),   updateFieldBool(     get_tg,   set(set_tg))),
		di(label("ng"),   updateFieldBool(     get_ng,   set(set_ng))),
		di(label("tn"),   updateFieldBool(     get_tn,   set(set_tn))),
		di(label("nnn"),  updateFieldBool(     get_nnn,  set(set_nnn))),
		di(label("fg"),   updateFieldBool(     get_fg,   set(set_fg))),
		di(label("ft"),   updateFieldBool(     get_ft,   set(set_ft)))
		]))
	].

numberParameters(time(_, _)) = 2.
numberParameters(history(_, _, _, _, _, _, _, _, _, _)) = 10.


mutateGene(Parameters, Index, !Distribution, !Random, Strategy, Result) :-
	Strategy = time(_, _),
	(if
		Index = 0
	then
		distribution.unitGaussian(Perturb0, !Distribution, !Random),
		NewTime =
			int.max(
				1,
				int.min(float.round_to_int(float(Strategy^timeGive) + Perturb0 * Parameters^timeStdDev) \/ 1,
				Parameters^maxTime)),
		Result = 'timeGive :='(Strategy, NewTime)
	else if
		Index = 1
	then
		distribution.unitGaussian(Perturb0, !Distribution, !Random),
		NewTime =
			int.max(
				1,
				int.min(float.round_to_int(float(Strategy^timeTake) + Perturb0 * Parameters^timeStdDev) \/ 1,
				Parameters^maxTime)),
		Result = 'timeTake :='(Strategy, NewTime)
	else
		throw("gl.givetake.strategy.mutateGene/5: Invalid gene index")
	)
	;
	Strategy = history(_, _, _, _, _, _, _, _, _, _),
	(if
		geneFunc(Index, GetFunc, SetFunc)
	then
		GetFunc(Strategy) = Value,
		Result = SetFunc(Strategy, bool.not(Value))
	else
		throw("gl.givetake.strategy.mutateGene/5: Invalid gene index")
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- pred geneFunc(int, func(strategy) = bool, func(strategy, bool) = strategy).
:- mode geneFunc(in,  out, out) is semidet.
% :- mode geneFunc(out, in,  in) is det.
% :- mode geneFunc(out, out, in) is det.
% :- mode geneFunc(out, in,  out) is det.

geneFunc(0, gt,  'gt :=').
geneFunc(1, gn,  'gn :=').
geneFunc(2, nt,  'nt :=').
geneFunc(3, nny, 'nny :=').
geneFunc(4, tg,  'tg :=').
geneFunc(5, ng,  'ng :=').
geneFunc(6, tn,  'tn :=').
geneFunc(7, nnn, 'nnn :=').
geneFunc(8, fg,  'fg :=').
geneFunc(9, ft,  'ft :=').



/**
 * fold(Strategy, AC) = Result
  
 * Adds the strategy provide probability to the accumulator.  Updates the
 * number of strategies reduced so far.
 */

:- func fold(strategy, accumulator) = accumulator.

fold(Strategy, AC) = Result :-
	Strategy = time(_, _),
	R1 = 'sumTimeGive :='(AC, AC^sumTimeGive + Strategy^timeGive),
	R2 = 'sumTimeTake :='(R1, AC^sumTimeTake + Strategy^timeTake),
	R3 = 'qtyTime :='(    R2, AC^qtyTime + 1.0),
	Result = R3
	;
	Strategy = history(_, _, _, _, _, _, _, _, _, _),
	Result = AC
	% encodecodeHist(Strategy, Index),
	% NewQtyHistory = array.set(AC^qtyHistory, Index, array.lookup(AC^qtyHistory, Index) + 1),
	% R1 = 'qtyHistory :='(AC, NewQtyHistory),
	% Result = R1
	.

:- func init = accumulator.

%init = ac(0, 0, 0.0, array.init(1024, 0)).
init = ac(0, 0, 0.0).

:- pred printStrategy(io.output_stream, strategy, io.state, io.state).
:- mode printStrategy(in, in, di, uo) is det.

printStrategy(Stream, Strategy, !IO) :-
	Strategy = time(TG, TT),
	io.print(Stream, "t ", !IO),
	io.print(Stream, TG, !IO),
	io.print(Stream, ' ', !IO),
	io.print(Stream, TT, !IO)
	;
	Strategy = history(_, _, _, _, _, _, _, _, _, _),
	io.print(Stream, "h ", !IO),
	gl.givetake.strategy.encodecode.encodecodeHist(Strategy, Index),
	io.print(Stream, Index, !IO)
	.

:- pred printAccumulator(io.output_stream, accumulator, io.state, io.state).
:- mode printAccumulator(in, in, di, uo) is det.

printAccumulator(Stream, AC, !IO) :-
	(if
		AC^qtyTime = 0.0
	then
		io.print(Stream, "1/0 1/0", !IO)
	else
		io.print(Stream, float(AC^sumTimeGive) / AC^qtyTime, !IO),
		io.print(Stream, ' ', !IO),
		io.print(Stream, float(AC^sumTimeTake) / AC^qtyTime, !IO)
	)
%	array.foldl(util.spacePrint(Stream), AC^qtyHistory, !IO)
	.

:- func default_timeGive = int.

default_timeGive = 1.

:- func default_timeTake = int.

default_timeTake = 2.

:- func default_gt = bool.

default_gt = no.

:- func default_gn = bool.

default_gn = no.

:- func default_nt = bool.

default_nt = no.

:- func default_nny = bool.

default_nny = no.

:- func default_tg = bool.

default_tg = no.

:- func default_ng = bool.

default_ng = no.

:- func default_tn = bool.

default_tn = no.

:- func default_nnn = bool.

default_nnn = no.

:- func default_fg = bool.

default_fg = no.

:- func default_ft = bool.

default_ft = no.

:- func get_timeGive(strategy) = int.

get_timeGive(P) = R :-
	P = time(_, _),
	R = P^timeGive
	;
	P = history(_, _, _, _, _, _, _, _, _, _),
	R = default_timeGive
	.

:- func set_timeGive(strategy, int) = strategy.

set_timeGive(P, V) = R :-
	P = time(_, _),
	R = 'timeGive :='(P, V)
	;
	P = history(_, _, _, _, _, _, _, _, _, _),
	R = time(V, default_timeTake)
	.


:- func get_timeTake(strategy) = int.

get_timeTake(P) = R :-
	P = time(_, _),
	R = P^timeTake
	;
	P = history(_, _, _, _, _, _, _, _, _, _),
	R = default_timeTake
	.

:- func set_timeTake(strategy, int) = strategy.

set_timeTake(P, V) = R :-
	P = time(_, _),
	R = 'timeTake :='(P, V)
	;
	P = history(_, _, _, _, _, _, _, _, _, _),
	R = time(default_timeGive, V)
	.


:- func get_gt(strategy) = bool.

get_gt(P) = R :-
	P = time(_, _),
	R = default_gt
	;
	P = history(_, _, _, _, _, _, _, _, _, _),
	R = P^gt
	.

:- func set_gt(strategy, bool) = strategy.

set_gt(P, V) = R :-
	P = time(_, _),
	R = history(V, default_gn, default_nt, default_nny, default_tg, default_ng, default_tn, default_nnn, default_fg, default_ft)
	;
	P = history(_, _, _, _, _, _, _, _, _, _),
	R = 'gt :='(P, V)
	.


:- func get_gn(strategy) = bool.

get_gn(P) = R :-
	P = time(_, _),
	R = default_gn
	;
	P = history(_, _, _, _, _, _, _, _, _, _),
	R = P^gn
	.

:- func set_gn(strategy, bool) = strategy.

set_gn(P, V) = R :-
	P = time(_, _),
	R = history(default_gt, V, default_nt, default_nny, default_tg, default_ng, default_tn, default_nnn, default_fg, default_ft)
	;
	P = history(_, _, _, _, _, _, _, _, _, _),
	R = 'gn :='(P, V)
	.


:- func get_nt(strategy) = bool.

get_nt(P) = R :-
	P = time(_, _),
	R = default_nt
	;
	P = history(_, _, _, _, _, _, _, _, _, _),
	R = P^nt
	.

:- func set_nt(strategy, bool) = strategy.

set_nt(P, V) = R :-
	P = time(_, _),
	R = history(default_gt, default_gn, V, default_nny, default_tg, default_ng, default_tn, default_nnn, default_fg, default_ft)
	;
	P = history(_, _, _, _, _, _, _, _, _, _),
	R = 'nt :='(P, V)
	.


:- func get_nny(strategy) = bool.

get_nny(P) = R :-
	P = time(_, _),
	R = default_nny
	;
	P = history(_, _, _, _, _, _, _, _, _, _),
	R = P^nny
	.

:- func set_nny(strategy, bool) = strategy.

set_nny(P, V) = R :-
	P = time(_, _),
	R = history(default_gt, default_gn, default_nt, V, default_tg, default_ng, default_tn, default_nnn, default_fg, default_ft)
	;
	P = history(_, _, _, _, _, _, _, _, _, _),
	R = 'nny :='(P, V)
	.


:- func get_tg(strategy) = bool.

get_tg(P) = R :-
	P = time(_, _),
	R = default_tg
	;
	P = history(_, _, _, _, _, _, _, _, _, _),
	R = P^tg
	.

:- func set_tg(strategy, bool) = strategy.

set_tg(P, V) = R :-
	P = time(_, _),
	R = history(default_gt, default_gn, default_nt, default_nny, V, default_ng, default_tn, default_nnn, default_fg, default_ft)
	;
	P = history(_, _, _, _, _, _, _, _, _, _),
	R = 'tg :='(P, V)
	.


:- func get_ng(strategy) = bool.

get_ng(P) = R :-
	P = time(_, _),
	R = default_ng
	;
	P = history(_, _, _, _, _, _, _, _, _, _),
	R = P^ng
	.

:- func set_ng(strategy, bool) = strategy.

set_ng(P, V) = R :-
	P = time(_, _),
	R = history(default_gt, default_gn, default_nt, default_nny, default_tg, V, default_tn, default_nnn, default_fg, default_ft)
	;
	P = history(_, _, _, _, _, _, _, _, _, _),
	R = 'ng :='(P, V)
	.


:- func get_tn(strategy) = bool.

get_tn(P) = R :-
	P = time(_, _),
	R = default_tn
	;
	P = history(_, _, _, _, _, _, _, _, _, _),
	R = P^tn
	.

:- func set_tn(strategy, bool) = strategy.

set_tn(P, V) = R :-
	P = time(_, _),
	R = history(default_gt, default_gn, default_nt, default_nny, default_tg, default_ng, V, default_nnn, default_fg, default_ft)
	;
	P = history(_, _, _, _, _, _, _, _, _, _),
	R = 'tn :='(P, V)
	.


:- func get_nnn(strategy) = bool.

get_nnn(P) = R :-
	P = time(_, _),
	R = default_nnn
	;
	P = history(_, _, _, _, _, _, _, _, _, _),
	R = P^nnn
	.

:- func set_nnn(strategy, bool) = strategy.

set_nnn(P, V) = R :-
	P = time(_, _),
	R = history(default_gt, default_gn, default_nt, default_nny, default_tg, default_ng, default_tn, V, default_fg, default_ft)
	;
	P = history(_, _, _, _, _, _, _, _, _, _),
	R = 'nnn :='(P, V)
	.


:- func get_fg(strategy) = bool.

get_fg(P) = R :-
	P = time(_, _),
	R = default_fg
	;
	P = history(_, _, _, _, _, _, _, _, _, _),
	R = P^fg
	.

:- func set_fg(strategy, bool) = strategy.

set_fg(P, V) = R :-
	P = time(_, _),
	R = history(default_gt, default_gn, default_nt, default_nny, default_tg, default_ng, default_tn, default_nnn, V, default_ft)
	;
	P = history(_, _, _, _, _, _, _, _, _, _),
	R = 'fg :='(P, V)
	.


:- func get_ft(strategy) = bool.

get_ft(P) = R :-
	P = time(_, _),
	R = default_ft
	;
	P = history(_, _, _, _, _, _, _, _, _, _),
	R = P^ft
	.

:- func set_ft(strategy, bool) = strategy.

set_ft(P, V) = R :-
	P = time(_, _),
	R = history(default_gt, default_gn, default_nt, default_nny, default_tg, default_ng, default_tn, default_nnn, default_fg, V)
	;
	P = history(_, _, _, _, _, _, _, _, _, _),
	R = 'ft :='(P, V)
	.


:- pred parse(strategy, list(int), list(int)).
:- mode parse(in, out, in) is det.
:- mode parse(out, in, out) is semidet.

parse(P) -->
	{P = time(TimeGive, TimeTake)},
	[0],
	parseable.int32(TimeGive),
	parseable.int32(TimeTake)
	.

parse(P) -->
	{P = history(GT, GN, NT, NNY, TG, NG, TN, NNN, FG, FT)},
	[1],
		parseable.bool(GT),
		parseable.bool(GN),
		parseable.bool(NT),
		parseable.bool(NNY),
		parseable.bool(TG),
		parseable.bool(NG),
		parseable.bool(TN),
		parseable.bool(NNN),
		parseable.bool(FG),
		parseable.bool(FT)
	.

:- end_module gl.givetake.strategy.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
