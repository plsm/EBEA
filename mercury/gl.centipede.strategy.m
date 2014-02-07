/**
 * 

 * @author Pedro Mariano
 * @version 1.0 2013/12/11
 */
:- module gl.centipede.strategy.

:- interface.

/**
 * A strategy in the Centipede game is the stage where the player stops.
 */
:- type strategy --->
	first(int) ;
%	mixed(int) ;
	second(int).

:- type strategyType --->
	first ;
%	mixed ;
	second.

:- instance printable(strategy).

:- instance parseable(strategy).

/**
 * Return a default Centipede strategy that can be used to construct a
 * player's chromosome.
  
 */
:- func default = strategy.

:- func init(strategyType, int) = strategy.

/**
 * init(Type, Time, Strategy)
  
 * Initialise a Centipede strategy with the given parameter.  There are two
 * types of strategies, one type represents the player that moves in odd
 * stages, the other in even stages.

 * @param FirstFlag If {@code yes} represents a player that moves in odd
 * stages (1, 3, ...)
  
 */
:- pred init(strategyType, int, strategy).
:- mode init(in, in, out) is semidet.

:- func dialog = list(dialogItem(strategy)).


:- func numberParameters(strategy) = int.

:- implementation.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

:- instance printable(strategy) where
[
	pred(print/4) is gl.centipede.strategy.print
].

:- instance parseable(strategy) where
[
	pred(parse/3) is gl.centipede.strategy.parse
].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

default = first(2).

init(Type, Time) = Result :-
	(if
		init(Type, Time, Strategy)
	then
		Result = Strategy
	else
		throw("invalid centipede strategy parameters")
	).

init(Type, Time, Strategy) :-
	Time >= 0,
	(
		Type = first,
		Strategy = first(Time)
		% ;
		% Type = mixed,
		% Strategy = mixed(Time)
		;
		Type = second,
		Strategy = second(Time)
	).

dialog =
	[di(label("first player"), subdialog(
		[di(label("time to stop"),   updateFieldInt( getFirstStop,  setFirstStop))])),
	 di(label("second player"), subdialog(
		[di(label("time to stop"),   updateFieldInt( getSecondStop, setSecondStop))]))
	].

numberParameters(Strategy) = Result :-
	Strategy = first(_),
	Result = 1
	% ;
	% Strategy = mixed(_),
	% Result = 1
	;
	Strategy = second(_),
	Result = 1
	.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- pred parse(strategy, list(int), list(int)).
:- mode parse(in, out, in) is det.
:- mode parse(out, in, out) is semidet.

parse(first(Int)) -->
	[1, Int].

parse(second(Int)) -->
	[2, Int].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% type class {@code writeable(strategy)} predicate

:- func getFirstStop(strategy) = int.

getFirstStop(first(Result)) = Result.
getFirstStop(second(_)) = 1.

:- func setFirstStop(strategy, int) = setResult(strategy).

setFirstStop(_, Time) =
	(if
		Time > 0
	then
		ok(first(Time))
	else
		error("time to stop must be greater than zero")
	).


:- func getSecondStop(strategy) = int.

getSecondStop(first(_)) = 1.
getSecondStop(second(Result)) = Result.

:- func setSecondStop(strategy, int) = setResult(strategy).

setSecondStop(_, Time) =
	(if
		Time > 0
	then
		ok(second(Time))
	else
		error("time to stop must be greater than zero")
	).

:- pred print(io.output_stream, strategy, io, io).
:- mode print(in, in, di, uo) is det.

print(Stream, Strategy, !IO) :-
	Strategy = first(Time),
	io.print(Stream, "first ", !IO),
	io.print(Stream, Time, !IO)
	% ;
	% Strategy = mixed(Time),
	% io.print(Stream, "m ", !IO),
	% io.print(Stream, Time, !IO)
	;
	Strategy = second(Time),
	io.print(Stream, "second ", !IO),
	io.print(Stream, Time, !IO)
	.

:- end_module gl.centipede.strategy.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
