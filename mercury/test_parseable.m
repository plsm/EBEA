/**
 * 

 * @author Pedro Mariano
 * @version 1.0 2014/01/ 9
 */
:- module test_parseable.

:- interface.

:- import_module io.

:- pred main(io.state, io.state).
:- mode main(di, uo) is det.

:- implementation.

:- import_module parseable.
:- import_module bool, float, integer, list, string.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

:- type point --->
	point(
		x :: int,
		y :: int
	).

:- instance parseable(point) where
	[
	 pred(parse/3) is parsePoint
	].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

main(!IO) :-
	list.foldl(test_parseFloat(yes), [0.3, 1.9, 123456789.12], !IO),
	
	test_parseInt(!IO),
	test_parseFloat(!IO)
	% List = [2, 1,3, 4],
	% (if
	% 	parsePoint(P, List, R)
	% then
	% 	io.format("Parsed %s into %s and remaining is %s\n", [s(string(List)), s(string(P)), s(string(R))], !IO)
	% else
	% 	io.format("Unable to parse %s into a point\n", [s(string(List))], !IO)
	% ),
	% test_integer(!IO)
	.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- pred test_parseInt(io.state, io.state).
:- mode test_parseInt(di, uo) is det.

test_parseInt(!IO) :-
	io.print("\nInt to parse? ", !IO),
	io.read_line_as_string(ILine, !IO),
	(if
		ILine = ok(SInt),
		string.to_int(string.strip(SInt), Int)
	then
		io.print("\nparseable.int32/3\n", !IO),
		parseable.int32(Int, List1, []),
		io.format("%d => %s\n", [i(Int), s(string(List1))], !IO),
		(if
			parseable.int32(NewInt1, List1, Remainder1)
		then
			io.format("%s => %d   leftover=%s   %s\n",
				[s(string(List1)), i(NewInt1), s(string(Remainder1)), s(if Int = NewInt1 then "ok" else "FAIL")], !IO)
		else
			io.print("Could not parse list to int!\n", !IO)
		),
		io.print("\nparseable.int16/3\n", !IO),
		parseable.int16(Int, List2, []),
		io.format("%d => %s\n", [i(Int), s(string(List2))], !IO),
		(if
			parseable.int16(NewInt2, List2, Remainder2)
		then
			io.format("%s => %d   leftover=%s   %s\n",
				[s(string(List2)), i(NewInt2), s(string(Remainder2)), s(if Int = NewInt2 then "ok" else "FAIL")], !IO)
		else
			io.print("Could not parse list to int!\n", !IO)
		),
		io.print("\nparseable.int8/3\n", !IO),
		parseable.int8(Int, List3, []),
		io.format("%d => %s\n", [i(Int), s(string(List3))], !IO),
		(if
			parseable.int8(NewInt3, List3, Remainder3)
		then
			io.format("%s => %d   leftover=%s   %s\n",
				[s(string(List3)), i(NewInt3), s(string(Remainder3)), s(if Int = NewInt3 then "ok" else "FAIL")], !IO)
		else
			io.print("Could not parse list to int!\n", !IO)
		),
		test_parseInt(!IO)
	else
		true
	).

:- pred test_parseFloat(io.state, io.state).
:- mode test_parseFloat(di, uo) is det.

test_parseFloat(!IO) :-
	io.print("\nFloat to parse? ", !IO),
	io.read_line_as_string(ILine, !IO),
	(if
		ILine = ok(SFloat),
		string.to_float(string.strip(SFloat), Float)
	then
		test_parseFloat(yes, Float, !IO),
		test_parseFloat(!IO)
	else
		true
	).


:- pred test_parseFloat(bool, float, io.state, io.state).
:- mode test_parseFloat(in, in, di, uo) is det.


test_parseFloat(Multiply, Float, !IO) :-
	io.print("\nparseable.float32/3\n", !IO),
	parseable.float32(Float, List3, []),
	io.format("%f => %s\n", [f(Float), s(string(List3))], !IO),
	(if
		parseable.float32(NewFloat3, List3, Remainder3)
	then
		io.format("%s => %f   leftover=%s   %s\n",
			[s(string(List3)), f(NewFloat3), s(string(Remainder3)), s(if Float = NewFloat3 then "ok" else "FAIL")], !IO),
		parseable.float32(NewFloat3, List5, []),
		io.format("%f => %s    %s\n", [f(NewFloat3), s(string(List5)), s(if List3 = List5 then "ok" else "FAIL")], !IO),
		(if
			parseable.float32(NewFloat5, List5, Remainder5)
		then
			io.format("%s => %f   leftover=%s   1vs3 %s 2vs3 %s\n",
				[s(string(List5)), f(NewFloat5), s(string(Remainder5)),
				 s(if Float = NewFloat5 then "ok" else "FAIL"),
				 s(if NewFloat3 = NewFloat5 then "ok" else "FAIL")], !IO),
			parseable.float32(NewFloat5, List8, []),
			io.format("%f => %s   l 1vs3 %s  l 2vs3 %s\n",
				[f(NewFloat5), s(string(List8)),
				 s(if List8 = List3 then "ok" else "FAIL"),
				 s(if List8 = List5 then "ok" else "FAIL")], !IO)
		else
			io.print("Could not parse list to float!\n", !IO)
		),
		(if
			Multiply = yes
		then
			test_parseFloat(no, NewFloat3 * 10.0, !IO)
		else
			true
		)
	else
		io.print("Could not parse list to float!\n", !IO)
	),
	io.nl(!IO).


:- pred test_integer(io.state, io.state).
:- mode test_integer(di, uo).


test_integer(!IO) :-
	io.print("\nInteger? ", !IO),
	io.read_line_as_string(ILine, !IO),
	(if
		ILine = ok(SInt),
		string.to_int(string.strip(SInt), Int)
	then
		Integer = integer(Int),
		io.format("%6d     = %s\n", [i(Int), s(string(Integer))], !IO),
		io.format("%6d*256 = %s\n", [i(Int), s(string(Integer * integer(256)))], !IO),
		io.format("%6d<<8  = %s\n", [i(Int), s(string(Integer << 8))], !IO),
		io.format("%6d<<24 = %s\n", [i(Int), s(string(Integer << 24))], !IO),
		Pred1 =
		(pred(Shift::in, IOdi::di, IOuo::uo) is det :-
			io.format("%6d<<%2d = %s\n", [i(Int), i(Shift), s(string(Integer << Shift))], IOdi, IOuo)
		),
		list.foldl(Pred1, 1..20, !IO),
		Pred2 =
		(pred(Shift::in, IOdi::di, IOuo::uo) is det :-
			io.format("%6d<<%2d+1 = %s\n", [i(Int), i(Shift), s(string(Integer << Shift + integer.one))], IOdi, IOuo)
		),
		list.foldl(Pred2, 1..20, !IO),
		test_integer(!IO)
	else
		true
	).

:- type parseInt --->
	b32 ;
	b16 ;
	bin.

:- pred runParser(string, pred(T, list(int), list.list(int)), pred(T, list(int), list.list(int)), T, io.state, io.state).
:- mode runParser(in, in(pred(in, out, in) is det), in(pred(out, in, out) is semidet), in, di, uo) is det.

runParser(ParserName, ParserPredIn, ParserPredOut, Value, !IO) :-
	ParserPredIn(Value, List, []),
	io.format("%s => %s\n", [s(string(Value)), s(string(List))], !IO),
	(if
		ParserPredOut(NewValue, List, Remainder)
	then
		io.format("%s => %s   leftover=%s\n", [s(string(List)), s(string(NewValue)), s(string(Remainder))], !IO)
	else
		io.format("Could not parse list using %s!\n", [s(ParserName)], !IO)
	).

:- pred parsePoint(point, list(int), list(int)).
:- mode parsePoint(in, out, in) is det.
:- mode parsePoint(out, in, out) is semidet.

parsePoint(Point) -->
	[Point^x, Point^y].


:- end_module test_parseable.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
