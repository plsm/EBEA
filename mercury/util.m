/**
 * 

 * @author Pedro Mariano
 * @version 1.0 2013/05/15
 */
:- module util.

:- interface.

%:- import_module scanable.
:- import_module array, bool, io, list, maybe, string.

/**
 * comment(Words)
  
 * While parsing a text file, comments must be skipped.  This predicate
 * success if the list of strings represents a comment.  This assumes a
 * line was read from the text stream using predicate {@code
 * io.read_line_as_string/4}
  
 */
:- pred comment(list(string)).
:- mode comment(in) is semidet.

:- func string + maybe_error(_) = string.

:- func joinError(maybe_error(_), maybe_error(_)) = maybe_error(_).
:- mode joinError(in, in) = out(bound(error(ground))) is det.

/**
 * readInt(Stream, LineFlag, Msg, MMin, MMax, MResult, !IO)
  
 * Try to read an integer that may be constrained to an interval {@code MMin} {@code MMax}.
 */
:- pred readInt(io.input_stream, bool, string, maybe(int), maybe(int), maybe(int), io, io).
:- mode readInt(in, in, in, in, in, out, di, uo) is det.

:- pred parseInt(string, string, maybe(int), maybe(int), maybe(int), io, io).
:- mode parseInt(in, in, in, in, out, di, uo) is det.

/**
 * spacePrint(Value, !IO)

 * Print a space then parameter {@code Value}. 
 */
:- pred spacePrint(T, io, io).
:- mode spacePrint(in, di, uo) is det.

/**
 * spacePrint(Stream, Value, !IO)

 * Print a space then parameter {@code Value}. 
 */
:- pred spacePrint(io.output_stream, T, io, io).
:- mode spacePrint(in, in, di, uo) is det.

% :- pred scanValues(io.input_stream, string, pred(string, T), scanable.result(list(T)), io, io).
% :- mode scanValues(in, in, in(pred(in, out) is semidet), out, di, uo) is det.

:- func rangeInt(int, int, int) = list(int).

/**
 * Call the command and handle the output of executing it.
 */
:- pred callSystem(string, io, io).
:- mode callSystem(in, di, uo) is det.

:- func arrayInitUnique(int, T) = array(T).
:- mode arrayInitUnique(in, in) = uo is det.

:- pred arrayInitUnique(int::in, T::in, array(T)::uo) is det.

:- func arrayUnsafeLookup(array(T), int) = T.
:- mode arrayUnsafeLookup(ui, in) = out is det.

:- pred arrayUnsafeSet(int, T, array(T), array(T)).
:- mode arrayUnsafeSet(in, in, di, uo) is det.

:- implementation.

:- import_module int.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

% scanValues(Stream, Label, MapPred, IMListValues, !IO) :-
% 	io.read_line_as_string(Stream, ILineValues, !IO),
% 	(if
% 		ILineValues = ok(LineValues)
% 	then
% 		(if
% 			string.words(LineValues) = [Label | StringValues]
% 		then
% 			(if
% 				list.map(MapPred, StringValues, TypeValues)
% 			then
% 				IMListValues = ok(ok(TypeValues))
% 			else
% 				IMListValues = ok(error(string.format("There are invalid %s values\n", [s(Label)])))
% 			)
% 		else
% 			IMListValues = ok(error(string.format("Expecting a line with %s values.\n", [s(Label)])))
% 		)
% 	else
% 		IMListValues = scanable.noOkToResult(string.format("%s values", [s(Label)]), ILineValues)
% 	).

comment([]).
comment(["#" | _]).

AC + New = Result :-
	New = error(NewMessage),
	(if
		AC = ""
	then
		Result = NewMessage
	else
		Result = AC ++ "; " ++ NewMessage
	)
	;
	New = ok(_),
	Result = AC
	.

joinError(New, AC) = Result :-
	AC = error(ACMessage),
	(
		New = error(NewMessage),
		Result = error(ACMessage ++ " " ++ NewMessage)
		;
		New = ok(_),
		Result = error(ACMessage)
	)
	;
	AC = ok(_),
	(
		New = error(NewMessage),
		Result = error(NewMessage)
		;
		New = ok(_),
		Result = error("")
	)
	.

readInt(Stream, LineFlag, Msg, MMin, MMax, MResult, !IO) :-
	% switch LineFlag
	LineFlag = no,
	io.read_word(Stream, RWord, !IO),
	(
		RWord = ok(Word),
		parseInt(string.from_char_list(Word), Msg, MMin, MMax, MResult, !IO)
		;
		RWord = eof,
		io.format(io.stderr_stream, "End-of-file reached while reading %s.\n", [s(Msg)], !IO),
		MResult = no
		;
		RWord = error(Error),
		io.format(io.stderr_stream, "Error reading %s:\n%s\n", [s(Msg), s(io.error_message(Error))], !IO),
		MResult = no
	)
	;
	LineFlag = yes,
	io.read_line_as_string(Stream, RWord, !IO),
	(
		RWord = ok(Word),
		parseInt(Word, Msg, MMin, MMax, MResult, !IO)
		;
		RWord = eof,
		io.format(io.stderr_stream, "End-of-file reached while reading %s.\n", [s(Msg)], !IO),
		MResult = no
		;
		RWord = error(Error),
		io.format(io.stderr_stream, "Error reading %s:\n%s\n", [s(Msg), s(io.error_message(Error))], !IO),
		MResult = no
	).

parseInt(Word, Msg, MMin, MMax, MResult, !IO) :-
	(if
		string.to_int(string.strip(Word), Int)
	then
		(if
			MMin = yes(Min),
			Int < Min
		then
			io.format(io.stderr_stream, "%s is below %d.\n", [s(Msg), i(Min)], !IO),
			MResult = no
		else if
			MMax = yes(Max),
			Int > Max
		then
			io.format(io.stderr_stream, "%s is higher than %d.\n", [s(Msg), i(Max)], !IO),
			MResult = no
		else
			MResult = yes(Int)
		)
	else
		io.format(io.stderr_stream, "Parse error while reading %s.\n", [s(Msg)], !IO),
		MResult = no
	).

spacePrint(Value, !IO) :-
	io.print(" ", !IO),
	io.print(Value, !IO).

spacePrint(Stream, Value, !IO) :-
	io.print(Stream, " ", !IO),
	io.print(Stream, Value, !IO).

rangeInt(Min, Step, Max) = Result :-
	(if
		Min > Max
	then
		Result = []
	else
		Result = [Min | rangeInt(Min + Step, Step, Max)]
	).


callSystem(Command, !IO) :-
	io.call_system(Command, IResult, !IO),
	(
		IResult = ok(ExitStatus),
		(if
			ExitStatus \= 0
		then
			io.format("Exit status is %d\n", [i(ExitStatus)], !IO)
		else
			true
		)
		;
		IResult = error(Error),
		io.format("Calling command `%s` resulted in the following error:\n%s\n", [s(Command), s(io.error_message(Error))], !IO)
	).

arrayInitUnique(Size, Item) = Result :-
	arrayInitUnique(Size, Item, Result).

:- pragma foreign_proc("C",
    arrayInitUnique(Size::in, Item::in, Array::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness,
        sharing(yes(int, T, array(T)), [
            cel(Item, []) - cel(Array, [T])
        ])
    ],
"
    ML_alloc_array(Array, Size + 1, MR_ALLOC_ID);
    ML_init_array(Array, Size, Item);
").

:- pragma foreign_proc("Java",
    arrayInitUnique(Size::in, Item::in, Array::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Array = jmercury.array.ML_new_array(Size, Item, true);
").


:- pragma foreign_proc("C",
    arrayUnsafeLookup(Array::ui, Index::in) = (Item::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness,
        sharing(yes(array(T), int, T), [
            cel(Array, [T]) - cel(Item, [])
        ])
    ],
"
    Item = Array->elements[Index];
").

:- pragma foreign_proc("Java",
    arrayUnsafeLookup(Array::ui, Index::in) = (Item::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if (Array instanceof int[]) {
        Item = ((int[]) Array)[Index];
    } else if (Array instanceof double[]) {
        Item = ((double[]) Array)[Index];
    } else if (Array instanceof char[]) {
        Item = ((char[]) Array)[Index];
    } else if (Array instanceof boolean[]) {
        Item = ((boolean[]) Array)[Index];
    } else {
        Item = ((Object[]) Array)[Index];
    }
").


:- pragma foreign_proc("C",
    arrayUnsafeSet(Index::in, Item::in, Array0::di, Array::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness,
        sharing(yes(int, T, array(T), array(T)), [
            cel(Array0, []) - cel(Array, []),
            cel(Item, [])   - cel(Array, [T])
        ])
    ],
"
    Array0->elements[Index] = Item; /* destructive update! */
    Array = Array0;
").

:- pragma foreign_proc("Java",
    arrayUnsafeSet(Index::in, Item::in, Array0::di, Array::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if (Array0 instanceof int[]) {
        ((int[]) Array0)[Index] = (Integer) Item;
    } else if (Array0 instanceof double[]) {
        ((double[]) Array0)[Index] = (Double) Item;
    } else if (Array0 instanceof char[]) {
        ((char[]) Array0)[Index] = (Character) Item;
    } else if (Array0 instanceof boolean[]) {
        ((boolean[]) Array0)[Index] = (Boolean) Item;
    } else {
        ((Object[]) Array0)[Index] = Item;
    }
    Array = Array0;         /* destructive update! */
").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- end_module util.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
