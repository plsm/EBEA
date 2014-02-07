/**
 * 

 * @author Pedro Mariano
 * @version 1.0 2013/04/15
 */
:- module parseable.

:- interface.

:- import_module bool, list, map.

:- typeclass parseable(T)
	where
[
	pred parse(T, list(int), list(int)),
	mode parse(in, out, in) is det,
	mode parse(out, in, out) is semidet
].

/**
 * Represents how lists should be parsed.

 * @cons normalType A list should be considered a normal type, each
 * constructor is assigned a different byte.  Suitable for lists with less
 * than five elements.

 * @cons withLength There is an initial 32 bit int with the list size.
 * List elements are not separated by any byte token.  Suitable for lists
 * with more than four elements.

 * @cons onlyElements(Length) Only the elements are parsed to bytes.
 * Suitable for lists that have the same length.
  
 */

:- type parseListMode --->
	normalType ;
	withLength ;
	onlyElements(int).


/**
 * This predicate parses a list of bytes to a list of {@code parseable}
 * elements.  Parameter {@code ParseListMode} specifies how the list is
 * parsed.

*/
:- pred parseList(parseListMode, list(T), list(int), list(int)) <= parseable(T).
:- mode parseList(in, in, out, in) is det.
:- mode parseList(in, out, in, out) is semidet.

/**
 * This predicate parses a list of bytes to a map of {@code parseable} keys
 * and elements.  The map is stored as a list of keys and elements pairs.
 * Parameter {@code ParseListMode} specifies how the list is parsed.

*/
:- pred parseMap(parseListMode, map(K, V), list(int), list(int)) <= (parseable(K), parseable(V)).
:- mode parseMap(in, in, out, in) is det.
:- mode parseMap(in, out, in, out) is semidet.

:- pred float32(float, list(int), list(int)).
:- mode float32(in, out, in) is det.
:- mode float32(out, in, out) is semidet.

:- pred bool(bool, list(int), list(int)).
:- mode bool(in, out, in) is det.
:- mode bool(out, in, out) is semidet.

:- pred int32(int, list(int), list(int)).
:- mode int32(in, out, in) is det.
:- mode int32(out, in, out) is semidet.

:- pred int16(int, list(int), list(int)).
:- mode int16(in, out, in) is det.
:- mode int16(out, in, out) is semidet.

:- pred int8(int, list(int), list(int)).
:- mode int8(in, out, in) is det.
:- mode int8(out, in, out) is semidet.


:- instance parseable(int).

:- instance parseable(float).

:- instance parseable(bool).


:- implementation.

:- import_module exception, int.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

:- instance parseable(int) where
[
	pred(parse/3) is int32
].

:- instance parseable(float) where
[
	pred(parse/3) is float32
].

:- instance parseable(bool) where
[
	pred(parse/3) is bool
].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions


parseList(normalType, []) -->
	[0].

parseList(normalType, [Head | Tail]) -->
	[1],
	parseable.parse(Head),
	parseList(normalType, Tail).

parseList(withLength, List) -->
	parseList_withLength(List).

parseList(onlyElements(Length), List) -->
	parseList_onlyElements(Length, List).

:- pragma promise_pure(parseMap/4).

parseMap(ParseListMode::in, Map::in, ListBytes::out, RestBytes::in) :-
	parseList(ParseListMode, map.keys(Map), ListBytes, TmpBytes),
	parseList(ParseListMode, map.values(Map), TmpBytes, RestBytes).

parseMap(ParseListMode::in, Map::out, ListBytes::in, RestBytes::out) :-
	parseList(ParseListMode, Keys, ListBytes, TmpBytes),
	parseList(ParseListMode, Values, TmpBytes, RestBytes),
	Map = map.from_corresponding_lists(Keys, Values)
	.

int32(Num, [Byte1, Byte2, Byte3, Byte4 | Rest], Rest) :-
	int32bits(Num, Byte1, Byte2, Byte3, Byte4).

int16(Num, [B0, B1 | Rest], Rest) :-
	int16Bytes(Num, B0, B1).

int8(Num, [B0 | Rest], Rest) :-
	int8bits(Num, B0).

% :- pragma promise_pure(int8/3).

% int8(Num::in, [Byte | Rest]::out, Rest::in) :-
% 	Num = Byte.

% int8(Num::out, [Byte | Rest]::in, Rest::out) :-
% 	(if
% 		Byte > 127
% 	then
% 		Num = 256 - Byte
% 	else
% 		Num = Byte
% 	).

float32(Num, [Byte1, Byte2, Byte3, Byte4 | Rest], Rest) :-
	float32bits(Num, Byte1, Byte2, Byte3, Byte4).

bool(no) --> [0].
bool(yes) --> [1].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions



:- pred parseList_withLength(list(T), list(int), list(int)) <= parseable(T).
:- mode parseList_withLength(in, out, in) is det.
:- mode parseList_withLength(out, in, out) is semidet.

:- pragma promise_pure(parseList_withLength/3).

parseList_withLength(ListElements::in, ListBytes::out, RestBytes::in) :-
	int32(list.length(ListElements), ListBytes, TmpBytes),
	parseList_onlyElements(list.length(ListElements), ListElements, TmpBytes, RestBytes).

parseList_withLength(ListElements::out, ListBytes::in, RestBytes::out) :-
	int32(NumberElements, ListBytes, TmpBytes),
	parseList_onlyElements(NumberElements, ListElements, TmpBytes, RestBytes).


:- pred parseList_onlyElements(int, list(T), list(int), list(int)) <= parseable(T).
:- mode parseList_onlyElements(in, in, out, in) is det.
:- mode parseList_onlyElements(in, out, in, out) is semidet.

:- pragma promise_pure(parseList_onlyElements/4).

parseList_onlyElements(NumberElements::in, ListElements::in, ListBytes::out, RestBytes::in) :-
	ListElements = [],
	(if
		NumberElements = 0
	then
		ListBytes = RestBytes
	else
		throw("parseList/4: there is a mismatch between the provided number of elements and the real number of elements in the list")
	)
	;
	ListElements = [Element | RestElements],
	parseable.parse(Element, ListBytes, TmpBytes),
	parseList_onlyElements(NumberElements - 1, RestElements, TmpBytes, RestBytes).

parseList_onlyElements(NumberElements::in, ListElements::out, ListBytes::in, RestBytes::out) :-
	(if
		NumberElements = 0
	then
		ListElements = [],
		ListBytes = RestBytes
	else
		ListElements = [Element | RestElements],
		parseable.parse(Element, ListBytes, TmpBytes),
		parseList_onlyElements(NumberElements - 1, RestElements, TmpBytes, RestBytes)
	).





:- pred float32bits(float, int, int, int, int).
:- mode float32bits(in, out, out, out, out) is det.
:- mode float32bits(out, in, in, in, in) is det.

:- pragma foreign_proc(
	"C",
	float32bits(Num::in, B0::out, B1::out, B2::out, B3::out),
	[will_not_call_mercury, thread_safe, promise_pure],
	"
	union {
		unsigned char bytes[4];
		float value;
	} conv;
	conv.value = Num;
	B0 = conv.bytes [0];
	B1 = conv.bytes [1];
	B2 = conv.bytes [2];
	B3 = conv.bytes [3];
	"
	).

:- pragma foreign_proc(
	"C",
	float32bits(Num::out, B0::in, B1::in, B2::in, B3::in),
	[will_not_call_mercury, thread_safe, promise_pure],
	"
	union {
		unsigned char bytes[4];
		float value;
	} conv;
							  printf (""sizeof(float)=%d\\n"", sizeof (float));
	conv.bytes [0] = B0;
	conv.bytes [1] = B1;
	conv.bytes [2] = B2;
	conv.bytes [3] = B3;
	Num = conv.value;
	"
	).

:- pragma foreign_proc(
	"Java",
	float32bits(Num::in, B0::out, B1::out, B2::out, B3::out),
	[will_not_call_mercury, thread_safe, promise_pure],
	"
	int NumInt = Float.floatToRawIntBits ((float) Num);
	B0 = NumInt & 0xFF;
	B1 = (NumInt >> 8) & 0xFF;
	B2 = (NumInt >> 16) & 0xFF;
	B3 = (NumInt >> 24) & 0xFF;
	"
	).

:- pragma foreign_proc(
	"Java",
	float32bits(Num::out, B0::in, B1::in, B2::in, B3::in),
	[will_not_call_mercury, thread_safe, promise_pure],
	"
	int NumInt = (B0 & 0xFF)
	| ((B1 &0xFF) << 8)
	| ((B2 &0xFF) << 16)
	| ((B3 &0xFF) << 24);
	Num = Float.intBitsToFloat (NumInt);
	"
	).


:- pred int32bits(int, int, int, int, int).
:- mode int32bits(in, out, out, out, out) is det.
:- mode int32bits(out, in, in, in, in) is det.

:- pragma foreign_proc(
	"C",
	int32bits(Num::in, B0::out, B1::out, B2::out, B3::out),
	[will_not_call_mercury, thread_safe, promise_pure],
	"
	union {
		unsigned char bytes[4];
		int32_t value;
	} conv;
	conv.value = (int32_t) Num;
	B0 = conv.bytes [0];
	B1 = conv.bytes [1];
	B2 = conv.bytes [2];
	B3 = conv.bytes [3];
	"
	).

:- pragma foreign_proc(
	"C",
	int32bits(Num::out, B0::in, B1::in, B2::in, B3::in),
	[will_not_call_mercury, thread_safe, promise_pure],
	"
	union {
		unsigned char bytes[4];
		int32_t value;
	} conv;
	conv.bytes [0] = B0;
	conv.bytes [1] = B1;
	conv.bytes [2] = B2;
	conv.bytes [3] = B3;
	Num = conv.value;
	"
	).

:- pragma foreign_proc(
	"Java",
	int32bits(Num::in, B0::out, B1::out, B2::out, B3::out),
	[will_not_call_mercury, thread_safe, promise_pure],
	"
	B0 = Num & 0xFF;
	B1 = (Num >> 8) & 0xFF;
	B2 = (Num >> 16) & 0xFF;
	B3 = (Num >> 24) & 0xFF;
	"
	).

:- pragma foreign_proc(
	"Java",
	int32bits(Num::out, B0::in, B1::in, B2::in, B3::in),
	[will_not_call_mercury, thread_safe, promise_pure],
	"
	Num = (B0 & 0xFF)
	| ((B1 &0xFF) << 8)
	| ((B2 &0xFF) << 16)
	| ((B3 &0xFF) << 24);
	"
	).


:- pred int16Bytes(int, int, int).
:- mode int16Bytes(in, out, out) is det.
:- mode int16Bytes(out, in, in) is det.

:- pragma foreign_proc(
	"C",
	int16Bytes(Num::in, B0::out, B1::out),
	[will_not_call_mercury, thread_safe, promise_pure],
	"
	union {
		unsigned char bytes[2];
		int16_t value;
	} conv;
	conv.value = (int16_t) Num;
	B0 = conv.bytes [0];
	B1 = conv.bytes [1];
	"
	).

:- pragma foreign_proc(
	"C",
	int16Bytes(Num::out, B0::in, B1::in),
	[will_not_call_mercury, thread_safe, promise_pure],
	"
	union {
		unsigned char bytes[2];
		int16_t value;
	} conv;
	conv.bytes [0] = B0;
	conv.bytes [1] = B1;
	Num = conv.value;
	"
	).

:- pragma foreign_proc(
	"Java",
	int16Bytes(Num::in, B0::out, B1::out),
	[will_not_call_mercury, thread_safe, promise_pure],
	"
	B0 = ((short) Num) & 0xFF;
	B1 = (((short) Num) >> 8) & 0xFF;
	"
	).

:- pragma foreign_proc(
	"Java",
	int16Bytes(Num::out, B0::in, B1::in),
	[will_not_call_mercury, thread_safe, promise_pure],
	"
	Num = (short) ( (((short) B0) & 0xFF)
	| (( ((short) B1) &0xFF) << 8) );
	"
	).


:- pred int8bits(int, int).
:- mode int8bits(in, out) is det.
:- mode int8bits(out, in) is det.

:- pragma foreign_proc(
	"C",
	int8bits(Num::in, B0::out),
	[will_not_call_mercury, thread_safe, promise_pure],
	"
	union {
		unsigned char bytes[1];
		int8_t value;
	} conv;
	conv.value = (int8_t) Num;
	B0 = conv.bytes [0];
	"
	).

:- pragma foreign_proc(
	"C",
	int8bits(Num::out, B0::in),
	[will_not_call_mercury, thread_safe, promise_pure],
	"
	union {
		unsigned char bytes[1];
		int8_t value;
	} conv;
	conv.bytes [0] = B0;
	Num = conv.value;
	"
	).

:- pragma foreign_proc(
	"Java",
	int8bits(Num::in, B0::out),
	[will_not_call_mercury, thread_safe, promise_pure],
	"
	B0 = ((byte) Num);
	"
	).

:- pragma foreign_proc(
	"Java",
	int8bits(Num::out, B0::in),
	[will_not_call_mercury, thread_safe, promise_pure],
	"
	Num = B0;
	"
	).

:- end_module parseable.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
