/**
 * Provides types to construct the logical layout of an user interface.
 * The purpose of this user interface is to display and edit some data and
 * perform some actions with it.  There are two main types that can be used
 * to construct a menu and a dialog.  Menus are used to perform actions
 * with the data, while dialogues are used to edit and display some data.
 * Menu actions always receive the IO state, as such they are represented
 * by predicates.  Dialog actions have get and set functions to access and
 * modify data fields.

 * <p> Menu actions always edit the data itself and not some field of the
 * data.  The field data cannot be existentially quantified.  The inst
 * declaration does not support it.
  
 * @author Pedro Mariano
 * @version 1.0 2013/09/14
 */
:- module userInterface.

:- interface.

:- import_module bool, io, list, maybe.

/**
 * An user interface can consist of a menu with options that trigger
 * actions or a dialog where the user can edit some data.
  
 */
:- type userInterface(D) --->
	m(menu(D)) ;
	d(dialog(D)).

:- inst userInterface == bound(
	m(menu) ;
	d(ground)
	).

/*
:- inst userInterface(D) == bound(
	m(menu(D)) ;
	d(dialog(D))
	).
*/

/**
 * Represents a menu with options that trigger actions on some data.  The
 * user can select the option to be performed.
 */

:- type menu(D) == list(menuItem(D)).

:- type menuItem(D) --->
	mi(mid :: interfaceData, menuAction(D)).

:- type menuAction(D) --->
	submenu(list(menuItem(D))) ;

	updateData(func(D) = D) ;
	updateDataIO(pred(D, D, io.state, io.state)) ;
	
	actionDataIO(pred(D, io.state, io.state)) ;
	
	actionIO(pred(io.state, io.state)) ;

	edit(dialog(D))
	.

:- inst menu == list_skel(menuItem).

:- inst menuItem == bound(mi(ground, menuAction)).

:- inst menuAction == bound(
	submenu(list_skel(menuItem)) ;
	updateData(ground) ;
	updateDataIO(pred(in, out, di, uo) is det) ;
	actionDataIO(pred(in, di, uo) is det) ;
	% updateDataIO(pred(in, out, di, uo) is cc_multi) ;
	% actionDataIO(pred(in, di, uo) is cc_multi) ;
	actionIO(pred(di, uo) is det) ;
	edit(ground)
	).


/*
:- inst menu(D) == bound(menu(list_skel(menuItem(D)))).

:- inst menuItem(D) == bound(mi(ground, menuAction(D))).

:- inst menuAction(D) == bound(
	submenu(menu(D)) ;
	updateData(func(in(D)) = out(D) is det) ;
	updateDataIO(pred(in(D), out(D), di, uo) is det) ;
	actionDataIO(pred(in(D), di, uo) is det) ;
	actionIO(pred(di, uo) is det) ;
	edit(ground)
	).
*/



/**
 * Represents a dialog with options to edit some data.
 */
:- type dialog(D) --->
	dialog(
		items   :: list(dialogItem(D))
	)
	;
	some [F]
	dialog(get(D, F), set(D, F), list(dialogItem(F))).

:- type dialogItem(D) --->
	di(interfaceData, dialogAction(D)) ;
	di(interfaceData).

:- type dialogAction(D) --->
	subdialog(list(dialogItem(D))) ;

	newValue(D) ;
	updateData(func(D) = D) ;
	
	some [F] (editField( get(D, F), set(D, F), list(dialogItem(F)))) ;
	
	updateFieldInt(    get(D, int),    set(D, int)) ;
	updateFieldString( get(D, string), set(D, string)) ;
	updateFieldFloat(  get(D, float),  set(D, float)) ;
	updateFieldBool(   get(D, bool),   set(D, bool)) ;

	some [F]
	editListFieldAny(      get(D, list(F)),      set(D, list(F)),     F, list(dialogItem(F)) ) ;
	
	updateListFieldInt(    get(D, list(int)),    set(D, list(int))              ) ;
	updateListFieldString( get(D, list(string)), set(D, list(string))           ) ;
	updateListFieldFloat(  get(D, list(float)),  set(D, list(float))            ) ;

	some [F]
	selectOneOf(get(D, F), set(D, F), options(F)) ;
	selectOneOf(options(D))
	.

:- type options(D) --->
	options(func(D) = maybe(int), list(choiceItem(D))).

/**
 * Represents an option that is presented to the user by dialog item {@code
 * oneOf/2}.  This constructor is used to display a set of mutually
 * exclusive options.
  
 */
:- type choiceItem(D) --->
	ci(interfaceData, list(dialogItem(D))) ;
	ci(interfaceData, D, list(dialogItem(D))).

/*
:- inst dialog(D) == bound(dialog(list_skel(dialogItem(D)))).

:- inst dialogItem(D) == bound(d(ground, dialogAction(D))).

:- inst dialogAction(D) --->
	subdialog(dialog(D)) ;
	updateData(set1(D)) ;
	editField(get(D), set2(D), dialog(ground)) ;
	updateFieldInt(    get(D), set2(D)) ;
	updateFieldFloat(  get(D), set2(D)) ;
	updateFieldString( get(D), set2(D)) ;
	editListFieldAny(  get(D), set2(D), ground, dialog(ground)) ;
	updateListFieldInt(    get(D), set2(D)) ;
	updateListFieldFloat(  get(D), set2(D)) ;
	updateListFieldString( get(D), set2(D))
	.
*/


:- type interfaceData --->
	label(string).

:- type get(D, F) == (func(D) = F).

:- type set(D, F) == (func(D, F) = setResult(D)).

:- type set(D) == (func(D) = setResult(D)).

:- type setResult(D) == maybe_error(D).

/*
:- inst get(D) == (func(in(D)) = out is det).

:- inst set1(D) == (func(in(D)) = out(maybe_error(D)) is det).

:- inst set2(D) == (func(in(D), in) = out(maybe_error(D)) is det).
*/

% getter functions

:- func mid(menuItem(D)) = interfaceData.
:- func did(dialogItem(D)) = interfaceData.


:- type limit(T) --->
	bounded(T, bool) ;
	unbound.

/**
 * checkFloat(FieldName, LowerLimit, UpperLimit, Setter, Data, NewFieldValue) = NewData
 */
:- func checkFloat(string, limit(float), limit(float), func(T, float) = T, T, float) = setResult(T).

/**
 * checkInt(FieldName, LowerLimit, UpperLimit, Setter, Data, NewFieldValue) = NewData
 */
:- func checkInt(string, limit(int), limit(int), func(T, int) = T, T, int) = setResult(T).


/**
 * checkProbability(FieldName, Setter, Data, NewFieldValue) = NewData
 */
:- func checkProbability(string, func(T, float) = T, T, float) = setResult(T).



:- func set(func(D, F) = D) = (func(D, F) = setResult(D)).

:- implementation.

:- import_module int, float, string.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

set(Setter) =
	(func(D, F) = R :-
		R = ok(Setter(D, F))
	).

did(di(Result, _)) = Result.
did(di(Result)) = Result.

checkFloat(FieldValue, Lower, Upper, Setter, Data, Value) = Result :-
	Lower = unbound,
	Upper = unbound,
	Result = ok(Setter(Data, Value))
	;
	Lower = bounded(Min, no),
	Upper = unbound,
	(if
		Value > Min
	then
		Result = ok(Setter(Data, Value))
	else
		Result = error(string.format("%s must be greater than %f", [s(FieldValue), f(Min)]))
	)
	;
	Lower = bounded(Min, yes),
	Upper = unbound,
	(if
		Value >= Min
	then
		Result = ok(Setter(Data, Value))
	else
		Result = error(string.format("%s must be greater than or equal to %f", [s(FieldValue), f(Min)]))
	)
	;
	Lower = unbound,
	Upper = bounded(Max, no),
	(if
		Value < Max
	then
		Result = ok(Setter(Data, Value))
	else
		Result = error(string.format("%s must be less than %f", [s(FieldValue), f(Max)]))
	)
	;
	Lower = bounded(Min, no),
	Upper = bounded(Max, no),
	(if
		Value > Min,
		Value < Max
	then
		Result = ok(Setter(Data, Value))
	else
		Result = error(string.format("%s must be between %f and %f exclusively", [s(FieldValue), f(Min), f(Max)]))
	)
	;
	Lower = bounded(Min, yes),
	Upper = bounded(Max, no),
	(if
		Value >= Min,
		Value < Max
	then
		Result = ok(Setter(Data, Value))
	else
		Result = error(string.format("%s must be between %f and %f (exclusively)", [s(FieldValue), f(Min), f(Max)]))
	)
	;
	Lower = unbound,
	Upper = bounded(Max, yes),
	(if
		Value =< Max
	then
		Result = ok(Setter(Data, Value))
	else
		Result = error(string.format("%s must be less than or equal to %f", [s(FieldValue), f(Max)]))
	)
	;
	Lower = bounded(Min, no),
	Upper = bounded(Max, yes),
	(if
		Value > Min,
		Value =< Max
	then
		Result = ok(Setter(Data, Value))
	else
		Result = error(string.format("%s must be between %f and %f (inclusively)", [s(FieldValue), f(Min), f(Max)]))
	)
	;
	Lower = bounded(Min, yes),
	Upper = bounded(Max, yes),
	(if
		Value >= Min,
		Value =< Max
	then
		Result = ok(Setter(Data, Value))
	else
		Result = error(string.format("%s must be between %f and %f (inclusively)", [s(FieldValue), f(Min), f(Max)]))
	).

checkProbability(FieldName, Setter, Data, NewFieldValue) = checkFloat(FieldName, bounded(0.0, yes), bounded(1.0, yes), Setter, Data, NewFieldValue).

checkInt(FieldName, Lower, Upper, Setter, Data, Value) = Result :-
	Lower = unbound,
	Upper = unbound,
	Result = ok(Setter(Data, Value))
	;
	Lower = bounded(Min, no),
	Upper = unbound,
	(if
		Value > Min
	then
		Result = ok(Setter(Data, Value))
	else
		Result = error(string.format("%s must be greater than %i", [s(FieldName), i(Min)]))
	)
	;
	Lower = bounded(Min, yes),
	Upper = unbound,
	(if
		Value >= Min
	then
		Result = ok(Setter(Data, Value))
	else
		Result = error(string.format("%s must be greater than or equal to %i", [s(FieldName), i(Min)]))
	)
	;
	Lower = unbound,
	Upper = bounded(Max, no),
	(if
		Value < Max
	then
		Result = ok(Setter(Data, Value))
	else
		Result = error(string.format("%s must be less than %i", [s(FieldName), i(Max)]))
	)
	;
	Lower = bounded(Min, no),
	Upper = bounded(Max, no),
	(if
		Value > Min,
		Value < Max
	then
		Result = ok(Setter(Data, Value))
	else
		Result = error(string.format("%s must be between %i and %i exclusively", [s(FieldName), i(Min), i(Max)]))
	)
	;
	Lower = bounded(Min, yes),
	Upper = bounded(Max, no),
	(if
		Value >= Min,
		Value < Max
	then
		Result = ok(Setter(Data, Value))
	else
		Result = error(string.format("%s must be between %i and %i (exclusively)", [s(FieldName), i(Min), i(Max)]))
	)
	;
	Lower = unbound,
	Upper = bounded(Max, yes),
	(if
		Value =< Max
	then
		Result = ok(Setter(Data, Value))
	else
		Result = error(string.format("%s must be less than or equal to %i", [s(FieldName), i(Max)]))
	)
	;
	Lower = bounded(Min, no),
	Upper = bounded(Max, yes),
	(if
		Value > Min,
		Value =< Max
	then
		Result = ok(Setter(Data, Value))
	else
		Result = error(string.format("%s must be between %i and %i (inclusively)", [s(FieldName), i(Min), i(Max)]))
	)
	;
	Lower = bounded(Min, yes),
	Upper = bounded(Max, yes),
	(if
		Value >= Min,
		Value =< Max
	then
		Result = ok(Setter(Data, Value))
	else
		Result = error(string.format("%s must be between %i and %i (inclusively)", [s(FieldName), i(Min), i(Max)]))
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- end_module userInterface.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
