/**
 * 

 * @author Pedro Mariano
 * @version 1.0 2013/09/14
 */
:- module ui_console.

:- interface.

:- import_module userInterface.
:- import_module io.

:- pred show(userInterface(D), D, D, io.state, io.state).
%:- mode show(in(userInterface), di, uo, di, uo) is det.
:- mode show(in(userInterface), in, out, di, uo) is det.
%:- mode show(in(userInterface(D)), in(D), out(D), di, uo) is det.

:- implementation.

:- import_module bool, exception, float, int, list, math, maybe, string.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

:- type navigation ---> nav(list(string)).

:- type additional ---> insert ; editDelete ; nothing.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

show(UserInterface, !Data, !IO) :-
	UserInterface = m(Menu),
	showMenu(Menu, nav([]), !Data, !IO)
	;
	UserInterface = d(Dialog),
	(
		Dialog = dialog(ListDialogItem),
		showDialog(ListDialogItem, nav([]), !Data, !IO)
		;
		Dialog = dialog(Get, Set, ListDialogItem),
		showDialog(ListDialogItem, nav([]), Get(!.Data), NextField, !IO),
		MData = Set(!.Data, NextField),
		handleResult(MData, !Data, !IO)
	)
	.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions


:- pred showMenu(menu(D), navigation, D, D, io.state, io.state).
:- mode showMenu(in(menu), in, in, out, di, uo) is det.

showMenu(ListMenuItem, Navigation, !Data, !IO) :-
	ListInterfaceData = list.map(userInterface.mid, ListMenuItem),
	printInterface(ListInterfaceData, Navigation, !IO),
	askActionUser(list.length(ListInterfaceData), SelectedAction, !IO),
	(if
		SelectedAction > 0,
		getMenuItem(ListMenuItem, SelectedAction) = MenuItem,
		MenuItem = mi(MenuInterfaceData, MenuAction)
	then
		MenuAction = submenu(Menu),
		showMenu(Menu, nextNavigation(Navigation, MenuInterfaceData), !Data, !IO)
		;
		MenuAction = updateData(FuncMap),
		!:Data = FuncMap(!.Data)
		;
		MenuAction = updateDataIO(PredMap),
		PredMap(!Data, !IO)
		;
		MenuAction = actionDataIO(PredAct),
		PredAct(!.Data, !IO)
		;
		MenuAction = actionIO(PredAct),
		PredAct(!IO)
		;
		MenuAction = edit(Dialog),
		(
			Dialog = dialog(ListDialogItem),
			showDialog(ListDialogItem, nextNavigation(Navigation, MenuInterfaceData), !Data, !IO)
			;
			Dialog = dialog(Get, Set, ListDialogItem),
			showDialog(ListDialogItem, Navigation, Get(!.Data), NextField, !IO),
			MData = Set(!.Data, NextField),
			handleResult(MData, !Data, !IO)
		)
	else
		true
	),
	(if
		SelectedAction > 0
	then
		showMenu(ListMenuItem, Navigation, !Data, !IO)
	else
		true
	).

:- pred showDialog(list(dialogItem(D)), navigation, D, D, io.state, io.state).
:- mode showDialog(in, in, in, out, di, uo) is det.

showDialog(ListDialogItem, Navigation, !Data, !IO) :-
	ListInterfaceData = list.map(userInterface.did, ListDialogItem),
	printInterface(ListInterfaceData, Navigation, !IO),
	askActionUser(list.length(ListInterfaceData), SelectedAction, !IO),
	(if
		SelectedAction > 0,
		list.det_index1(ListDialogItem, SelectedAction) = DialogItem
	then
		handleDialogAction(Navigation, DialogItem, !Data, !IO)
	else
		true
	),
	(if
		SelectedAction > 0
	then
		showDialog(ListDialogItem, Navigation, !Data, !IO)
	else
		true
	).

:- pred handleDialogAction(navigation, userInterface.dialogItem(D), D, D, io.state, io.state).
:- mode handleDialogAction(in, in, in, out, di, uo) is det.
%:- mode handleDialogAction(in, in(dialogItem(D)), in(D), out(D), di, uo) is det.

handleDialogAction(_Navigation, di(DialogInterfaceData), !Data, !IO) :-
	DialogInterfaceData = label(Option),
	io.print(Option, !IO),
	io.nl(!IO),
	true.

handleDialogAction(Navigation, di(DialogInterfaceData, DialogAction), !Data, !IO) :-
	DialogAction = subdialog(Dialog),
	showDialog(Dialog, nextNavigation(Navigation, DialogInterfaceData), !Data, !IO)
	.

handleDialogAction(Navigation, di(DialogInterfaceData, DialogAction), !Data, !IO) :-
	DialogAction = editField(Get, Set, Dialog),
	showDialog(Dialog, nextNavigation(Navigation, DialogInterfaceData), Get(!.Data), NextField, !IO),
	MData = Set(!.Data, NextField),
	handleResult(MData, !Data, !IO)
	.

handleDialogAction(_Navigation, di(_DialogInterfaceData, DialogAction), !Data, !IO) :-
	DialogAction = updateFieldBool(Get, Set),
	Bool = bool.not(Get(!.Data)),
	io.format("-> %s", [s(string(Bool))], !IO),
	Set(!.Data, Bool) = MData,
	handleResult(MData, !Data, !IO)
	.

handleDialogAction(_Navigation, di(_DialogInterfaceData, DialogAction), !Data, !IO) :-
	DialogAction = updateFieldInt(Get, Set),
	io.format("[%d] ? ", [i(Get(!.Data))], !IO),
	io.flush_output(!IO),
	io.read_line_as_string(ILine, !IO),
	(if
		ILine = ok(Line),
		string.to_int(string.strip(Line), Int)
	then
		Set(!.Data, Int) = MData,
		handleResult(MData, !Data, !IO)
	else
		io.print("Not an integer\n", !IO)
	)
	.

handleDialogAction(_Navigation, di(_DialogInterfaceData, DialogAction), !Data, !IO) :-
	DialogAction = updateFieldFloat(Get, Set),
	io.format("[%f] ? ", [f(Get(!.Data))], !IO),
	io.flush_output(!IO),
	io.read_line_as_string(ILine, !IO),
	(if
		ILine = ok(Line),
		string.to_float(string.strip(Line), Float)
	then
		Set(!.Data, Float) = MData,
		handleResult(MData, !Data, !IO)
	else
		io.print("Not a floating point number\n", !IO)
	)
	.

handleDialogAction(_Navigation, di(_DialogInterfaceData, DialogAction), !Data, !IO) :-
	DialogAction = updateFieldString(Get, Set),
	io.format("[%s] ?", [s(Get(!.Data))], !IO),
	io.flush_output(!IO),
	io.read_line_as_string(ILine, !IO),
	(if
		ILine = ok(Line),
		string.strip(Line) = Str
	then
		Set(!.Data, Str) = MData,
		handleResult(MData, !Data, !IO)
	else
		io.print("Not a valid string\n", !IO)
	)
	.

handleDialogAction(Navigation, di(DialogInterfaceData, DialogAction), !Data, !IO) :-
	DialogAction = updateListFieldFloat(Get, Set),
	handleEditListFieldAction(nextNavigation(Navigation, DialogInterfaceData), Get, Set, string, readValueForList(string.to_float), no, !Data, !IO)
	.

handleDialogAction(Navigation, di(DialogInterfaceData, DialogAction), !Data, !IO) :-
	DialogAction = updateListFieldInt(Get, Set),
	handleEditListFieldAction(nextNavigation(Navigation, DialogInterfaceData), Get, Set, string, readValueForList(string.to_int), no, !Data, !IO)
	.

handleDialogAction(Navigation, di(DialogInterfaceData, DialogAction), !Data, !IO) :-
	DialogAction = updateListFieldString(Get, Set),
	MapString = (func(X) = X :- true),
	Parse = (pred(Y::in, Y::out) is semidet :- true),
	handleEditListFieldAction(nextNavigation(Navigation, DialogInterfaceData), Get, Set, MapString, readValueForList(Parse), no, !Data, !IO)
	.

handleDialogAction(Navigation, di(DialogInterfaceData, DialogAction), !Data, !IO) :-
	DialogAction = editListFieldAny(Get, Set, DefaultValue, Dialog),
	TruncString =
	(func(X) = R :-
		SX = string(X),
		(if
			string.length(SX) > 100
		then
			R = string.left(SX, 97) ++ "..."
		else
			R = SX
		)
	),
	ReadValueForList =
	(pred(Nav::in, MValue::out, IOdi::di, IOuo::uo) is det :-
		showDialog(Dialog, Nav, DefaultValue, Value, IOdi, IOuo),
		MValue = yes(Value)
	),
	EditListValue =
	(pred(Nav::in, Value::in, Result::out, IOdi::di, IOuo::uo) is det :-
		showDialog(Dialog, Nav, Value, Result, IOdi, IOuo)
	),
	handleEditListFieldAction(nextNavigation(Navigation, DialogInterfaceData), Get, Set, TruncString, ReadValueForList, yes(EditListValue), !Data, !IO).


handleDialogAction(_Navigation, di(_DialogInterfaceData, DialogAction), !Data, !IO) :-
	DialogAction = updateData(Set),
	Set(!.Data) = !:Data.
%	handleResult(Set(!.Data), !Data, !IO).

handleDialogAction(_Navigation, di(_DialogInterfaceData, DialogAction), _, Data, !IO) :-
	DialogAction = newValue(Data).

handleDialogAction(Navigation, di(DialogInterfaceData, DialogAction), !Data, !IO) :-
	DialogAction = selectOneOf(options(FuncSelectedIndex, ListChoices)),
	MapToString =
	(pred(CI::in, S::out, Idx::in, NextIdx::out) is det :-
		NextIdx = Idx + 1,
		(
			CI = ci(ChoiceItemInterfaceData, _, _)
			;
			CI = ci(ChoiceItemInterfaceData, _)
		),
		S1 = (if yes(Idx) = FuncSelectedIndex(!.Data) then "(*)" else "( )"),
		(
			ChoiceItemInterfaceData = label(Option),
			S2 = Option
		),
		S = S1 ++ " " ++ S2
	),
	list.map_foldl(MapToString, ListChoices, ListStrings, 0, _),
	printList(nothing, ListStrings, nextNavigation(Navigation, DialogInterfaceData), !IO),
	askActionUser(list.length(ListStrings), SelectedAction, !IO),
	(if
		SelectedAction > 0
	then
		list.det_split_list(SelectedAction - 1, ListChoices, Start, End),
		ChoiceItem = list.det_head(End),
		(
			ChoiceItem = ci(ValueInterfaceData, Value, Dialog),
			(
				Dialog = [],
				!:Data = Value
				;
				Dialog = [_|_],
				(if
					FuncSelectedIndex(!.Data) = yes(SelectedAction - 1)
				then
					showDialog(Dialog, nextNavigation(Navigation, ValueInterfaceData), !Data, !IO)
				else
					showDialog(Dialog, nextNavigation(Navigation, ValueInterfaceData), Value, !:Data, !IO)
				)
			)
			;
			ChoiceItem = ci(ValueInterfaceData, Dialog),
			(
				Dialog = []
				;
				Dialog = [_|_],
				(if
					FuncSelectedIndex(!.Data) = yes(SelectedAction - 1)
				then
					showDialog(Dialog, nextNavigation(Navigation, ValueInterfaceData), !Data, !IO)
				else
					true
				)
			)
		)
	else
		true
	)
	.

handleDialogAction(Navigation, di(DialogInterfaceData, DialogAction), !Data, !IO) :-
	DialogAction = selectOneOf(GetFunc, SetFunc, options(FuncSelectedIndex, ListChoices)),
	MapToString =
	(pred(CI::in, S::out, Idx::in, NextIdx::out) is det :-
		NextIdx = Idx + 1,
		(
			CI = ci(ChoiceItemInterfaceData, _, _)
			;
			CI = ci(ChoiceItemInterfaceData, _)
		),
		S1 = (if yes(Idx) = FuncSelectedIndex(GetFunc(!.Data)) then "(*)" else "( )"),
		(
			ChoiceItemInterfaceData = label(Option),
			S2 = Option
		),
		S = S1 ++ " " ++ S2
	),
	list.map_foldl(MapToString, ListChoices, ListStrings, 0, _),
	printList(nothing, ListStrings, nextNavigation(Navigation, DialogInterfaceData), !IO),
	askActionUser(list.length(ListStrings), SelectedAction, !IO),
	(if
		SelectedAction > 0
	then
		list.det_split_list(SelectedAction - 1, ListChoices, Start, End),
		ChoiceItem = list.det_head(End),
		(
			ChoiceItem = ci(ValueInterfaceData, AValue, Dialog),
			(
				Dialog = [],
				MNextValue = yes(AValue)
				;
				Dialog = [_|_],
				(if
					FuncSelectedIndex(GetFunc(!.Data)) = yes(SelectedAction - 1)
				then
					showDialog(Dialog, nextNavigation(Navigation, ValueInterfaceData), GetFunc(!.Data), NextValue, !IO)
				else
					showDialog(Dialog, nextNavigation(Navigation, ValueInterfaceData), AValue, NextValue, !IO)
				),
				MNextValue = yes(NextValue)
			)
			;
			ChoiceItem = ci(ValueInterfaceData, Dialog),
			(
				Dialog = [],
				MNextValue = no
				;
				Dialog = [_|_],
				(if
					FuncSelectedIndex(GetFunc(!.Data)) = yes(SelectedAction - 1)
				then
					showDialog(Dialog, nextNavigation(Navigation, ValueInterfaceData), GetFunc(!.Data), NextValue, !IO),
					MNextValue = yes(NextValue)
				else
					MNextValue = no
				)
			)
		),
		(
			MNextValue = yes(NV),
			SetFunc(!.Data, NV) = MData,
			handleResult(MData, !Data, !IO)
			;
			MNextValue = no
		)
	else
		true
	)
	.

	
:- pred readValueForList(pred(string, T), navigation, maybe(T), io.state, io.state).
:- mode readValueForList(in(pred(in, out) is semidet), in, out, di, uo) is det.

readValueForList(Parse, _, MValue, !IO) :-
	io.print("? ", !IO),
	io.read_line_as_string(ILine, !IO),
	(
		ILine = ok(Line),
		(if
			Parse(string.strip(Line), NewValue)
		then
			MValue = yes(NewValue)
		else
			io.print("Parse error\n", !IO),
			MValue = no
		)
		;
		ILine = eof,
		MValue = no
		;
		ILine = error(_),
		MValue = no
	).

:- pred handleEditListFieldAction(
	navigation,
	userInterface.get(D, list(F)), userInterface.set(D, list(F)),
	func(F) = string,
	pred(navigation, maybe(F), io.state, io.state),
	maybe(pred(navigation, F, F, io.state, io.state)),
	D, D,
	io.state, io.state).
:- mode handleEditListFieldAction(
	in, in, in, in,
	in(pred(in, out, di, uo) is det),
	in(maybe(pred(in, in, out, di, uo) is det)),
	in, out, di, uo) is det.

handleEditListFieldAction(Navigation, Get, Set, MapString, ReadValueForList, MEditListValue, !Data, !IO) :-
	Get(!.Data) = ListOptions,
	printList(insert, list.map(MapString, ListOptions), Navigation, !IO),
	askActionUser(list.length(ListOptions) + 1, SelectedAction, !IO),
	(if
		SelectedAction = list.length(ListOptions) + 1
	then
		ReadValueForList(nextNavigation(Navigation, label("insert")), MValue, !IO),
		(
			MValue = yes(Value),
			Set(!.Data, list.append(ListOptions, [Value])) = MData,
			handleResult(MData, !Data, !IO)
			;
			MValue = no
		),
		handleEditListFieldAction(Navigation, Get, Set, MapString, ReadValueForList, MEditListValue, !Data, !IO)
	else if
		SelectedAction =< list.length(ListOptions),
		SelectedAction > 0
	then
		list.det_split_list(SelectedAction - 1, ListOptions, Start, End),
		list.det_tail(End) = NewEnd,
		(
			MEditListValue = no,
			MData = Set(!.Data, list.append(Start, NewEnd))
			;
			MEditListValue = yes(EditListValue),
			handleEditDeleteListValue(nextNavigation(Navigation, label(string.format("element #%d", [i(SelectedAction)]))), MapString, EditListValue, list.det_head(End), Result, !IO),
			(
				Result = no,
				MData = Set(!.Data, list.append(Start, NewEnd))
				;
				Result = yes(NewValue),
				MData = Set(!.Data, list.append(Start, [NewValue | NewEnd]))
			)
		),
		handleResult(MData, !Data, !IO),
		handleEditListFieldAction(Navigation, Get, Set, MapString, ReadValueForList, MEditListValue, !Data, !IO)
	else if
		SelectedAction = 0
	then
		true
	else
		throw("handleEditListFieldAction/9: Never reached, invalid selected action")
	).

:- pred handleEditDeleteListValue(navigation, func(T) = string, pred(navigation, T, T, io.state, io.state), T, maybe(T), io.state, io.state).
:- mode handleEditDeleteListValue(in, in, in(pred(in, in, out, di, uo) is det), in, out, di, uo) is det.

handleEditDeleteListValue(Navigation, MapString, EditListValue, Value, Result, !IO) :-
	printList(editDelete, [MapString(Value)], Navigation, !IO),
	askActionUser(3, SelectedAction, !IO),
	(if
		SelectedAction = 2
	then
		EditListValue(nextNavigation(Navigation, label("edit")), Value, NextValue, !IO),
		Result = yes(NextValue)
	else if
		SelectedAction = 3
	then
		Result = no
	else if
		SelectedAction = 1
	then
		handleEditDeleteListValue(Navigation, MapString, EditListValue, Value, Result, !IO)
	else if
		SelectedAction = 0
	then
		Result = yes(Value)
	else
		throw("handleEditDeleteListValue/7: Never reached, invalid selected action")
	).


/**
 * Handles the result of setting the new value of the data after the user edited some field.
 */
:- pred handleResult(maybe_error(D), D, D, io.state, io.state).
:- mode handleResult(in, in, out, di, uo) is det.

handleResult(ok(Data), _, Data, !IO).

handleResult(error(Msg), !Data, !IO) :-
	io.print(Msg, !IO),
	io.nl(!IO).

/**
 * Print an interface with the available options.  This is used to present a menu or a dialog.
 */
:- pred printInterface(list(interfaceData), navigation, io.state, io.state).
:- mode printInterface(in, in, di, uo) is det.

printInterface(ListInterfaceData, Navigation, !IO) :-
	Navigation = nav(Title),
	% calculate number of chars occupied by actions numbers
	(
		ListInterfaceData = [],
		WidthIdx = 1
		;
		ListInterfaceData = [_ | _],
		WidthIdx = float.ceiling_to_int(math.log10(float(list.length(ListInterfaceData) + 1)))
	),
	% print navigation
	PredPrintNavigation =
	(pred(Option::in, IOdi1::di, IOuo1::uo) is det :-
		io.format(" » %s", [s(Option)], IOdi1, IOuo1)
	),
	list.foldr(PredPrintNavigation, Title, !IO),
	io.nl(!IO),
	% calculate the longest menu item
	FuncMaxLength =
	(func(InterfaceData, Length) = Result :-
		InterfaceData = label(Option),
		Result = int.max(string.length(Option), Length)
	),
	list.foldl(FuncMaxLength, ListInterfaceData, 4) = MaxLength,
	% print header  ▄▀▌▐
	% print header ▄▀▌▐
	io.format(" %s\n", [s(string.pad_right("", '▄', MaxLength + 5 + WidthIdx))], !IO),
	% print menu items
	PredPrintOption =
	(pred(InterfaceData::in, N::in, NN::out, IOdi2::di, IOuo2::uo) is det :-
		InterfaceData = label(Option),
		io.format("▐ %*d - %s ▌\n", [i(WidthIdx), i(N), s(string.pad_right(Option, ' ', MaxLength))], IOdi2, IOuo2),
		NN = N + 1
	),
	list.foldl2(PredPrintOption, ListInterfaceData, 1, _, !IO),
	% print the return from menu item (exit or back)
	io.format("▐ %s - %s ▌\n", [s(string.pad_left("0", ' ', WidthIdx)), s(string.pad_right(if Title = [] then "Exit" else "Back", ' ', MaxLength))], !IO),
	io.format(" %s\n", [s(string.pad_right("", '▀', MaxLength + 5 + WidthIdx))], !IO).





:- pred printList(additional, list(string), navigation, io.state, io.state).
:- mode printList(in, in, in, di, uo) is  det.

printList(Additional, List, Navigation, !IO) :-
	% print navigation
	Navigation = nav(Title),
	PredPrintNavigation =
	(pred(Option::in, IOdi1::di, IOuo1::uo) is det :-
		io.format(" » %s", [s(Option)], IOdi1, IOuo1)
	),
	list.foldr(PredPrintNavigation, Title, !IO),
	io.nl(!IO),
	% calculate number of chars occupied by actions numbers
	(
		List = [],
		WidthIdx = 1
		;
		List = [_ | _],
		WidthIdx = float.ceiling_to_int(math.log10(float(list.length(List) + 1)))
	),
	% calculate the longest menu item
	FuncMaxLength =
	(func(String, Length) = Result :-
		Result = int.max(string.length(String), Length)
	),
	list.foldl(FuncMaxLength, List, 6) = MaxLength,
	% print top horizontal line
	io.format(" %s\n", [s(string.pad_right("", '▄', MaxLength + 5 + WidthIdx))], !IO),
	% print list elements
	PredPrintOption =
	(pred(String::in, N::in, NN::out, IOdi2::di, IOuo2::uo) is det :-
		io.format("▐ %*d - %s ▌\n", [i(WidthIdx), i(N), s(string.pad_right(String, ' ', MaxLength))], IOdi2, IOuo2),
		NN = N + 1
	),
	list.foldl2(PredPrintOption, List, 1, Number, !IO),
	% print insert option
	(
		Additional = nothing
		;
		Additional = insert,
		io.format("▐ %*d - %s ▌\n", [i(WidthIdx), i(Number), s(string.pad_right("Insert", ' ', MaxLength))], !IO)
		;
		Additional = editDelete,
		io.format("▐ %*d - %s ▌\n", [i(WidthIdx), i(Number), s(string.pad_right("Edit", ' ', MaxLength))], !IO),
		io.format("▐ %*d - %s ▌\n", [i(WidthIdx), i(Number + 1), s(string.pad_right("Delete", ' ', MaxLength))], !IO)
	),
	% print the return from field list dialog
	io.format("▐ %s - %s ▌\n", [s(string.pad_left("0", ' ', WidthIdx)), s(string.pad_right("Back", ' ', MaxLength))], !IO),
	% print bottom horizontal line
	io.format(" %s\n", [s(string.pad_right("", '▀', MaxLength + 5 + WidthIdx))], !IO).
	


/**
 * askActionUser(NumberOptions, SelectedActionIndex, !IO)
 *
 * Asks the user for a integer, which must be between 0 and the number of
 * available menu or dialog actions.
 */
:- pred askActionUser(int, int, io.state, io.state).
:- mode askActionUser(in, out, di, uo) is det.

askActionUser(NumberOptions, Result, !IO) :-
	io.write_string("? ", !IO),
	io.flush_output(!IO),
	io.read_line_as_string(ILine, !IO),
	(
		ILine = ok(Line),
		(if
			string.to_int(string.strip(Line), Int)
		then
			(if
				Int >= 0,
				Int =< NumberOptions
			then
				Result = Int
			else
				io.print("Invalid option!\n", !IO),
				askActionUser(NumberOptions, Result, !IO)
			)
		else
			io.print("Invalid option!\n", !IO),
			askActionUser(NumberOptions, Result, !IO)
		)
		;
		ILine = eof,
		Result = -1
		;
		ILine = error(_),
		Result = -1
	).

:- func nextNavigation(navigation, interfaceData) = navigation.

nextNavigation(nav(Current), label(String)) = nav([String | Current]).

:- func getMenuItem(list(menuItem(D)), int) = menuItem(D).
:- mode getMenuItem(in(list_skel(menuItem)), in) = out(menuItem).
%:- mode getMenuItem(in(list_skel(menuItem(D))), in) = out(menuItem(D)).

getMenuItem(ListMenuItem, Index) = Result :-
	ListMenuItem = [MenuItem | Rest],
	(if
		Index = 1
	then
		Result = MenuItem
	else
		Result = getMenuItem(Rest, Index - 1)
	)
	;
	ListMenuItem = [],
	throw("Never reached")
	.

:- end_module ui_console.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
