/**
 * An User Interface implemented in the Java SWING

 * @author Pedro Mariano
 * @version 1.0 2013/08/10
 */
:- module ui_swing.

:- interface.

:- import_module userInterface.
:- import_module io.

:- pred show(string, userInterface(D), D, D, io.state, io.state).
:- mode show(in, in(userInterface), di, uo, di, uo) is det.
:- mode show(in, in(userInterface), in, out, di, uo) is det.
%:- mode show(in(userInterfaceSkel), di, uo, di, uo) is det.

:- pred debug(io.state, io.state).
:- mode debug(di, uo) is det.

:- implementation.

:- import_module bool, int, list, maybe.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

:- type frame(D).

:- pragma foreign_type("Java", frame(D), "ui.swing.UIFrame").

:- type ddpanel(D).

:- pragma foreign_type("Java", ddpanel(D), "ui.swing.DynamicDataPanel<? extends ui.swing.DynamicDataPanel>").

:- type soopanel(D).

:- pragma foreign_type("Java", soopanel(D), "ui.swing.SelectOneOfPanel").

:- type label.

:- pragma foreign_type("Java", label, "javax.swing.JLabel").

:- type button.

:- pragma foreign_type("Java", button, "javax.swing.JButton").

:- type checkbox.

:- pragma foreign_type("Java", checkbox, "javax.swing.JCheckBox").

:- type radioButton.

:- pragma foreign_type("Java", radioButton, "javax.swing.JRadioButton").

%:- mode frame_di == in(ground).
%:- mode frame_ui == in(ground).
%:- mode frame_uo == out(ground).

:- mode frame_di == di.
:- mode frame_ui == ui.
:- mode frame_uo == uo.

:- type swingInterface(D) --->
	swingInterface(
		frame  :: frame(D),
		panels :: list(swingPanel(D))
	).

:- type swingPanel(D) --->
	swingPanel(
		panel      :: ddpanel(D),
		components :: list(swingComponent(D))
	) ;
	some [F]
	swingPanel(
		getFunc         :: get(D, F),
		fieldPanel      :: ddpanel(F),
		fieldComponents :: list(swingComponent(F))
	).

:- type swingComponent(D) --->
	nop.

:- type maos --->
	maos(esq :: int, dir :: string).

:- type debug --->
	debug(moes :: list(maos)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

show(Title, UI, !Data, !IO) :-
	InitFrame = initFrame(Title, !.Data),
	(
		UI = m(ListMenuItems),
		list.foldl(buildToolBarFrame, ListMenuItems, InitFrame, ShowFrame)
		;
		UI = d(Dialog),
		(
			Dialog = dialog(ListDialogItems),
			initPanel(InitPanel, InitFrame, Tmp1Frame),
			list.foldl2(trace_buildDialogForm(no), ListDialogItems, Tmp1Frame, Tmp2Frame, InitPanel, ShowPanel),
			handle_edit(createButton(label("Application")), ShowPanel, Tmp2Frame, ShowFrame)
			;
			Dialog = dialog(Get, Set, ListDialogItems),
			initPanel(InitPanel, InitFrame, Tmp1Frame),
			list.foldl2(trace_buildDialogForm(no), ListDialogItems, Tmp1Frame, Tmp2Frame, InitPanel, ShowPanel),
			handle_edit(createButton(label("Application")), Get, Set, ShowPanel, Tmp2Frame, ShowFrame)
		)
	),
	io.print("Showing frame...\n", !IO),
	showFrame(ShowFrame, !:Data, !IO),
	io.print("Frame shown.\n", !IO).

debug(!IO) :-
	true.
	% Dados = debug([maos(1, "um"), maos(2, "dois")]),
	% InitFrame = initFrame("Debug App", Dados),

	% initPanel(InitPanel, !Frame),
	% list.foldl2(buildDialogForm, ListDialogItems, !Frame, InitPanel, ShowPanel),
	% handle_edit(createButton(InterfaceData), ShowPanel, !Frame),
	
	% initCellRenderer(Panel, getMoes, setMao, InitFrame, ShowFrame),
	% showFrame(ShowFrame, NextDados, !IO),
	% io.print(NextDados, !IO).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions


:- func getMoes(debug) = list(maos).
getMoes(D) = D^moes.

:- func setMao(debug, maos, int) = maybe_error(debug).
setMao(Debug, Maos, Index) = Result :-
	NextList = list.det_replace_nth(Debug^moes, Index, Maos),
	(if
		list.member(maos(3, "três"), NextList)
	then
		Result = error("Não pode haver duas sem três")
	else
		Result = ok('moes :='(Debug, NextList))
	).


:- func initFrame(string, D) = frame(D).
:- mode initFrame(in, in) = frame_uo is det.
:- mode initFrame(in, ui) = frame_uo is det.

:- pragma foreign_proc(
	"Java",
	initFrame(Title::in, Data::in) = (Result::frame_uo),
	[will_not_call_mercury, promise_pure],
	"
	Result = new ui.swing.UIFrame (Title, Data);
	"
	).

:- pragma foreign_proc(
	"Java",
	initFrame(Title::in, Data::ui) = (Result::frame_uo),
	[will_not_call_mercury, promise_pure],
	"
	Result = new ui.swing.UIFrame (Title, Data);
	"
	).

:- pred initPanel(ddpanel(F), frame(D), frame(D)).
:- mode initPanel(uo, frame_di, frame_uo) is det.

:- pragma foreign_proc(
	"Java",
	initPanel(Panel::uo, FrameDI::frame_di, FrameUO::frame_uo),
	[will_not_call_mercury, promise_pure],
	"
	Panel = FrameDI.newUIPanel ();
	FrameUO = FrameDI;
	"
	).

/**
 * initCellRenderer(Renderer, !Frame).

 * Unify {@code Renderer} with a new {@code
 * ui.swing.FieldListCellRendererPanel} instance that is used by the swing
 * interface to render field list elements.
  
 */
:- pred initCellRenderer(ddpanel(E), ddpanel(D)).
:- mode initCellRenderer(uo, ui) is det.

:- pragma foreign_proc(
	"Java",
	initCellRenderer(
		Result::uo,
		Panel::ui
	),
	[will_not_call_mercury, promise_pure],
	"
	Result = Panel.newFieldListCellRendererPanel ();
	"
	).


/**
 * initCellEditor(Editor, SetFieldListElement, !Frame).

 * Unify {@code Editor} with a new {@code
 * ui.swing.FieldListCellEditorPanel} instance that is used by the swing
 * interface to edit field list elements.  Parameter {@code
 * SetFieldListElement} is used by the swing interface to replace the
 * element being edited in the field list.  The function returns a {@code
 * maybe_error(D)} value to indicate if the replace is successful or not.
  
 */
:- pred initCellEditor(ddpanel(E), func(F, E, int) = maybe_error(F), ddpanel(F)).
:- mode initCellEditor(uo, in, ui) is det.

:- pragma foreign_proc(
	"Java",
	initCellEditor(
		Result::uo,
		SetFieldListElement::in,
		Panel::ui
	),
	[will_not_call_mercury, promise_pure],
	"
	Result = Panel.newFieldListCellEditorPanel (SetFieldListElement);
	"
	).

:- pred initSelectOneOfPanel(soopanel(D), string, func(D) = maybe(int), ddpanel(D), ddpanel(D)).
:- mode initSelectOneOfPanel(uo, in, in, di, uo) is det.

:- pragma foreign_proc(
	"Java",
	initSelectOneOfPanel(
		Result::uo,
		PanelName::in,
		FuncSelectedIndex::in,
		PanelDI::di,
		PanelUO::uo),
	[will_not_call_mercury, promise_pure],
	"
	Result = PanelDI.newSelectOneOfPanel (PanelName, FuncSelectedIndex);
	PanelUO = PanelDI;
	"
	).

:- pred initSelectOneOfPanel(soopanel(F), string, get(D, F), set(D, F), func(F) = maybe(int), ddpanel(D), ddpanel(D)).
:- mode initSelectOneOfPanel(uo, in, in, in, in, di, uo) is det.

:- pragma foreign_proc(
	"Java",
	initSelectOneOfPanel(
		Result::uo,
		PanelName::in,
		GetFunc::in,
		SetFunc::in,
		FuncSelectedIndex::in,
		PanelDI::di,
		PanelUO::uo),
	[will_not_call_mercury, promise_pure],
	"
	Result = PanelDI.newSelectOneOfPanel (PanelName, GetFunc, SetFunc, FuncSelectedIndex);
	PanelUO = PanelDI;
	"
	).

:- pred initInlinePanelForChoice(ddpanel(D), soopanel(D), soopanel(D)).
:- mode initInlinePanelForChoice(uo, di, uo) is det.

:- pragma foreign_proc(
	"Java",
	initInlinePanelForChoice(
		Result::uo,
		PanelDI::di, PanelUO::uo),
	[will_not_call_mercury, promise_pure],
	"
	Result = PanelDI.newInlinePanelForChoice ();
	PanelUO = PanelDI;
	"
	).
:- pred initInlinePanelForData(ddpanel(D), string, ddpanel(D), ddpanel(D)).
:- mode initInlinePanelForData(uo, in, di, uo) is det.

:- pragma foreign_proc(
	"Java",
	initInlinePanelForData(
		Result::uo,
		PanelName::in,
		PanelDI::di, PanelUO::uo),
	[will_not_call_mercury, promise_pure],
	"
	Result = PanelDI.newInlinePanelForData (PanelName);
	PanelUO = PanelDI;
	"
	).

:- pred initInlinePanelForFieldData(ddpanel(F), get(D, F), set(D, F), string, ddpanel(D), ddpanel(D)).
:- mode initInlinePanelForFieldData(uo, in, in, in, di, uo) is det.

:- pragma foreign_proc(
	"Java",
	initInlinePanelForFieldData(
		Result::uo,
		GetFunc::in,
		SetFunc::in,
		PanelName::in,
		PanelDI::di, PanelUO::uo),
	[will_not_call_mercury, promise_pure],
	"
	Result = PanelDI.newInlinePanelForFieldData (PanelName, GetFunc, SetFunc);
	PanelUO = PanelDI;
	"
	).

/**
 * Create the frame tool bar using the menu items of some {@code menu} value.
 */
:- pred buildToolBarFrame(userInterface.menuItem(D), frame(D), frame(D)).
:- mode buildToolBarFrame(in(userInterface.menuItem), frame_di, frame_uo) is det.

buildToolBarFrame(mi(InterfaceData, Action), !Frame) :-
	Action = submenu(ListMenuItems),
	handle_submenu_begin(createLabel(InterfaceData), !Frame),
	list.foldl(buildToolBarFrame, ListMenuItems, !Frame),
	handle_submenu_end(!Frame)
	;
	Action = updateData(Func),
	handle_updateData_frame(createButton(InterfaceData), Func, !Frame)
	;
	Action = updateDataIO(Pred),
	handle_updateDataIO(createButton(InterfaceData), Pred, !Frame)
	;
	Action = actionDataIO(Pred),
	handle_actionDataIO(createButton(InterfaceData), Pred, !Frame)
	;
	Action = actionIO(Pred),
	handle_actionIO(createButton(InterfaceData), Pred, !Frame)
	;
	Action = edit(Dialog),
	(
		Dialog = dialog(ListDialogItems),
		initPanel(InitPanel, !Frame),
		list.foldl2(trace_buildDialogForm(no), ListDialogItems, !Frame, InitPanel, ShowPanel),
		handle_edit(createButton(InterfaceData), ShowPanel, !Frame)
		;
		Dialog = dialog(Get, Set, ListDialogItems),
		initPanel(InitPanel, !Frame),
		list.foldl2(trace_buildDialogForm(no), ListDialogItems, !Frame, InitPanel, ShowPanel),
		handle_edit(createButton(InterfaceData), Get, Set, ShowPanel, !Frame)
	)
	.


:- pred handle_submenu_begin(label, frame(D), frame(D)).
:- mode handle_submenu_begin(in, frame_di, frame_uo) is det.

:- pragma foreign_proc(
	"Java",
	handle_submenu_begin(Label::in, FrameDI::frame_di, FrameUO::frame_uo),
	[will_not_call_mercury, promise_pure],
	"
	FrameUO = FrameDI.handle_submenu_begin (Label);
	"
	).

:- pred handle_submenu_end(frame(D), frame(D)).
:- mode handle_submenu_end(frame_di, frame_uo) is det.

:- pragma foreign_proc(
	"Java",
	handle_submenu_end(FrameDI::frame_di, FrameUO::frame_uo),
	[will_not_call_mercury, promise_pure],
	"
	FrameUO = FrameDI.handle_submenu_end ();
	"
	).

:- pred handle_updateData_frame(button, func(D) = D, frame(D), frame(D)).
:- mode handle_updateData_frame(in, in, frame_di, frame_uo) is det.

:- pragma foreign_proc(
	"Java",
	handle_updateData_frame(Button::in, SetFunc::in, FrameDI::frame_di, FrameUO::frame_uo),
	[will_not_call_mercury, promise_pure],
	"
	FrameUO = FrameDI.handle_updateData (Button, SetFunc);
	"
	).

:- pred handle_updateDataIO(button, pred(D, D, io.state, io.state), frame(D), frame(D)).
:- mode handle_updateDataIO(in, in(pred(in, out, di, uo) is det), frame_di, frame_uo) is det.

:- pragma foreign_proc(
	"Java",
	handle_updateDataIO(Button::in, PredIO::in(pred(in, out, di, uo) is det), FrameDI::frame_di, FrameUO::frame_uo),
	[will_not_call_mercury, promise_pure],
	"
	FrameUO = FrameDI.handle_updateDataIO (Button, PredIO);
	"
	).

:- pred handle_actionDataIO(button, pred(D, io.state, io.state), frame(D), frame(D)).
:- mode handle_actionDataIO(in, in(pred(in, di, uo) is det), frame_di, frame_uo) is det.

:- pragma foreign_proc(
	"Java",
	handle_actionDataIO(Button::in, PredIO::in(pred(in, di, uo) is det), FrameDI::frame_di, FrameUO::frame_uo),
	[will_not_call_mercury, promise_pure],
	"
	FrameUO = FrameDI.handle_actionDataIO (Button, PredIO);
	"
	).

:- pred handle_actionIO(button, pred(io.state, io.state), frame(D), frame(D)).
:- mode handle_actionIO(in, in(pred(di, uo) is det), frame_di, frame_uo) is det.

:- pragma foreign_proc(
	"Java",
	handle_actionIO(Button::in, PredIO::in(pred(di, uo) is det), FrameDI::frame_di, FrameUO::frame_uo),
	[will_not_call_mercury, promise_pure],
	"
	FrameUO = FrameDI.handle_actionIO (Button, PredIO);
	"
	).

:- pred handle_edit(button, ddpanel(D), frame(D), frame(D)).
:- mode handle_edit(in, ui, frame_di, frame_uo) is det.

:- pragma foreign_proc(
	"Java",
	handle_edit(Button::in, Panel::ui, FrameDI::frame_di, FrameUO::frame_uo),
	[will_not_call_mercury, promise_pure],
	"
	FrameUO = FrameDI.handle_edit (Button, (ui.swing.UIPanel) Panel);
	"
	).

:- pred handle_edit(button, get(D, F), set(D, F), ddpanel(F), frame(D), frame(D)).
:- mode handle_edit(in, in, in, ui, frame_di, frame_uo) is det.

:- pragma foreign_proc(
	"Java",
	handle_edit(Button::in, GetFunc::in, SetFunc::in, Panel::ui, FrameDI::frame_di, FrameUO::frame_uo),
	[will_not_call_mercury, promise_pure],
	"
	FrameUO = FrameDI.handle_edit (Button, GetFunc, SetFunc, (ui.swing.UIPanel) Panel);
	"
	).

:- pred handle_editField_frame(button, get(D, F), set(D, F), ddpanel(F), frame(D), frame(D)).
:- mode handle_editField_frame(in, in, in, ui, di, uo) is det.

:- pragma foreign_proc(
	"Java",
	handle_editField_frame(Button::in, GetFunc::in, SetFunc::in, Panel::ui, FrameDI::di, FrameUO::uo),
	[will_not_call_mercury, promise_pure],
	"
	FrameUO = FrameDI.handle_editField (Button, GetFunc, SetFunc, (ui.swing.UIPanel) Panel);
	"
	).


:- pred trace_buildDialogForm(bool, userInterface.dialogItem(F), frame(D), frame(D), ddpanel(F), ddpanel(F)).
:- mode trace_buildDialogForm(in, in, di, uo, di, uo) is det.

trace_buildDialogForm(Flat, DialogItem, !Frame, !Panel) :-
	(
		DialogItem = di(InterfaceData, _)
		;
		DialogItem = di(InterfaceData)
	),
	trace [io(!IO)] (io.print(InterfaceData, !IO), io.nl(!IO)),
	buildDialogForm(Flat, DialogItem, !Frame, !Panel).

/**
 * buildDialogForm(Flat, DialogItem, !Frame, !Panel)
  
 * Create a dialog form using the dialog items of some {@code dialog} value.

 * Parameter {@code DialogItem} can be an instance of {@code
 * di(InterfaceData, Action)} meaning an appropriate swing component is
 * added to the panel.

 * <p> Flag {@code Flat} controls the creation of sub panels or not.  If
 * {@code yes} all components are added in the same {@code panel(P)}, but they are
 * grouped in swing panels.
  
 */
:- pred buildDialogForm(bool, userInterface.dialogItem(F), frame(D), frame(D), ddpanel(F), ddpanel(F)).
:- mode buildDialogForm(in, in, di, uo, di, uo) is det.

buildDialogForm(_Flat, di(InterfaceData), !Frame, !Panel) :-
	handle_noaction(createLabel(InterfaceData), !Panel).

buildDialogForm(Flat, di(InterfaceData, Action), !Frame, !Panel) :-
	Action = subdialog(ListDialogItems),
	(
		Flat = yes,
		initInlinePanelForData(InlinePanel, toString(InterfaceData), !Panel),
		list.foldl2(trace_buildDialogForm(yes), ListDialogItems, !Frame, InlinePanel, _)
		;
		Flat = no,
		initPanel(InitPanel, !Frame),
		list.foldl2(trace_buildDialogForm(no), ListDialogItems, !Frame, InitPanel, ShowPanel),
		handle_subdialog(createButton(InterfaceData), ShowPanel, !Panel)
	)
	;
	Action = newValue(Value),
	handle_newValue(createButton(InterfaceData), Value, !Panel)
	;
	Action = updateData(SetFunc),
	handle_updateData_panel(createButton(InterfaceData), SetFunc, !Panel)
	;
	Action = editField(GetFunc, SetFunc, ListDialogItems),
	(
		Flat = yes,
		initInlinePanelForFieldData(InlinePanel, GetFunc, SetFunc, toString(InterfaceData), !Panel),
		list.foldl2(trace_buildDialogForm(yes), ListDialogItems, !Frame, InlinePanel, _)
		% handle_panelBegin(GetFunc, toString(InterfaceData), !Panel),
		% list.foldl2(buildDialogForm(yes), ListDialogItems, !Frame, !Panel),
		% handle_panelEnd(GetFunc, !Panel)
		;
		Flat = no,
		initPanel(InitPanel, !Frame),
		list.foldl2(trace_buildDialogForm(no), ListDialogItems, !Frame, InitPanel, ShowPanel),
		handle_editField_panel(createButton(InterfaceData), GetFunc, SetFunc, ShowPanel, !Panel)
	)
	;
	Action = updateFieldInt(GetFunc, SetFunc),
	handle_updateFieldInt(createLabel(InterfaceData), GetFunc, SetFunc, !Panel)
	;
	Action = updateFieldString(GetFunc, SetFunc),
	handle_updateFieldString(createLabel(InterfaceData), GetFunc, SetFunc, !Panel)
	;
	Action = updateFieldFloat(GetFunc, SetFunc),
	handle_updateFieldFloat(createLabel(InterfaceData), GetFunc, SetFunc, !Panel)
	;
	Action = updateFieldBool(GetFunc, SetFunc),
	handle_updateFieldBool(createCheckbox(InterfaceData), GetFunc, SetFunc, !Panel)
	;
	Action = editListFieldAny(GetFunc, SetFunc, DefaultValue, SubDialog),
	(
		Flat = yes,
		initPanel(InitPanel, !Frame),
		ListDialogItems = [di(InterfaceData, Action)],
		list.foldl2(trace_buildDialogForm(no), ListDialogItems, !Frame, InitPanel, ShowPanel),
		handle_subdialog(createButton(InterfaceData), ShowPanel, !Panel)
		;
		Flat = no,
		SetFieldListElement = setFieldListElement(GetFunc, SetFunc),
		initCellRenderer(InitCellRenderer, !.Panel),
		list.foldl2(trace_buildDialogForm(yes), SubDialog, !Frame, InitCellRenderer, ShowCellRenderer),
		%list.foldl2(buildDialogForm(no), SubDialog, !Frame, InitCellRenderer, ShowCellRenderer),
		initCellEditor(InitCellEditor, SetFieldListElement, !.Panel),
		list.foldl2(trace_buildDialogForm(yes), SubDialog, !Frame, InitCellEditor, ShowCellEditor),
		%list.foldl2(buildDialogForm(no), SubDialog, !Frame, InitCellEditor, ShowCellEditor),
		handle_editListFieldAny(
			toString(InterfaceData),
			GetFunc, SetFunc,
			list.length, list.det_index0,
			ShowCellRenderer, ShowCellEditor, DefaultValue, !Panel
		)
	)
	;
	Action = updateListFieldInt(GetFunc, SetFunc),
	handle_updateListFieldInt(toString(InterfaceData), GetFunc, SetFunc, !Panel)
	;
	Action = updateListFieldString(GetFunc, SetFunc),
	handle_updateListFieldString(toString(InterfaceData), GetFunc, SetFunc, !Panel)
	;
	Action = updateListFieldFloat(GetFunc, SetFunc),
	handle_updateListFieldFloat(toString(InterfaceData), GetFunc, SetFunc, !Panel)
	;
	Action = selectOneOf(options(GetChoice, ListChoiceItems)),
	initSelectOneOfPanel(InitSelectOneOfPanel, toString(InterfaceData), GetChoice, !Panel),
	list.foldl2(buildSelectOneOfPanel, ListChoiceItems, InitSelectOneOfPanel, _ShowSelectOneOfPanel, !Frame)
%	handle_selectOneOf(ShowSelectOneOfPanel, !Panel)
	;
	Action = selectOneOf(GetFunc, SetFunc, options(GetChoice, ListChoiceItems)),
	initSelectOneOfPanel(InitSelectOneOfPanel, toString(InterfaceData), GetFunc, SetFunc, GetChoice, !Panel),
	list.foldl2(buildSelectOneOfPanel(GetFunc), ListChoiceItems, InitSelectOneOfPanel, _ShowSelectOneOfPanel, !Frame)
%	handle_selectOneOf(ShowSelectOneOfPanel, !Panel)
	.

% :- pred handle_panelBegin(string, ddpanel(D), ddpanel(D)).
% :- mode handle_panelBegin(in, di, uo) is det.

% :- pragma foreign_proc(
% 	"Java",
% 	handle_panelBegin(Title::in, PanelDI::di, PanelUO::uo),
% 	[will_not_call_mercury, promise_pure],
% 	"
% 	PanelUO = PanelDI.panelBegin (Title);
% 	"
% 	).

% :- pred handle_panelEnd(ddpanel(D), ddpanel(D)).
% :- mode handle_panelEnd(di, uo) is det.

% :- pragma foreign_proc(
% 	"Java",
% 	handle_panelEnd(PanelDI::di, PanelUO::uo),
% 	[will_not_call_mercury, promise_pure],
% 	"
% 	PanelUO = PanelDI.panelEnd ();
% 	"
% 	).


% :- pred handle_panelBegin(get(D, F), string, ddpanel(D), ddpanel(F)).
% :- mode handle_panelBegin(in, in, di, uo) is det.

% :- pragma foreign_proc(
% 	"Java",
% 	handle_panelBegin(_Get::in, Title::in, PanelDI::di, PanelUO::uo),
% 	[will_not_call_mercury, promise_pure],
% 	"
% 	PanelUO = PanelDI.panelBegin (Title);
% 	"
% 	).

% :- pred handle_panelEnd(get(D, F), ddpanel(F), ddpanel(D)).
% :- mode handle_panelEnd(in, di, uo) is det.

% :- pragma foreign_proc(
% 	"Java",
% 	handle_panelEnd(_Get::in, PanelDI::di, PanelUO::uo),
% 	[will_not_call_mercury, promise_pure],
% 	"
% 	PanelUO = PanelDI.panelEnd ();
% 	"
% 	).



:- pred handle_noaction(label, ddpanel(D), ddpanel(D)).
:- mode handle_noaction(in, di, uo) is det.

:- pragma foreign_proc(
	"Java",
	handle_noaction(Label::in, PanelDI::di, PanelUO::uo),
	[will_not_call_mercury, promise_pure],
	"
	PanelUO = PanelDI.handle_noaction (Label);
	"
	).

:- pred handle_subdialog(button, ddpanel(D), ddpanel(D), ddpanel(D)).
:- mode handle_subdialog(in, ui, di, uo) is det.

:- pragma foreign_proc(
	"Java",
	handle_subdialog(Button::in, NewPanel::ui, PanelDI::di, PanelUO::uo),
	[will_not_call_mercury, promise_pure],
	"
	PanelUO = PanelDI.handle_subdialog (Button, (ui.swing.UIPanel) NewPanel);
	"
	).

:- pred handle_newValue(button, D, ddpanel(D), ddpanel(D)).
:- mode handle_newValue(in, in, di, uo) is det.

:- pragma foreign_proc(
	"Java",
	handle_newValue(Button::in, NewValue::in, PanelDI::di, PanelUO::uo),
	[will_not_call_mercury, promise_pure],
	"
	PanelUO = PanelDI.handle_newValue (Button, NewValue);
	"
	).

:- pred handle_updateData_panel(button, func(D) = D, ddpanel(D), ddpanel(D)).
:- mode handle_updateData_panel(in, in, di, uo) is det.

:- pragma foreign_proc(
	"Java",
	handle_updateData_panel(Button::in, SetFunc::in, PanelDI::di, PanelUO::uo),
	[will_not_call_mercury, promise_pure],
	"
	PanelUO = PanelDI.handle_updateData (Button, SetFunc);
	"
	).

:- pred handle_editField_panel(button, get(D, F), set(D, F), ddpanel(F), ddpanel(D), ddpanel(D)).
:- mode handle_editField_panel(in, in, in, ui, di, uo) is det.

:- pragma foreign_proc(
	"Java",
	handle_editField_panel(Button::in, GetFunc::in, SetFunc::in, NewPanel::ui, PanelDI::di, PanelUO::uo),
	[will_not_call_mercury, promise_pure],
	"
	PanelUO = PanelDI.handle_editField (Button, GetFunc, SetFunc, (ui.swing.UIPanel) NewPanel);
	"
	).


:- pred handle_updateFieldInt(label, get(D, int), set(D, int), ddpanel(D), ddpanel(D)).
:- mode handle_updateFieldInt(in, in, in, di, uo) is det.

:- pragma foreign_proc(
	"Java",
	handle_updateFieldInt(Label::in, GetFunc::in, SetFunc::in, PanelDI::di, PanelUO::uo),
	[will_not_call_mercury, promise_pure],
	"
	PanelUO = PanelDI.handle_updateFieldInt (Label, GetFunc, SetFunc);
	"
	).

:- pred handle_updateFieldString(label, get(D, string), set(D, string), ddpanel(D), ddpanel(D)).
:- mode handle_updateFieldString(in, in, in, di, uo) is det.

:- pragma foreign_proc(
	"Java",
	handle_updateFieldString(Label::in, GetFunc::in, SetFunc::in, PanelDI::di, PanelUO::uo),
	[will_not_call_mercury, promise_pure],
	"
	PanelUO = PanelDI.handle_updateFieldString (Label, GetFunc, SetFunc);
	"
	).

:- pred handle_updateFieldFloat(label, get(D, float), set(D, float), ddpanel(D), ddpanel(D)).
:- mode handle_updateFieldFloat(in, in, in, di, uo) is det.

:- pragma foreign_proc(
	"Java",
	handle_updateFieldFloat(Label::in, GetFunc::in, SetFunc::in, PanelDI::di, PanelUO::uo),
	[will_not_call_mercury, promise_pure],
	"
	PanelUO = PanelDI.handle_updateFieldFloat (Label, GetFunc, SetFunc);
	"
	).

:- pred handle_updateFieldBool(checkbox, get(D, bool), set(D, bool), ddpanel(D), ddpanel(D)).
:- mode handle_updateFieldBool(in, in, in, di, uo) is det.

:- pragma foreign_proc(
	"Java",
	handle_updateFieldBool(CheckBox::in, GetFunc::in, SetFunc::in, PanelDI::di, PanelUO::uo),
	[will_not_call_mercury, promise_pure],
	"
	PanelUO = PanelDI.handle_updateFieldBool (CheckBox, GetFunc, SetFunc);
	"
	).


:- pred handle_editListFieldAny(
	string, get(D, list(F)), set(D, list(F)),
	func(list(F)) = int, func(list(F), int) = F,
	ddpanel(F), ddpanel(F), F,
	ddpanel(D), ddpanel(D)).
:- mode handle_editListFieldAny(in, in,  in, in,  in, in,  in,  in, di, uo) is det.

:- pragma foreign_proc(
	"Java",
	handle_editListFieldAny(
		FieldName::in,
		GetFunc::in, SetFunc::in,
		ListSizeFunc::in, ListElementFunc::in,
		ShowCellRenderer::in, ShowCellEditor::in, DefaultValue::in,
		PanelDI::di, PanelUO::uo),
	[will_not_call_mercury, promise_pure],
	"
	PanelUO = PanelDI.handle_editListFieldAny (
		FieldName,
		GetFunc, SetFunc, ListSizeFunc, ListElementFunc,
		(ui.swing.FieldListCellRendererPanel) ShowCellRenderer,
		(ui.swing.FieldListCellEditorPanel) ShowCellEditor, DefaultValue);
	"
	).

:- pred handle_updateListFieldInt(string, get(D, list(int)), set(D, list(int)), ddpanel(D), ddpanel(D)).
:- mode handle_updateListFieldInt(in, in, in, di, uo) is det.

:- pragma foreign_proc(
	"Java",
	handle_updateListFieldInt(FieldName::in, GetFunc::in, SetFunc::in, PanelDI::di, PanelUO::uo),
	[will_not_call_mercury, promise_pure],
	"
	PanelUO = PanelDI.handle_updateListFieldInt (FieldName, GetFunc, SetFunc);
	"
	).

:- pred handle_updateListFieldString(string, get(D, list(string)), set(D, list(string)), ddpanel(D), ddpanel(D)).
:- mode handle_updateListFieldString(in, in, in, di, uo) is det.

:- pragma foreign_proc(
	"Java",
	handle_updateListFieldString(FieldName::in, GetFunc::in, SetFunc::in, PanelDI::di, PanelUO::uo),
	[will_not_call_mercury, promise_pure],
	"
	PanelUO = PanelDI.handle_updateListFieldString (FieldName, GetFunc, SetFunc);
	"
	).

:- pred handle_updateListFieldFloat(string, get(D, list(float)), set(D, list(float)), ddpanel(D), ddpanel(D)).
:- mode handle_updateListFieldFloat(in, in, in, di, uo) is det.

:- pragma foreign_proc(
	"Java",
	handle_updateListFieldFloat(FieldName::in, GetFunc::in, SetFunc::in, PanelDI::di, PanelUO::uo),
	[will_not_call_mercury, promise_pure],
	"
	PanelUO = PanelDI.handle_updateListFieldFloat (FieldName, GetFunc, SetFunc);
	"
	).


%%
%%
%%


:- pred buildSelectOneOfPanel(choiceItem(F), soopanel(F), soopanel(F), frame(D), frame(D)).
:- mode buildSelectOneOfPanel(in, di, uo, di, uo) is det.

buildSelectOneOfPanel(ci(InterfaceData, ListDialogItems), !SOOPanel, !Frame) :-
	ListDialogItems = [],
	handle_choiceItem_noValue(createRadioButton(InterfaceData), !SOOPanel)
	;
	ListDialogItems = [_|_],
	initInlinePanelForChoice(InlinePanel, !SOOPanel),
	list.foldl2(trace_buildDialogForm(no), ListDialogItems, !Frame, InlinePanel, ShowPanel),
	handle_choiceItem_noValue(createRadioButton(InterfaceData), ShowPanel, !SOOPanel)
	.

buildSelectOneOfPanel(ci(InterfaceData, Value, ListDialogItems), !SOOPanel, !Frame) :-
	ListDialogItems = [],
	handle_choiceItem_withValue(createRadioButton(InterfaceData), Value, !SOOPanel)
	;
	ListDialogItems = [_|_],
	initInlinePanelForChoice(InlinePanel, !SOOPanel),
	list.foldl2(trace_buildDialogForm(no), ListDialogItems, !Frame, InlinePanel, ShowPanel),
	handle_choiceItem_withValue(createRadioButton(InterfaceData), Value, ShowPanel, !SOOPanel)
	.

:- pred buildSelectOneOfPanel(get(D1, F), choiceItem(F), soopanel(F), soopanel(F), frame(D2), frame(D2)).
:- mode buildSelectOneOfPanel(in, in, di, uo, di, uo) is det.

buildSelectOneOfPanel(_GetFunc, ci(InterfaceData, ListDialogItems), !SOOPanel, !Frame) :-
	ListDialogItems = [],
	handle_choiceItem_noValue(createRadioButton(InterfaceData), !SOOPanel)
	;
	ListDialogItems = [_|_],
	initInlinePanelForChoice(InlinePanel, !SOOPanel),
	list.foldl2(trace_buildDialogForm(no), ListDialogItems, !Frame, InlinePanel, ShowPanel),
	handle_choiceItem_noValue(createRadioButton(InterfaceData), ShowPanel, !SOOPanel)
	.

buildSelectOneOfPanel(_GetFunc, ci(InterfaceData, Value, ListDialogItems), !SOOPanel, !Frame) :-
	ListDialogItems = [],
	handle_choiceItem_withValue(createRadioButton(InterfaceData), Value, !SOOPanel)
	;
	ListDialogItems = [_|_],
	initInlinePanelForChoice(InlinePanel, !SOOPanel),
	list.foldl2(trace_buildDialogForm(no), ListDialogItems, !Frame, InlinePanel, ShowPanel),
	handle_choiceItem_withValue(createRadioButton(InterfaceData), Value, ShowPanel, !SOOPanel)
	.



:- pred handle_choiceItem_noValue(radioButton, soopanel(D), soopanel(D)).
:- mode handle_choiceItem_noValue(in, di, uo) is det.

:- pragma foreign_proc(
	"Java",
	handle_choiceItem_noValue(RadioButton::in, PanelDI::di, PanelUO::uo),
	[will_not_call_mercury, promise_pure],
	"
	PanelUO = PanelDI.handle_choiceItem (RadioButton, null, null);
	"
	).

:- pred handle_choiceItem_noValue(radioButton, ddpanel(D), soopanel(D), soopanel(D)).
:- mode handle_choiceItem_noValue(in, in, di, uo) is det.

:- pragma foreign_proc(
	"Java",
	handle_choiceItem_noValue(RadioButton::in, ValuePanel::in, PanelDI::di, PanelUO::uo),
	[will_not_call_mercury, promise_pure],
	"
	PanelUO = PanelDI.handle_choiceItem (RadioButton, null, (ui.swing.InlinePanelField) ValuePanel);
	"
	).

:- pred handle_choiceItem_withValue(radioButton, D, soopanel(D), soopanel(D)).
:- mode handle_choiceItem_withValue(in, in, di, uo) is det.

:- pragma foreign_proc(
	"Java",
	handle_choiceItem_withValue(RadioButton::in, Value::in, PanelDI::di, PanelUO::uo),
	[will_not_call_mercury, promise_pure],
	"
	PanelUO = PanelDI.handle_choiceItem (RadioButton, Value, null);
	"
	).

:- pred handle_choiceItem_withValue(radioButton, D, ddpanel(D), soopanel(D), soopanel(D)).
:- mode handle_choiceItem_withValue(in, in, in, di, uo) is det.

:- pragma foreign_proc(
	"Java",
	handle_choiceItem_withValue(RadioButton::in, AValue::in, ValuePanel::in, PanelDI::di, PanelUO::uo),
	[will_not_call_mercury, promise_pure],
	"
	PanelUO = PanelDI.handle_choiceItem (RadioButton, AValue, (ui.swing.InlinePanelField) ValuePanel);
	"
	).


%%
%%
%%


:- pred showFrame(frame(D), D, io.state, io.state).
:- mode showFrame(frame_ui, uo, di, uo) is det.

:- pragma foreign_proc(
	"Java",
	showFrame(SwingForm::frame_ui, Data::uo, IOdi::di, IOuo::uo),
	[may_call_mercury , promise_pure],
	"
	Data = SwingForm.showFrame ();
	IOuo = IOdi;
	"
	).

%%
%%
%%

:- func setFieldListElement(get(D, list(F)), set(D, list(F)), D, F, int) = maybe_error(D).

setFieldListElement(Get, Set, Data, Element, Index) = Result :-
	Get(Data) = PrevList,
	NextList = list.det_replace_nth(PrevList, Index, Element),
	Result = Set(Data, NextList),
	trace [io(!IO)]
	(
		io.print("Replacing ", !IO),
		io.print(Index, !IO),
		io.print("th element of ", !IO),
		io.print(NextList, !IO),
		io.print(" with ", !IO),
		io.print(Element, !IO),
		io.print(" and result is ", !IO),
		io.print(Result, !IO),
		io.nl(!IO)
	).
	
/**
 * Create a button given an interface data.
 */
:- func createButton(userInterface.interfaceData) = button.
:- mode createButton(in) = uo is det.

:- pragma foreign_proc(
	"Java",
	createButton(UI::in) = (Result::uo),
	[will_not_call_mercury, promise_pure],
	"
	Result = new javax.swing.JButton (UI.F1);
	"
	).

/**
 * Create a label given an interface data.
 */
:- func createLabel(userInterface.interfaceData) = label.

:- pragma foreign_proc(
	"Java",
	createLabel(UI::in) = (Result::out),
	[will_not_call_mercury, promise_pure],
	"
	Result = new javax.swing.JLabel (UI.F1);
	"
	).

/**
 * Create a Swing check box from the given interface data.
 */

:- func createCheckbox(userInterface.interfaceData) = checkbox.

:- pragma foreign_proc(
	"Java",
	createCheckbox(UI::in) = (Result::out),
	[will_not_call_mercury, promise_pure],
	"
	Result = new javax.swing.JCheckBox (UI.F1);
	"
	).

/**
 * Create a Swing radio button from the given interface data.
 */

:- func createRadioButton(userInterface.interfaceData) = radioButton.

:- pragma foreign_proc(
	"Java",
	createRadioButton(UI::in) = (Result::out),
	[will_not_call_mercury, promise_pure],
	"
	Result = new javax.swing.JRadioButton (UI.F1);
	"
	).

:- func toString(interfaceData) = string.

toString(label(S)) = S.

:- end_module ui_swing.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
