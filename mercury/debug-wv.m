/**
 * Debug the weight vector module predicates and functions.

 * @author Pedro Mariano
 * @version 1.0 2014/10/11
 */
:- module 'debug-wv'.

:- interface.

:- import_module io.

:- pred main(io.state::di, io.state::uo) is det.

:- implementation.

:- import_module ebea, ebea.player, ebea.player.selection, ebea.player.selection.wv.
:- import_module ui_console, userInterface.
:- import_module int, float, list, random, string.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

:- type data --->
	data(
		wv  :: weightVector(string),
		rng :: random.supply
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

main(!IO) :-
	random.init(123, Random),
	Data = data(ebea.player.selection.wv.init, Random),
	ui_console.show(menu, Data, _, !IO)
	.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- func menu = userInterface(data).
:- mode menu = out(userInterface).

menu = m(
	[
	 mi(label("Add Element"),          updateDataIO(addElement)),
	 mi(label("Draw Elements"),        updateDataIO(drawElements)),
	 mi(label("Update Weight"),        updateDataIO('debug-wv'.updateWeight)),
	 mi(label("Print"),                actionDataIO(printWV))
	]).


:- pred addElement(
	data     :: in,  data     :: out,
	io.state :: di,  io.state :: uo
) is det.

addElement(!Data, !IO) :-
	io.print("Element? ", !IO),
	io.flush_output(!IO),
	io.read_line_as_string(IElement, !IO),
	io.print("Weight? ", !IO),
	io.flush_output(!IO),
	io.read_line_as_string(IWeight, !IO),
	(if
		IElement = ok(SElement),
		Element = string.strip(SElement),
		IWeight = ok(SWeight),
		string.to_float(string.strip(SWeight), Weight),
		Weight >= 0.0
	then
		ebea.player.selection.wv.addElements([Element], Weight, !.Data^wv, NextWV),
		!:Data = 'wv :='(!.Data, NextWV)
	else
		io.print("Input Error!\n", !IO)
	).

:- pred updateWeight(
	data     :: in,  data     :: out,
	io.state :: di,  io.state :: uo
) is det.

updateWeight(!Data, !IO) :-
	io.print("Element? ", !IO),
	io.flush_output(!IO),
	io.read_line_as_string(IElement, !IO),
	io.print("Weight? ", !IO),
	io.flush_output(!IO),
	io.read_line_as_string(IWeight, !IO),
	io.print("Probability? ", !IO),
	io.flush_output(!IO),
	io.read_line_as_string(IProbability, !IO),
	(if
		IElement = ok(SElement),
		Element = string.strip(SElement),
		IWeight = ok(SWeight),
		string.to_float(string.strip(SWeight), Weight),
		Weight >= 0.0,
		IProbability = ok(SProbability),
		string.to_float(string.strip(SProbability), Probability),
		Probability >= 0.0,
		Probability =< 1.0
	then
		ebea.player.selection.wv.updateWeight(Probability, Weight, Element, !.Data^wv, NextWV),
		!:Data = 'wv :='(!.Data, NextWV)
	else
		io.print("Input Error!\n", !IO)
	).

:- pred drawElements(
	data     :: in,  data     :: out,
	io.state :: di,  io.state :: uo
) is det.

drawElements(!Data, !IO) :-
	io.print("How many elements? ", !IO),
	io.flush_output(!IO),
	io.read_line_as_string(IQty, !IO),
	(	if
		IQty = ok(SQty),
		string.to_int(string.strip(SQty), Qty),
		Qty > 0
	then
		ebea.player.selection.wv.drawAsList(
			!.Data^wv,
			Qty,
			Result,
			!.Data^rng, NextRng),
		!:Data = 'rng :='(!.Data, NextRng),
		io.print("DBG ", !IO),
		io.print(Result, !IO),
		io.nl(!IO)
	else
		io.print("Input Error!\n", !IO)
	).


:- pred printWV(
	data     :: in,
	io.state :: di,  io.state :: uo
) is det.

printWV(Data, !IO) :-
	PredPrintElementWeight =
	(pred(Element::in, Weight::in, !.IOl::di, !:IOl::uo) is det :-
		io.print("\t", !IOl),
		io.print(Element, !IOl),
		io.print(":", !IOl),
		io.print(Weight, !IOl)
	),
	io.print("DBG ", !IO),
	ebea.player.selection.wv.fold(PredPrintElementWeight, Data^wv, !IO),
	io.nl(!IO)
	.
	
:- end_module 'debug-wv'.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
