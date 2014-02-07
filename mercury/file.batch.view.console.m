/**
 * Provides predicates to edit and view a {@code batch} value in a console
 * application.

 * @author Pedro Mariano
 * @version 1.0 2013/06/ 7
 */
:- module file.batch.view.console.

:- interface.

:- pred new(batch, io.state, io.state).
:- mode new(out(latest), di, uo) is det.

:- pred edit(batch, batch, io.state, io.state).
:- mode edit(in(latest), out(latest), di, uo) is det.

:- pred print(batch, io.state, io.state).
:- mode print(in(latest), di, uo) is det.

:- implementation.

:- import_module ebea, ebea.energy, ebea.population.
:- import_module menu, menu.action.
:- import_module my, my.random, my.string.
:- import_module bool, float, list, pretty_printer, string, univ.
:- import_module gl.
:- import_module gl.centipede, gl.centipede.view, gl.centipede.view.console.
:- import_module gl.'2x2', gl.'2x2'.view, gl.'2x2'.view.console.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

new(Result, !IO) :-
	DPB =
	'batch.0.2'(
		mt(clock),
		cpv([], [], [], [], [], [], [], [], 1000),
		no,
		gl.centipede.defaultFactory,
		gl.'2x2'.defaultFactory),
	edit(file.batch.defaultBatch, Result, !IO).

edit(!CPV, !IO) :-
	Menu =
	  [
		menuItemSubMenuField("pseudo-random number generator", random, 'random :=',
		  [
			menuItem("clock", predicate2(clockSeed)),
			menu.action.menuItemEditField("seed", explain(my.string.stringToInt("seed", bounded(0, no), unbound)), 'seed :='),
			menu.action.menuItemSelectField("type", my.random.supplyType, my.random.setType)
		  ]),
		menuItemSubMenuField("common parameter values", commonParameterValues, 'commonParameterValues :=',
		  [
			menu.action.menuItemEditListField( "mutation probability", explain(my.string.stringToFloat("mutation probability", bounded(0.0, yes), bounded(1.0, yes))), lMutationProbability, 'lMutationProbability :='),
			menu.action.menuItemEditListField( "energy reproduce",     explain(my.string.stringToFloat("energy reproduce",     bounded(0.0, no),  unbound)),           lEnergyReproduce, 'lEnergyReproduce :='),
			menu.action.menuItemEditListField( "energy birth",         explain(my.string.stringToFloat("energy birth",         bounded(0.0, yes), unbound)),           lEnergyBirth, 'lEnergyBirth :='),
			menu.action.menuItemEditListField( "energy gain process",  simple(energyGainProcess),                                                                      lEnergyGainProcess, 'lEnergyGainProcess :='),
			menu.action.menuItemEditListField( "population dynamics",  simple(dynamic),                                                                                lPopulationDynamic, 'lPopulationDynamic :='),
			menu.action.menuItemEditListField( "old age",              explain(my.string.stringToInt("old age",                bounded(0, yes),   unbound)),           lOldAge, 'lOldAge :='),
			menu.action.menuItemEditListField( "carrying capacity",    explain(my.string.stringToFloat("carrying capacity",    bounded(0.0, yes), unbound)),           lCarryingCapacity, 'lCarryingCapacity :='),
			menu.action.menuItemEditListField( "runs",                 explain(my.string.stringToInt("runs",                   bounded(0, yes),   unbound)),           lRuns, 'lRuns :='),
			menu.action.menuItemEditField(     "rounds",               explain(my.string.stringToInt("rounds",                 bounded(0, yes),   unbound)),           'rounds :=')
		  ]),
		menuItemSubMenuField("centipede", gameFactory_centipede, set_gameFactory_centipede, gl.centipede.view.console.menuEditFactory),
		menuItemSubMenuField("2x2",       gameFactory_2x2,       set_gameFactory_2x2,       gl.'2x2'.view.console.menuEditFactory)
	  ],
	menu.printMenu(Menu, !CPV, !IO).

print(Batch, !IO) :-
	Doc = docs(
	  [
		str("batch parameters"),
		indent(
		  [
			hard_nl,
			str("pseudo-random number generator"),
			hard_nl,
			pretty_printer.format(Batch^random)
		  ]),
		hard_nl,
		indent(
		  [
			str("EBEA parameters"),
			docFieldList("mutation probability", Batch^commonParameterValues^lMutationProbability),
			docFieldList("energy reproduce",     Batch^commonParameterValues^lEnergyReproduce),
			docFieldList("energy gain process",  Batch^commonParameterValues^lEnergyGainProcess),
			docFieldList("energy at birth",      Batch^commonParameterValues^lEnergyBirth),
			docFieldList("old age",              Batch^commonParameterValues^lOldAge),
			docFieldList("carrying capacity",    Batch^commonParameterValues^lCarryingCapacity),
			docFieldList("population dynamic",   Batch^commonParameterValues^lPopulationDynamic),
			docFieldList("runs",                 Batch^commonParameterValues^lRuns),
			docField("rounds",                   Batch^commonParameterValues^rounds)
		  ]),
		hard_nl,
		str("everything"),
		hard_nl,
		pretty_printer.format(Batch)
	  ]),

	pretty_printer.write_doc(Doc, !IO),
	io.nl(!IO),
	pretty_printer.write_doc(pretty_printer.format(Batch), !IO),
	io.nl(!IO).

:- func map(T) = univ.

map(A) = R :-
	univ.type_to_univ(A, R).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions


:- func docField(string, T) = pretty_printer.doc.

docField(Name, Value) = Result :-
	Result = pretty_printer.indent([hard_nl, str(Name), pretty_printer.indent([hard_nl, pretty_printer.format(Value)])]).

:- func docFieldList(string, list(T)) = pretty_printer.doc.

docFieldList(Name, Value) = Result :-
	Map =
	(func(A) = R :-
	univ.type_to_univ(A, R)),
	Result = pretty_printer.indent([hard_nl, str(Name), pretty_printer.indent([hard_nl, pretty_printer.format_list(list.map(Map, Value), str(" "))])]).

:- pred dynamic(string, ebea.population.dynamic).
:- mode dynamic(in, out) is semidet.

dynamic(S, D) :- ebea.population.stringDynamic(S, D).

:- pred energyGainProcess(string, ebea.energy.energyGainProcess).
:- mode energyGainProcess(in, out) is semidet.

energyGainProcess(S, D) :- ebea.energy.mapEnergyGainProcessString(S, D).

:- pred clockSeed(my.random.supplyParameter, my.random.supplyParameter, io.state, io.state).
:- mode clockSeed(in, out, di, uo) is det.

clockSeed(mt(_), mt(clock), !IO).
clockSeed(random(clock), random(clock `with_type` my.random.seed), !IO).
clockSeed(random(value(_)), random(clock `with_type` my.random.seed), !IO).

:- func 'seed :='(my.random.supplyParameter, int) = my.random.supplyParameter.

'seed :='(mt(_), Seed) = mt(value(Seed)).
'seed :='(random(clock), Seed) = random(value(Seed)).
'seed :='(random(value(_)), Seed) = random(value(Seed)).

:- end_module file.batch.view.console.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
