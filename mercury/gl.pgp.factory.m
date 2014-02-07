/**
 * 

 * @author Pedro Mariano
 * @version 1.0 2013/12/12
 */
:- module gl.pgp.factory.

:- interface.

:- type factory --->
	factory(
		vPlayers              :: list(int),
		vProvisionCost        :: list(float),
		vCooperateProbability :: list(float),
		vQty                  :: list(int),
		vStddev               :: list(float)
	).

:- func default = factory.

:- pred value(factory, string, game, list({int, strategy}), parameters).
:- mode value(in, out, out, out, out) is nondet.

:- func dialog = dialog(factory).

:- func vPlayers(factory) = list(int).

:- func vProvisionCost(factory) = list(float).

:- func vCooperateProbability(factory) = list(float).

:- func vQty(factory) = list(int).

:- func vStddev(factory) = list(float).

:- implementation.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

default = factory(
	[3],    % number of players
	[0.2],  % provision cost
	[0.5],  % provide probability
	[10],   % quantity of initial strategies
	[0.1]   % standard deviation of Gaussian distribution
	).


value(Factory, Key, Game, StrategyQuantities, Parameters) :-
	Key = string.format("%d %f %d %f %f",
		[i(Players), f(ProvisionCost),
		 i(Qty), f(CooperateProbability),
		 f(Stddev)]),
	list.member(Players,              Factory^vPlayers),
	list.member(ProvisionCost,        Factory^vProvisionCost),
	gl.pgp.game.init(Players, 1.0, ProvisionCost, Game),
	list.member(CooperateProbability, Factory^vCooperateProbability),
	list.member(Qty,                  Factory^vQty),
	StrategyQuantities = [{Qty, prob(CooperateProbability)}],
	list.member(Stddev,               Factory^vStddev),
	Parameters = parameters(Stddev).

dialog = dialog(
	[di(label("number of players"),     updateListFieldInt(  vPlayers,               setVPlayers )),
	 di(label("provision cost"),        updateListFieldFloat( vProvisionCost,        setVProvisionCost )),
	 di(label("provision probability"), updateListFieldFloat( vCooperateProbability, setVCooperateProbability)),
	 di(label("strategy quantity"),     updateListFieldInt(   vQty,                  setVQty )),
	 di(label("standard deviation"),    updateListFieldFloat( vStddev,               setVStddev ))
	]).
					 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- func setVPlayers(factory, list(int)) = maybe_error(factory).

setVPlayers(Factory, VPlayers) =
	(if
		all [Players]
		list.member(Players, VPlayers)
		=>
		Players > 1
	then
		ok('vPlayers :='(Factory, VPlayers))
	else
		error("Number of players must be equal to or greater than two")
	).


:- func setVProvisionCost(factory, list(float)) = maybe_error(factory).

setVProvisionCost(Factory, VProvisionCost) =
	(if
		all [ProvisionCost]
			list.member(ProvisionCost, VProvisionCost)
			=>
			ProvisionCost > 0.0
	then
		ok('vProvisionCost :='(Factory, VProvisionCost))
	else
		error("Provision cost must be greater than zero")
	).

:- func setVCooperateProbability(factory, list(float)) = maybe_error(factory).

setVCooperateProbability(Factory, VCooperateProbability) =
	(if
		all [CooperateProbability]
			list.member(CooperateProbability, VCooperateProbability)
			=>
			(
				CooperateProbability =< 1.0,
				CooperateProbability >= 0.0
			)
	then
		ok('vCooperateProbability :='(Factory, VCooperateProbability))
	else
		error("Invalid probability value")
	).


:- func setVQty(factory, list(int)) = maybe_error(factory).

setVQty(Factory, VQty) =
	(if
		all [Qty]
		list.member(Qty, VQty)
		=>
		Qty > 0
	then
		ok('vQty :='(Factory, VQty))
	else
		error("Number of strategies must be equal to or greater than one")
	).



:- func setVStddev(factory, list(float)) = maybe_error(factory).

setVStddev(Factory, VStddev) =
	(if
		all [Stddev]
		list.member(Stddev, VStddev)
		=>
		Stddev >= 0.0
	then
		ok('vStddev :='(Factory, VStddev))
	else
		error("Standard deviation must be zero or positive")
	).



:- end_module gl.pgp.factory.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
