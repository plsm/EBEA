/**
 * 

 * @author Pedro Mariano
 * @version 1.0 2013/12/13
 */
:- module gl.'pgp+pa'.factory.

:- interface.

:- type factory --->
	factory(
		vPlayers             :: list(int),
		vLonerPayoff         :: list(float),
		vProvisionCost       :: list(float),
		vPunishPerformerCost :: list(float),
		vPunishSubjectCost   :: list(float),
		vLonerQty            :: list(int),
		vDefectorQty         :: list(int),
		vCooperatorQty       :: list(int),
		vPunisherQty         :: list(int)
	).

:- func default = factory.

:- pred value(factory, string, game, list({int, strategy}), parameters).
:- mode value(in, out, out, out, out) is nondet.


:- func dialog = list(dialogItem(factory)).


:- func vPlayers(factory) = list(int).

:- func vLonerPayoff(factory) = list(float).

:- func vProvisionCost(factory) = list(float).

:- func vPunishPerformerCost(factory) = list(float).

:- func vPunishSubjectCost(factory) = list(float).



:- implementation.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

default = factory(
	[3, 4, 5],
	[0.5],
	[0.2],
	[0.1],
	[0.2],
	[5, 0],
	[5, 0],
	[5, 0],
	[5, 0]).

value(Factory, Key, Game, StrategyQuantities, Parameters) :-
	Key = string.format("%d %f %f %f %f %d %d %d %d",
		[i(Players), f(LonerPayoff), f(ProvisionCost), f(PunishPerformerCost), f(PunishSubjectCost),
		 i(LonerQty), i(DefectorQty), i(CooperatorQty), i(PunisherQty)]),
	list.member(Players,             Factory^vPlayers),
	list.member(LonerPayoff,         Factory^vLonerPayoff),
	list.member(ProvisionCost,       Factory^vProvisionCost),
	list.member(PunishPerformerCost, Factory^vPunishPerformerCost),
	list.member(PunishSubjectCost,   Factory^vPunishSubjectCost),
	init(Players, LonerPayoff, ProvisionCost, PunishPerformerCost, PunishSubjectCost, Game),
	Parameters = noParameters,
	list.member(LonerQty,      Factory^vLonerQty),
	list.member(DefectorQty,   Factory^vDefectorQty),
	list.member(CooperatorQty, Factory^vCooperatorQty),
	list.member(PunisherQty,   Factory^vPunisherQty),
	StrategyQuantities = [{LonerQty, pure(loner)}, {DefectorQty, pure(defector)}, {CooperatorQty, pure(cooperator)}, {PunisherQty, pure(punisher)}].

dialog =
	[
	di(label("number of players"),       updateListFieldInt(    vPlayers,               setVPlayers)),
	di(label("provision cost"),          updateListFieldFloat(  vProvisionCost,         setVProvisionCost)),
	di(label("loner payoff"),            updateListFieldFloat(  vLonerPayoff,           setVLonerPayoff)),
	di(label("punisher performer cost"), updateListFieldFloat(  vPunishPerformerCost, setVPunishPerformerCost)),
	di(label("punisher subject cost"),   updateListFieldFloat(  vPunishSubjectCost,   setVPunishSubjectCost))
	].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions


:- func setVPlayers(factory, list(int)) = setResult(factory).

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

:- func setVProvisionCost(factory, list(float)) = setResult(factory).

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

:- func setVLonerPayoff(factory, list(float)) = setResult(factory).

setVLonerPayoff(Factory, VLonerPayoff) =
	(if
		all [LonerPayoff]
			list.member(LonerPayoff, VLonerPayoff)
			=>
			LonerPayoff > 0.0
	then
		ok('vLonerPayoff :='(Factory, VLonerPayoff))
	else
		error("Loner payoff must be greater than zero")
	).



:- func setVPunishPerformerCost(factory, list(float)) = setResult(factory).

setVPunishPerformerCost(Factory, VPunishPerformerCost) =
	(if
		all [PunishPerformerCost]
			list.member(PunishPerformerCost, VPunishPerformerCost)
			=>
			PunishPerformerCost > 0.0
	then
		ok('vPunishPerformerCost :='(Factory, VPunishPerformerCost))
	else
		error("Punish performer cost must be greater than zero")
	).

:- func setVPunishSubjectCost(factory, list(float)) = setResult(factory).

setVPunishSubjectCost(Factory, VPunishSubjectCost) =
	(if
		all [PunishSubjectCost]
			list.member(PunishSubjectCost, VPunishSubjectCost)
			=>
			PunishSubjectCost > 0.0
	then
		ok('vPunishSubjectCost :='(Factory, VPunishSubjectCost))
	else
		error("Punish subject cost must be greater than zero")
	).

:- end_module gl.'pgp+pa'.factory.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
