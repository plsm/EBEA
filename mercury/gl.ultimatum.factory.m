/**
 * 

 * @author Pedro Mariano
 * @version 1.0 2013/12/14
 */
:- module gl.ultimatum.factory.

:- interface.

:- type factory --->
	factory(
		vCakeSize                :: list(int),
		vDictatorCakeDivision    :: list(int),
		vDictatorQty             :: list(int),
		vSerfAcceptanceThreshold :: list(int),
		vSerfQty                 :: list(int),
		vStddev                  :: list(float)
	).

:- func default = factory.

:- pred value(factory, string, game, list({int, strategy}), parameters).
:- mode value(in, out, out, out, out) is nondet.

:- implementation.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

default = factory(
	[10],  % cake size
	[5],   % dictator cake division
	[10],  % number of dictators
	[5],   % serf acceptance threshold
	[10],  % number of serfs
	[1.0]  % standard deviation of discrete Gaussian distribution
	).

value(Factory, Key, Game, StrategyQuantities, Parameters) :-
	Key = string.format("%d %d %d %d %d %f",
		[i(CakeSize),
		 i(CakeDivision), i(DictatorQty),
		 i(SerfAcceptanceThreshold), i(SerfQty),
		 f(Stddev)]),
	list.member(CakeSize,        Factory^vCakeSize),
	init(CakeSize, Game),
	list.member(CakeDivision,  Factory^vDictatorCakeDivision),
	list.member(DictatorQty,   Factory^vDictatorQty),
	CakeDivision =< CakeSize,
	initDictator(CakeDivision, DictatorStrategy),
	list.member(SerfAcceptanceThreshold,  Factory^vSerfAcceptanceThreshold),
	list.member(SerfQty,                  Factory^vSerfQty),
	SerfAcceptanceThreshold =< CakeSize,
	initSerf(SerfAcceptanceThreshold, SerfStrategy),
	StrategyQuantities = [{DictatorQty, DictatorStrategy}, {SerfQty, SerfStrategy}],
	list.member(Stddev,               Factory^vStddev),
	Parameters = parameters(CakeSize, Stddev).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- end_module gl.ultimatum.factory.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
