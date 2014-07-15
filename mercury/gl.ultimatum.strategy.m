/**
 * 

 * @author Pedro Mariano
 * @version 1.0 2013/12/14
 */
:- module gl.ultimatum.strategy.

:- interface.

:- type strategy --->
	d(dictator) ;
	s(serf).

:- type dictator --->
	dictator(cakeDivision::int).

:- type serf --->
	simple(cakeAcceptanceThreshold::int) ;
	complete(cakeAcceptance::map(int, bool)).

:- instance parseable(strategy).

/**
 * Return a default Ultimatum strategy that can be used to construct a
 * player's chromosome.
  
 */
:- func default = strategy.

:- func initDictator(int) = strategy.

:- pred initDictator(int, strategy).
:- mode initDictator(in, out) is semidet.

:- func initSerf(int) = strategy.

:- pred initSerf(int, strategy).
:- mode initSerf(in, out) is semidet.

/**
 * Return the number of parameters a strategy has.  Each parameter
 * corresponds to a single gene that can be mutated when a newborn is
 * created.

 * <p>Dictator strategies only have a single parameter that is the cake
 * division proposal.  Serf simple strategies have a single parameter that
 * is the cake acceptance threshold.  A complete serf strategy has a map
 * that assigns to each cake division a Boolean value representing
 * acceptance or not.
 */

:- func numberParameters(strategy) = int.

/**
 * mutateGene(Index, !Random, Strategy, Result)
 
 * Mutate the single gene of an Ultimatum chromosome.  Currently
 * we sum an uniform random variable.

 * <p> This predicate is part of type class {@code chromosome(strategy,unit,parameters)}.
 *
 */

:- pred mutateGene(parameters, int, distribution, distribution, R, R, strategy, strategy)
	<= ePRNG(R).
:- mode mutateGene(in, in, in, out, in, out, in, out) is det.

:- pred print(io.output_stream, strategy, io, io).
:- mode print(in, in, di, uo) is det.

:- func dialog = list(dialogItem(strategy)).

:- implementation.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

:- instance parseable(strategy) where
[
	pred(parse/3) is gl.ultimatum.strategy.parse
].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

default = initDictator(5).

initDictator(CakeDivision) =
	(if
		initDictator(CakeDivision, S)
	then
		S
	else
		throw("gl.ultimatum.strategy.initDictator/1: invalid parameter")
	).

initDictator(CakeDivision, Strategy) :-
	Strategy = d(Dictator),
	Dictator^cakeDivision = CakeDivision,
	CakeDivision >= 0.

initSerf(CakeAcceptanceThreshold) =
	(if
		initSerf(CakeAcceptanceThreshold, S)
	then
		S
	else
		throw("gl.ultimatum.strategy.initSerf/1: invalid parameter")
	).

initSerf(CakeAcceptanceThreshold, Strategy) :-
	Strategy = s(Serf),
	Serf^cakeAcceptanceThreshold = CakeAcceptanceThreshold,
	CakeAcceptanceThreshold >= 0.

numberParameters(Strategy) = Result :-
	Strategy = d(dictator(_)),
	Result = 1
	;
	Strategy = s(simple(_)),
	Result = 1
	;
	Strategy = s(complete(CakeAcceptance)),
	Result = map.count(CakeAcceptance)
	.

mutateGene(Parameters, Index, !Distribution, !Random, Strategy, Result) :-
	Strategy = d(dictator(CakeDivision)),
	(if
		Index = 0
	then
		distribution.unitGaussian(Perturb0, !Distribution, !Random),
		NewCakeDivision =
			int.max(
				0,
				int.min(float.round_to_int(float(CakeDivision) + Perturb0 * Parameters^stdev),
				Parameters^cakeSizeCopy)),
		Result = d(dictator(NewCakeDivision))
	else
		throw("gl.pgp.mutateGene/5: Invalid gene index")
	)
	;
	Strategy = s(Serf),
	Serf = simple(CakeAcceptanceThreshold),
	(if
		Index = 0
	then
		distribution.unitGaussian(Perturb0, !Distribution, !Random),
		NewCakeAcceptanceThreshold = 
			int.max(
				0,
				int.min(float.round_to_int(float(CakeAcceptanceThreshold) + Perturb0 * Parameters^stdev),
				Parameters^cakeSizeCopy)),
		Result = s(simple(NewCakeAcceptanceThreshold))
	else
		throw("gl.pgp.mutateGene/5: Invalid gene index")
	)
	;
	Strategy = s(Serf),
	Serf = complete(CakeAcceptance),
	NewAccept = bool.not(map.lookup(CakeAcceptance, Index)),
	Result = s(complete(map.det_update(CakeAcceptance, Index, NewAccept)))
	.

print(Stream, Strategy, !IO) :-
	Strategy = d(dictator(CakeDivision)),
	io.print(Stream, "d ", !IO),
	io.print(Stream, CakeDivision, !IO)
	;
	Strategy = s(Serf),
	Serf = simple(_),
	io.print(Stream, "ss ", !IO),
	io.print(Stream, Serf^cakeAcceptanceThreshold, !IO)
	;
	Strategy = s(Serf),
	Serf = complete(_),
	io.print(Stream, "sc ", !IO),
	map.foldl(Print, Serf^cakeAcceptance, !IO),
	Print =
	(pred(_Idx::in, B::in, IOdi::di, IOuo::uo) is det :-
		B = yes,
		io.print(Stream, "1", IOdi, IOuo)
		;
		B = no,
		io.print(Stream, "0", IOdi, IOuo)
	)
	.

dialog =
	[di(label("dictator"),  subdialog(
		[di(label("cake division"),  updateFieldInt( getCakeDivision, setCakeDivision))])),
	 di(label("serf"),      subdialog(
		[di(label("cake acceptance threshold"),  updateFieldInt( getCakeAcceptanceThreshold, setCakeAcceptanceThreshold))]))
	].



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- pred parse(strategy, list(int), list(int)).
:- mode parse(in, out, in) is det.
:- mode parse(out, in, out) is semidet.

parse(S) -->
	{S = d(dictator(CakeDivision))},
	[0],
	[CakeDivision]
	;
	{S = s(simple(CakeAcceptanceThreshold))},
	[1, 0, CakeAcceptanceThreshold]
	;
	{S = s(complete(CakeAcceptance))},
	[1, 1],
	parseable.parseMap(withLength, CakeAcceptance)
	.

:- func getCakeDivision(strategy) = int.

getCakeDivision(d(dictator(CakeDivision))) = CakeDivision.

getCakeDivision(s(_)) = 5.

:- func setCakeDivision(strategy, int) = setResult(strategy).

setCakeDivision(_, CakeDivision) =
	(if
		CakeDivision >= 0
	then
		ok(d(dictator(CakeDivision)))
	else
		error("Cake division must be zero or positive")
	).


:- func getCakeAcceptanceThreshold(strategy) = int.

getCakeAcceptanceThreshold(d(_)) = 5.

getCakeAcceptanceThreshold(s(simple(R))) = R.

getCakeAcceptanceThreshold(s(complete(_))) = 5.

:- func setCakeAcceptanceThreshold(strategy, int) = setResult(strategy).

setCakeAcceptanceThreshold(_, CakeAcceptanceThreshold) =
	(if
		CakeAcceptanceThreshold >= 0
	then
		ok(s(simple(CakeAcceptanceThreshold)))
	else
		error("Cake acceptance threshold must be zero or positive")
	).

:- end_module gl.ultimatum.strategy.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
