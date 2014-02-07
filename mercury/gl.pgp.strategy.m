/**
 * 

 * @author Pedro Mariano
 * @version 1.0 2013/12/12
 */
:- module gl.pgp.strategy.

:- interface.

:- import_module bool.

/**
 * The strategy of a Public Good Provision game is the probability to provide the good.
 */
:- type strategy --->
	prob(provideProbability :: float) ;
	dete(provideAction      :: bool).

:- instance printable(strategy).

:- instance parseable(strategy).

/**
 * Return a default Public Good Provision strategy that can be used to
 * construct a player's chromosome.
  
 */
:- func default = strategy.

:- pred init(float, strategy).
:- mode init(in, out) is semidet.

:- func init(float) = strategy.


/**
 * Returns value {@code 1}.  The strategy of the Public Good Provision game
 * only contains one parameter: the probability to provide for the good or
 * whether or not the player provides the good.

 * <p> This function is part of type class {@code
 * chromosome(strategy,strategy)}.
 */

:- func numberParameters(strategy) = int.

/**
 * mutateGene(Parameters, Index, !Distribution, !Random, Strategy, Result)
 
 * Mutate the single gene of a Public Good Provision chromosome.  Currently
 * we sum an uniform random variable.

 * <p> This predicate is part of type class {@code chromosome(strategy,strategy)}.
 *
 */

:- pred mutateGene(parameters, int, distribution, distribution, R, R, strategy, strategy)
	<= ePRNG(R).
:- mode mutateGene(in, in, in, out, in, out, in, out) is det.

:- func dialog = list(dialogItem(strategy)).

/**
 * <p> This predicate is part of type class {@code
   chromosome(strategy,strategy)}.
 */

:- pred print(io.output_stream, strategy, io, io).
:- mode print(in, in, di, uo) is det.


:- implementation.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

:- instance printable(strategy) where
	[
		pred(print/4) is gl.pgp.strategy.print
	].

:- instance parseable(strategy) where
[
	pred(parse/3) is gl.pgp.strategy.parse
].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

default = dete(yes).

init(ProvideProbability, Strategy) :-
	ProvideProbability >= 0.0,
	ProvideProbability =< 1.0,
	Strategy = prob(ProvideProbability).

init(ProvideProbability) = Result :-
	(if
		init(ProvideProbability, Strategy)
	then
		Result = Strategy
	else
		throw("gl.pgp.strategy.init/1: invalid parameters")
	).

numberParameters(_) = 1.

mutateGene(Parameters, Index, !Distribution, !Random, Strategy, Result) :-
	(if
		Index = 0
	then
		Strategy = prob(ProvideProbability),
		distribution.unitGaussian(Perturb0, !Distribution, !Random),
		Perturb = Perturb0 * Parameters^stdev,
		Result = prob(float.max(0.0, float.min(ProvideProbability + Perturb, 1.0)))
		;
		Strategy = dete(ProvideAction),
		Result = dete(bool.not(ProvideAction))
	else
		throw("gl.pgp.mutateGene/5: Invalid gene index")
	).

dialog =
	[
	di(label("provide probability"), updateFieldFloat( getProvideProbability, userInterface.checkFloat("provide probability", bounded(0.0, yes), bounded(1.0, yes), setProvideProbability))),
	di(label("provide action"),      updateFieldBool(  getProvideAction,      setProvideAction))
	].

print(Stream, Strategy, !IO) :-
	Strategy = prob(ProvideProbability),
	io.print(Stream, "p ", !IO),
	io.print(Stream, ProvideProbability, !IO)
	;
	Strategy = dete(ProvideAction),
	io.print(Stream, "d ", !IO),
	io.print(Stream, ProvideAction, !IO)
	.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- pred parse(strategy, list(int), list(int)).
:- mode parse(in, out, in) is det.
:- mode parse(out, in, out) is semidet.

parse(P) -->
	{P = prob(ProvideProbability)},
	[0],
	parseable.float32(ProvideProbability)
	;
	{P = dete(ProvideAction)},
	[1],
	parseable.bool(ProvideAction)
	.

:- func getProvideProbability(strategy) = float.

getProvideProbability(prob(Result)) = Result.
getProvideProbability(dete(yes)) = 1.0.
getProvideProbability(dete(no)) = 0.0.

:- func getProvideAction(strategy) = bool.

getProvideAction(dete(Result)) = Result.
getProvideAction(prob(ProvideProbability)) =
	(if
		ProvideProbability < 0.5
	then
		no
	else
		yes
	).

:- func setProvideProbability(strategy, float) = strategy.

setProvideProbability(_, ProvideProbability) = prob(ProvideProbability).

:- func setProvideAction(strategy, bool) = setResult(strategy).

setProvideAction(_, ProvideAction) = ok(dete(ProvideAction)).


:- end_module gl.pgp.strategy.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
