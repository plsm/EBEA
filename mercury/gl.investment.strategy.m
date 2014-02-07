/**
 * 

 * @author Pedro Mariano
 * @version 1.0 2014/02/06
 */
:- module gl.investment.strategy.

:- interface.

:- type strategy --->
	prob(
		probability :: probability
	) ;
	dete(
		action :: bool
	) .

:- instance parseable(strategy).

:- instance printable(strategy).

/**
 * Return a default value of {@code strategy}.
 */
:- func default = strategy.

:- func dialog = list(dialogItem(strategy)).

/**
 * numberGenes(Strategy) = Result
  
 * Return the number of genes used by the given strategy.  Genes are the
 * unit subject to mutation which is performed by predicate {@code
 * mutateGene}.

 * <p> This predicate is part of type class {@code
 * chromosome(strategy,unit,parameters)}.

 * @see mutateGene/8
 */
:- func numberGenes(strategy) = int.

/**
 * mutateGene(Parameters, Index, !Distribution, !Random, Strategy, Result)
 
 * Mutate one gene of the given strategy.  The parameters, probabilistic
 * distributions and pseudo-random number generators can be used by the
 * mutation operator.

 * <p> This predicate is part of type class {@code
 * chromosome(strategy,unit,parameter)}.
 */

:- pred mutateGene(parameter, int, distribution, distribution, R, R, strategy, strategy)
	<= ePRNG(R).
:- mode mutateGene(in, in, in, out, in, out, in, out) is det.

/**
 * printStrategy(Stream, Strategy, !IO)

 * Print a strategy to the given text stream.
  
 */
:- pred print(io.output_stream, strategy, io.state, io.state).
:- mode print(in, in, di, uo) is det.

:- implementation.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

:- instance parseable(strategy) where
[
	pred(parse/3) is gl.investment.strategy.parse
].

:- instance printable(strategy) where
[
	pred(print/4) is gl.investment.strategy.print
].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

default = Result :-
	Result^probability = default_probability.

dialog =
	[
	di(label("prob"),  subdialog( [
		di(label("probability"),  probability.dialogAction( get_probability,  set(set_probability)))
		])),
	di(label("dete"),  subdialog( [
		di(label("action"),  updateFieldBool(     get_action,  set(set_action)))
		]))
	].

numberGenes(Strategy) = Result :-
	Strategy = prob(_),
	Result = 1
	;
	Strategy = dete(_),
	Result = 1
	.

mutateGene(Parameters, Index, !Distribution, !Random, Strategy, Result) :-
	(if
		Index = 0
	then
		Strategy = prob(InvestProbability),
		probability.addGaussianNoise(Parameters^stddev, InvestProbability, ResultProbability, !Distribution, !Random),
		Result^probability = ResultProbability
		;
		Strategy = dete(InvestAction),
		Result^action = bool.not(InvestAction)
	else
		throw("gl.investment.mutateGene/5: Invalid gene index")
	).

print(Stream, Strategy, !IO) :-
	Strategy = prob(Probability),
	stringStrategyType(Str, prob),
	io.print(Stream, Str, !IO),
	io.print(Stream, " ", !IO),
	io.print(Stream, Probability, !IO)
	;
	Strategy = dete(Action),
	stringStrategyType(Str, dete),
	io.print(Stream, Str, !IO),
	io.print(Stream, " ", !IO),
	io.print(Stream, Action, !IO)
	.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions


:- pred stringStrategyType(string, stype).
:- mode stringStrategyType(in, out) is semidet.
:- mode stringStrategyType(out, in) is det.

stringStrategyType("prob", prob).
stringStrategyType("dete", dete).

:- func default_probability = probability.

default_probability = probability.zero.

:- func default_action = bool.

default_action = no.

:- func get_probability(strategy) = probability.

get_probability(P) = R :-
	P = prob(_),
	R = P^probability
	;
	P = dete(_),
	R = default_probability
	.

:- func set_probability(strategy, probability) = strategy.

set_probability(P, V) = R :-
	P = prob(_),
	R = 'probability :='(P, V)
	;
	P = dete(_),
	R = prob(V)
	.


:- func get_action(strategy) = bool.

get_action(P) = R :-
	P = prob(_),
	R = default_action
	;
	P = dete(_),
	R = P^action
	.

:- func set_action(strategy, bool) = strategy.

set_action(P, V) = R :-
	P = prob(_),
	R = dete(V)
	;
	P = dete(_),
	R = 'action :='(P, V)
	.


:- pred parse(strategy, list(int), list(int)).
:- mode parse(in, out, in) is det.
:- mode parse(out, in, out) is semidet.

parse(P) -->
	{P = prob(Probability)},
	[0],
	probability.parse(Probability)
	.

parse(P) -->
	{P = dete(Action)},
	[1],
	parseable.bool(Action)
	.

:- end_module gl.investment.strategy.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:

