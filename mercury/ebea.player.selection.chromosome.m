/**
 * 

 * @author Pedro Mariano
 * @version 1.0 2014/03/05
 */
:- module ebea.player.selection.chromosome.

:- interface.

:- import_module userInterface.
:- import_module parseable.


/**
 * The chromosome part that is responsible for partner selection.
 * Selection can be random, based on the partner selection algorithm
 * presented in Mariano 2009, or based on generic opinion.

 * TODO: opinion chromosome has average and deviation of initial uncertainty.

 * TODO2 : remove payoff threshold from opinion. Increases search space
 * unnecessarily.
  
 */
:- type chromosome --->
	random ;
	partnerSelection(
		poolSize                :: int ,
		bitsPerProbability      :: int ,
		probabilityUpdateFactor :: float ,
		payoffThreshold_PS      :: float
	) ;
	opinion(
		payoffThreshold_O  :: float ,
		initialUncertainty :: float
	) ;
	opinion(
		initialAverageOpinion     :: float ,
		initialStdDevOpinion      :: float ,
		initialAverageUncertainty :: float ,
		initialStdDevUncertainty  :: float
	) .

:- instance parseable(ebea.player.selection.chromosome.chromosome).

:- instance printable(ebea.player.selection.chromosome.chromosome).


/**
 * Return a default value of {@code chromosome}.
 */
:- func default = ebea.player.selection.chromosome.chromosome.

:- func dialog = list(dialogItem(ebea.player.selection.chromosome.chromosome)).

:- pred parse(ebea.player.selection.chromosome.chromosome, list(int), list(int)).
:- mode parse(in, out, in) is det.
:- mode parse(out, in, out) is semidet.

/**
 * Print the selection part of an EBEA chromosome.
 */
:- pred print(io.output_stream, ebea.player.selection.chromosome.chromosome, io, io).
:- mode print(in, in, di, uo) is det.


:- implementation.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

:- instance parseable(ebea.player.selection.chromosome.chromosome) where
[
	pred(parse/3) is ebea.player.selection.chromosome.parse
].

:- instance printable(ebea.player.selection.chromosome.chromosome)
	where
[
	pred(print/4) is ebea.player.selection.chromosome.print
].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

default = random.

dialog =
	[di(label("selection genes"), selectOneOf(
		getCurrentChoice,
		setChoice,
		[
		 ci(label("random"),             []),
		 ci(label("partner selection"),
			[
			 di(label("pool size"),                  updateFieldInt(   get_poolSize,                 checkInt(   "pool size",                  bounded(0, yes), unbound,             set_poolSize))),
			 di(label("bits per probability"),       updateFieldInt(   get_bitsPerProbability,       checkInt(   "bits per probability",       bounded(1, yes), unbound,             set_bitsPerProbability))),
			 di(label("probability update factor"),  updateFieldFloat( get_probabilityUpdateFactor,  checkFloat( "probability update factor",  bounded(0.0, yes), bounded(1.0, yes), set_probabilityUpdateFactor))),
			 di(label("payoff threshold"),           updateFieldFloat( get_payoffThreshold_PS,       checkFloat( "payoff threshold",           bounded(-1.0, yes), bounded(1.0, yes), set_payoffThreshold_PS)))
			]),
		 ci(label("opinion"),
			[
			 di(label("payoff threshold (as fraction of game payoff)"),  updateFieldFloat( get_payoffThreshold_O,       checkFloat( "payoff threshold",     bounded(0.0, yes),  bounded(1.0, yes), set_payoffThreshold_O))),
			 di(label("initial uncertainty"),                            updateFieldFloat( get_initialUncertainty,      checkFloat( "initial uncertainty",  bounded(-1.0, yes), bounded(1.0, yes), set_initialUncertainty)))
			]),
		 ci(label("opinion"),
			 [
			  di(label("initialAverageOpinion"),      updateFieldFloat(    get_initialAverageOpinion,      checkFloat( "initialAverageOpinion",      unbound, unbound, set_initialAverageOpinion))),
			  di(label("initialStdDevOpinion"),       updateFieldFloat(    get_initialStdDevOpinion,       checkFloat( "initialStdDevOpinion",       unbound, unbound, set_initialStdDevOpinion))),
			  di(label("initialAverageUncertainty"),  updateFieldFloat(    get_initialAverageUncertainty,  checkFloat( "initialAverageUncertainty",  unbound, unbound, set_initialAverageUncertainty))),
			  di(label("initialStdDevUncertainty"),   updateFieldFloat(    get_initialStdDevUncertainty,   checkFloat( "initialStdDevUncertainty",   unbound, unbound, set_initialStdDevUncertainty)))
			 ])
		]))].

parse(P) -->
	{P = random},
	[0]
	.

parse(P) -->
	{P = partnerSelection(_, _, _, _)},
	[1],
	parseable.int32(P^poolSize),
	parseable.int32(P^bitsPerProbability),
	parseable.float32(P^probabilityUpdateFactor),
	parseable.float32(P^payoffThreshold_PS)
	.

parse(P) -->
	{P = opinion(_, _)},
	[2],
	parseable.float32(P^payoffThreshold_O),
	parseable.float32(P^initialUncertainty)
	.

parse(P) -->
	{P = opinion(_, _, _, _)},
	[3],
	parseable.float32(P^initialAverageOpinion),
	parseable.float32(P^initialStdDevOpinion),
	parseable.float32(P^initialAverageUncertainty),
	parseable.float32(P^initialStdDevUncertainty)
	.


print(Stream, Chromosome, !IO) :-
	Chromosome = random,
	io.print(Stream, "random", !IO)
	;
	Chromosome = partnerSelection(_, _, _, _),
	io.print(Stream, Chromosome^poolSize, !IO),
	io.print(Stream, " ", !IO),
	io.print(Stream, Chromosome^bitsPerProbability, !IO),
	io.print(Stream, " ", !IO),
	io.print(Stream, Chromosome^probabilityUpdateFactor, !IO),
	io.print(Stream, " ", !IO),
	io.print(Stream, Chromosome^payoffThreshold_PS, !IO)
	;
	Chromosome = opinion(_, _),
	io.print(Stream, Chromosome^payoffThreshold_O, !IO),
	io.print(Stream, ' ', !IO),
	io.print(Stream, Chromosome^initialUncertainty, !IO)
	;
	Chromosome = opinion(_, _, _, _),
	io.print(Stream, Chromosome^initialAverageOpinion, !IO),
	io.print(Stream, ' ', !IO),
	io.print(Stream, Chromosome^initialStdDevOpinion, !IO),
	io.print(Stream, ' ', !IO),
	io.print(Stream, Chromosome^initialAverageUncertainty, !IO),
	io.print(Stream, ' ', !IO),
	io.print(Stream, Chromosome^initialStdDevUncertainty, !IO)
	.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- func getCurrentChoice(ebea.player.selection.chromosome.chromosome) = maybe(int).

getCurrentChoice(random)                       = yes(0).
getCurrentChoice(partnerSelection(_, _, _, _)) = yes(1).
getCurrentChoice(opinion(_, _))                = yes(2).
getCurrentChoice(opinion(_, _, _, _))          = yes(3).

:- func setChoice(ebea.player.selection.chromosome.chromosome, int) = setResult(ebea.player.selection.chromosome.chromosome).

setChoice(Chromosome, Index) = ok(Result) :-
	(if
		Chromosome = random,                       Index = 0, R = Chromosome ;
		Chromosome = partnerSelection(_, _, _, _), Index = 0, R = random ;
		Chromosome = opinion(_, _),                Index = 0, R = random ;
		Chromosome = opinion(_, _, _, _),          Index = 0, R = random ;
		
		Chromosome = random,                       Index = 1, R = partnerSelection(default_poolSize, default_bitsPerProbability, default_probabilityUpdateFactor, default_payoffThreshold_PS) ;
		Chromosome = partnerSelection(_, _, _, _), Index = 1, R = Chromosome ;
		Chromosome = opinion(_, _),                Index = 1, R = partnerSelection(default_poolSize, default_bitsPerProbability, default_probabilityUpdateFactor, default_payoffThreshold_PS) ;
		Chromosome = opinion(_, _, _, _),          Index = 1, R = partnerSelection(default_poolSize, default_bitsPerProbability, default_probabilityUpdateFactor, default_payoffThreshold_PS) ;
		
		Chromosome = random,                       Index = 2, R = opinion(default_payoffThreshold_O, default_initialUncertainty) ;
		Chromosome = partnerSelection(_, _, _, _), Index = 2, R = opinion(default_payoffThreshold_O, default_initialUncertainty) ;
		Chromosome = opinion(_, _),                Index = 2, R = Chromosome ;
		Chromosome = opinion(_, _, _, _),          Index = 2, R = opinion(default_payoffThreshold_O, default_initialUncertainty) ;
		
		Chromosome = random,                       Index = 3, R = opinion(default_initialAverageOpinion, default_initialStdDevOpinion, default_initialAverageUncertainty, default_initialStdDevUncertainty) ;
		Chromosome = partnerSelection(_, _, _, _), Index = 3, R = opinion(default_initialAverageOpinion, default_initialStdDevOpinion, default_initialAverageUncertainty, default_initialStdDevUncertainty) ;
		Chromosome = opinion(_, _),                Index = 3, R = opinion(default_initialAverageOpinion, default_initialStdDevOpinion, default_initialAverageUncertainty, default_initialStdDevUncertainty) ;
		Chromosome = opinion(_, _, _, _),          Index = 3, R = Chromosome
	then
		Result = R
	else
		throw("setChoice/2: invalid index")
	).

:- func default_poolSize = int.

default_poolSize = 0.

:- func default_bitsPerProbability = int.

default_bitsPerProbability = 1.

:- func default_probabilityUpdateFactor = float.

default_probabilityUpdateFactor = 0.0.

:- func default_payoffThreshold_PS = float.

default_payoffThreshold_PS = 0.0.

:- func default_payoffThreshold_O = float.

default_payoffThreshold_O = 0.0.

:- func default_initialUncertainty = float.

default_initialUncertainty = 1.0.

:- func default_initialAverageOpinion = float.

default_initialAverageOpinion = 0.0.

:- func default_initialStdDevOpinion = float.

default_initialStdDevOpinion = 0.0.

:- func default_initialAverageUncertainty = float.

default_initialAverageUncertainty = 0.0.

:- func default_initialStdDevUncertainty = float.

default_initialStdDevUncertainty = 0.0.


:- func get_poolSize(ebea.player.selection.chromosome.chromosome) = int.

get_poolSize(P) = R :-
	P = random,
	R = default_poolSize
	;
	P = partnerSelection(_, _, _, _),
	R = P^poolSize
	;
	P = opinion(_, _),
	R = default_poolSize
	;
	P = opinion(_, _, _, _),
	R = default_poolSize
	.

:- func set_poolSize(ebea.player.selection.chromosome.chromosome, int) = ebea.player.selection.chromosome.chromosome.

set_poolSize(P, V) = R :-
	P = random,
	R = partnerSelection(V, default_bitsPerProbability, default_probabilityUpdateFactor, default_payoffThreshold_PS)
	;
	P = partnerSelection(_, _, _, _),
	R = 'poolSize :='(P, V)
	;
	P = opinion(_, _),
	R = partnerSelection(V, default_bitsPerProbability, default_probabilityUpdateFactor, default_payoffThreshold_PS)
	;
	P = opinion(_, _, _, _),
	R = partnerSelection(V, default_bitsPerProbability, default_probabilityUpdateFactor, default_payoffThreshold_PS)
	.


:- func get_bitsPerProbability(ebea.player.selection.chromosome.chromosome) = int.

get_bitsPerProbability(P) = R :-
	P = random,
	R = default_bitsPerProbability
	;
	P = partnerSelection(_, _, _, _),
	R = P^bitsPerProbability
	;
	P = opinion(_, _),
	R = default_bitsPerProbability
	;
	P = opinion(_, _, _, _),
	R = default_bitsPerProbability
	.

:- func set_bitsPerProbability(ebea.player.selection.chromosome.chromosome, int) = ebea.player.selection.chromosome.chromosome.

set_bitsPerProbability(P, V) = R :-
	P = random,
	R = partnerSelection(default_poolSize, V, default_probabilityUpdateFactor, default_payoffThreshold_PS)
	;
	P = partnerSelection(_, _, _, _),
	R = 'bitsPerProbability :='(P, V)
	;
	P = opinion(_, _),
	R = partnerSelection(default_poolSize, V, default_probabilityUpdateFactor, default_payoffThreshold_PS)
	;
	P = opinion(_, _, _, _),
	R = partnerSelection(default_poolSize, V, default_probabilityUpdateFactor, default_payoffThreshold_PS)
	.


:- func get_probabilityUpdateFactor(ebea.player.selection.chromosome.chromosome) = float.

get_probabilityUpdateFactor(P) = R :-
	P = random,
	R = default_probabilityUpdateFactor
	;
	P = partnerSelection(_, _, _, _),
	R = P^probabilityUpdateFactor
	;
	P = opinion(_, _),
	R = default_probabilityUpdateFactor
	;
	P = opinion(_, _, _, _),
	R = default_probabilityUpdateFactor
	.

:- func set_probabilityUpdateFactor(ebea.player.selection.chromosome.chromosome, float) = ebea.player.selection.chromosome.chromosome.

set_probabilityUpdateFactor(P, V) = R :-
	P = random,
	R = partnerSelection(default_poolSize, default_bitsPerProbability, V, default_payoffThreshold_PS)
	;
	P = partnerSelection(_, _, _, _),
	R = 'probabilityUpdateFactor :='(P, V)
	;
	P = opinion(_, _),
	R = partnerSelection(default_poolSize, default_bitsPerProbability, V, default_payoffThreshold_PS)
	;
	P = opinion(_, _, _, _),
	R = partnerSelection(default_poolSize, default_bitsPerProbability, V, default_payoffThreshold_PS)
	.


:- func get_payoffThreshold_PS(ebea.player.selection.chromosome.chromosome) = float.

get_payoffThreshold_PS(P) = R :-
	P = random,
	R = default_payoffThreshold_PS
	;
	P = partnerSelection(_, _, _, _),
	R = P^payoffThreshold_PS
	;
	P = opinion(_, _),
	R = default_payoffThreshold_PS
	;
	P = opinion(_, _, _, _),
	R = default_payoffThreshold_PS
	.

:- func set_payoffThreshold_PS(ebea.player.selection.chromosome.chromosome, float) = ebea.player.selection.chromosome.chromosome.

set_payoffThreshold_PS(P, V) = R :-
	P = random,
	R = partnerSelection(default_poolSize, default_bitsPerProbability, default_probabilityUpdateFactor, V)
	;
	P = partnerSelection(_, _, _, _),
	R = 'payoffThreshold_PS :='(P, V)
	;
	P = opinion(_, _),
	R = partnerSelection(default_poolSize, default_bitsPerProbability, default_probabilityUpdateFactor, V)
	;
	P = opinion(_, _, _, _),
	R = partnerSelection(default_poolSize, default_bitsPerProbability, default_probabilityUpdateFactor, V)
	.


:- func get_payoffThreshold_O(ebea.player.selection.chromosome.chromosome) = float.

get_payoffThreshold_O(P) = R :-
	P = random,
	R = default_payoffThreshold_O
	;
	P = partnerSelection(_, _, _, _),
	R = default_payoffThreshold_O
	;
	P = opinion(_, _),
	R = P^payoffThreshold_O
	;
	P = opinion(_, _, _, _),
	R = default_payoffThreshold_O
	.

:- func set_payoffThreshold_O(ebea.player.selection.chromosome.chromosome, float) = ebea.player.selection.chromosome.chromosome.

set_payoffThreshold_O(P, V) = R :-
	P = random,
	R = opinion(V, default_initialUncertainty)
	;
	P = partnerSelection(_, _, _, _),
	R = opinion(V, default_initialUncertainty)
	;
	P = opinion(_, _),
	R = 'payoffThreshold_O :='(P, V)
	;
	P = opinion(_, _, _, _),
	R = opinion(V, default_initialUncertainty)
	.


:- func get_initialUncertainty(ebea.player.selection.chromosome.chromosome) = float.

get_initialUncertainty(P) = R :-
	P = random,
	R = default_initialUncertainty
	;
	P = partnerSelection(_, _, _, _),
	R = default_initialUncertainty
	;
	P = opinion(_, _),
	R = P^initialUncertainty
	;
	P = opinion(_, _, _, _),
	R = default_initialUncertainty
	.

:- func set_initialUncertainty(ebea.player.selection.chromosome.chromosome, float) = ebea.player.selection.chromosome.chromosome.

set_initialUncertainty(P, V) = R :-
	P = random,
	R = opinion(default_payoffThreshold_O, V)
	;
	P = partnerSelection(_, _, _, _),
	R = opinion(default_payoffThreshold_O, V)
	;
	P = opinion(_, _),
	R = 'initialUncertainty :='(P, V)
	;
	P = opinion(_, _, _, _),
	R = opinion(default_payoffThreshold_O, V)
	.


:- func get_initialAverageOpinion(ebea.player.selection.chromosome.chromosome) = float.

get_initialAverageOpinion(P) = R :-
	P = random,
	R = default_initialAverageOpinion
	;
	P = partnerSelection(_, _, _, _),
	R = default_initialAverageOpinion
	;
	P = opinion(_, _),
	R = default_initialAverageOpinion
	;
	P = opinion(_, _, _, _),
	R = P^initialAverageOpinion
	.

:- func set_initialAverageOpinion(ebea.player.selection.chromosome.chromosome, float) = ebea.player.selection.chromosome.chromosome.

set_initialAverageOpinion(P, V) = R :-
	P = random,
	R = opinion(V, default_initialStdDevOpinion, default_initialAverageUncertainty, default_initialStdDevUncertainty)
	;
	P = partnerSelection(_, _, _, _),
	R = opinion(V, default_initialStdDevOpinion, default_initialAverageUncertainty, default_initialStdDevUncertainty)
	;
	P = opinion(_, _),
	R = opinion(V, default_initialStdDevOpinion, default_initialAverageUncertainty, default_initialStdDevUncertainty)
	;
	P = opinion(_, _, _, _),
	R = 'initialAverageOpinion :='(P, V)
	.


:- func get_initialStdDevOpinion(ebea.player.selection.chromosome.chromosome) = float.

get_initialStdDevOpinion(P) = R :-
	P = random,
	R = default_initialStdDevOpinion
	;
	P = partnerSelection(_, _, _, _),
	R = default_initialStdDevOpinion
	;
	P = opinion(_, _),
	R = default_initialStdDevOpinion
	;
	P = opinion(_, _, _, _),
	R = P^initialStdDevOpinion
	.

:- func set_initialStdDevOpinion(ebea.player.selection.chromosome.chromosome, float) = ebea.player.selection.chromosome.chromosome.

set_initialStdDevOpinion(P, V) = R :-
	P = random,
	R = opinion(default_initialAverageOpinion, V, default_initialAverageUncertainty, default_initialStdDevUncertainty)
	;
	P = partnerSelection(_, _, _, _),
	R = opinion(default_initialAverageOpinion, V, default_initialAverageUncertainty, default_initialStdDevUncertainty)
	;
	P = opinion(_, _),
	R = opinion(default_initialAverageOpinion, V, default_initialAverageUncertainty, default_initialStdDevUncertainty)
	;
	P = opinion(_, _, _, _),
	R = 'initialStdDevOpinion :='(P, V)
	.


:- func get_initialAverageUncertainty(ebea.player.selection.chromosome.chromosome) = float.

get_initialAverageUncertainty(P) = R :-
	P = random,
	R = default_initialAverageUncertainty
	;
	P = partnerSelection(_, _, _, _),
	R = default_initialAverageUncertainty
	;
	P = opinion(_, _),
	R = default_initialAverageUncertainty
	;
	P = opinion(_, _, _, _),
	R = P^initialAverageUncertainty
	.

:- func set_initialAverageUncertainty(ebea.player.selection.chromosome.chromosome, float) = ebea.player.selection.chromosome.chromosome.

set_initialAverageUncertainty(P, V) = R :-
	P = random,
	R = opinion(default_initialAverageOpinion, default_initialStdDevOpinion, V, default_initialStdDevUncertainty)
	;
	P = partnerSelection(_, _, _, _),
	R = opinion(default_initialAverageOpinion, default_initialStdDevOpinion, V, default_initialStdDevUncertainty)
	;
	P = opinion(_, _),
	R = opinion(default_initialAverageOpinion, default_initialStdDevOpinion, V, default_initialStdDevUncertainty)
	;
	P = opinion(_, _, _, _),
	R = 'initialAverageUncertainty :='(P, V)
	.


:- func get_initialStdDevUncertainty(ebea.player.selection.chromosome.chromosome) = float.

get_initialStdDevUncertainty(P) = R :-
	P = random,
	R = default_initialStdDevUncertainty
	;
	P = partnerSelection(_, _, _, _),
	R = default_initialStdDevUncertainty
	;
	P = opinion(_, _),
	R = default_initialStdDevUncertainty
	;
	P = opinion(_, _, _, _),
	R = P^initialStdDevUncertainty
	.

:- func set_initialStdDevUncertainty(ebea.player.selection.chromosome.chromosome, float) = ebea.player.selection.chromosome.chromosome.

set_initialStdDevUncertainty(P, V) = R :-
	P = random,
	R = opinion(default_initialAverageOpinion, default_initialStdDevOpinion, default_initialAverageUncertainty, V)
	;
	P = partnerSelection(_, _, _, _),
	R = opinion(default_initialAverageOpinion, default_initialStdDevOpinion, default_initialAverageUncertainty, V)
	;
	P = opinion(_, _),
	R = opinion(default_initialAverageOpinion, default_initialStdDevOpinion, default_initialAverageUncertainty, V)
	;
	P = opinion(_, _, _, _),
	R = 'initialStdDevUncertainty :='(P, V)
	.



:- end_module ebea.player.selection.chromosome.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:

