/**
 * 

 * @author Pedro Mariano
 * @version 1.0 2013/12/ 5
 * @version 2.0 2013/12/30
 */
:- module gl.givetake.game.

:- interface.

:- type game --->
	game(
		bg     :: float,
		cpt    :: float,
		cst    :: float,
		numberStages :: numberStages
	).

/**
 * Specify how the game is run.  Either we perform a certain number of
 * stages and thus compute an exact payoff for the given strategies, or we
 * computed the strategy class that results from a game between the given
 * strategies.

 */
:- type numberStages --->
	% runStages(
	% 	minimum :: maybe(int),
	% 	repeat  :: float,
	% 	maximum :: maybe(int)
	% ) ;
	computeClass
	.

:- instance parseable(game).

:- func default = game.

:- func dialog = list(dialogItem(game)).

:- func bg(game) = float.
:- func cpt(game) = float.
:- func cst(game) = float.

:- func 'bg :='(game, float) = game.
:- func 'cpt :='(game, float) = game.
:- func 'cst :='(game, float) = game.

:- implementation.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

:- instance parseable(game) where
[
	pred(parse/3) is gl.givetake.game.parse
].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

default = game(0.5, 2.5, 1.25, computeClass).

dialog =
	[
	di(label("bonus give"),        updateFieldFloat( bg,  checkFloat( "bg",   bounded(0.0, yes), bounded(1.0, no), 'bg :='))),
	di(label("cost perform take"), updateFieldFloat( cpt, checkFloat( "cpt",  bounded(0.0, yes), unbound, 'cpt :='))),
	di(label("cost subject take"), updateFieldFloat( cst, checkFloat( "cst",  bounded(0.0, yes), unbound, 'cst :=')))
	].

/*
error(Game, Msg) :-
	PredBG =
	(pred(M::out) is semidet :-
		M = "Bonus to give must be greater than or equal to zero and less than one",
		(
			Game^bg < 0.0
			;
			Game^bg >= 1.0
		)
	),
	PredCPT =
	(pred(M::out) is semidet :-
		M = "Cost to perform take action must be greater than or equal to zero",
		Game^cpt < 0.0
	),
	PredCST =
	(pred(M::out) is semidet :-
		M = "Cost payed by take action subject must be greater than or equal to zero",
		Game^cst < 0.0
	),
	PredMinStages =
	(pred(M::out) is semidet :-
		M = "Minimum number of stages must be positive",
		Game^numberStages^minimum = yes(Min),
		Min =< 0
	),
	PredMaxStages =
	(pred(M::out) is semidet :-
		M = "Maximum number of stages must be positive",
		Game^numberStages^maximum = yes(Max),
		Max =< 0
	),
	PredRepeatStages =
	(pred(M::out) is semidet :-
		M = "Probability to repeat one more stage must be between zero and one",
		(
			Game^numberStages^repeat < 0.0
			;
			Game^numberStages^repeat > 1.0
		)
	),
	util.collectMessages([PredBG, PredCPT, PredCST, PredMinStages, PredMaxStages, PredRepeatStages], Msg)
	.
	*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- pred parse(game, list(int), list(int)).
:- mode parse(in, out, in) is det.
:- mode parse(out, in, out) is semidet.

parse(P) -->
	{P = game(BG, CPT, CST, NumberStages)},
	parseable.float32(BG),
	parseable.float32(CPT),
	parseable.float32(CST),
	(
		{NumberStages = computeClass},
		[0]
	)
	.

:- end_module gl.givetake.game.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
