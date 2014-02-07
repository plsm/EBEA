/**
 * 

 * @author Pedro Mariano
 * @version 1.0 2013/12/11
 */
:- module gl.centipede.parameters.

:- interface.

/**
 * Parameters that control the mutation of Centipede strategies.
 */
:- type parameters --->
	parameters(
		timeStdDev        :: float,
		numberStagesCopy  :: int
	).

:- instance parseable(parameters).

/**
 * Return a default Centipede parameters that can be used to run
 * evolutionary games.
  
 */
:- func default = parameters.

:- func init(int, float) = parameters.

:- pred init(int, float, parameters).
:- mode init(in, in, out) is semidet.

:- pred errors(parameters, string).
:- mode errors(in, out) is semidet.

:- func dialog = list(dialogItem(parameters)).

:- func timeStdDev(parameters) = float.

:- func 'timeStdDev :='(parameters, float) = parameters.

:- implementation.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

:- instance parseable(parameters) where
[
	pred(parse/3) is gl.centipede.parameters.parse
].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

default = init(4, 1.0).

init(NumberStagesCopy, TimeStdDev) = Result :-
	(if
		init(NumberStagesCopy, TimeStdDev, R)
	then
		Result = R
	else
		throw("Invalid centipede general parameters")
	).

init(NumberStagesCopy, TimeStdDev, parameters(TimeStdDev, NumberStagesCopy)) :-
	NumberStagesCopy >= 2,
	TimeStdDev >= 0.0
	.

errors(Parameters, Message) :-
	(if
		Parameters^timeStdDev < 0.0
	then
		Msg1 = "standard deviation must be zero or positive"
	else
		Msg1 = ""
	),
	(if
		Parameters^numberStagesCopy < 2
	then
		Msg2 = "number of stages must be greater than or equal to 2"
	else
		Msg2 = ""
	),
	(
		Msg1 \= ""
		;
		Msg2 \= ""
	),
	Message = Msg1 ++ " " ++ Msg2
	.

dialog =
	[
	di(label("standard deviation of guassian distribution used to mutate strategy time"),
		 updateFieldFloat( timeStdDev, userInterface.checkFloat("standard deviation", bounded(0.0, yes), unbound, 'timeStdDev :=')))].
					 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- pred parse(parameters, list(int), list(int)).
:- mode parse(in, out, in) is det.
:- mode parse(out, in, out) is semidet.

parse(P) -->
	{P = parameters(TimeStdDev, NumberStagesCopy)},
	parseable.float32(TimeStdDev),
	[NumberStagesCopy]
	.

:- end_module gl.centipede.parameters.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
