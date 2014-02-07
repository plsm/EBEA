/**
 * 

 * @author Pedro Mariano
 * @version 1.0 2013/12/13
 */
:- module gl.'pgp+pa'.parameters.

:- interface.

/**
 * Parameters used to control mutation of a PGP with punishment and abstaining strategy.
 */
:- type parameters --->
	noParameters.

:- instance parseable(parameters).

:- func default = parameters.

:- func dialog = list(dialogItem(parameters)).

:- implementation.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

:- instance parseable(parameters) where
[
	pred(parse/3) is gl.'pgp+pa'.parameters.parse
].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

default = noParameters.

dialog = [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- pred parse(parameters, list(int), list(int)).
:- mode parse(in, out, in) is det.
:- mode parse(out, in, out) is semidet.

parse(P) -->
	{P = noParameters},
	[0]
	.

:- end_module gl.'pgp+pa'.parameters.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
