/**
 * Provides type class {@code factory}.  Instances of this class must
 * provide a method to produce a collection of values of some type.

 * @author Pedro Mariano
 * @version 1.0 2013/05/23
 */
:- module gfactory.

:- interface.

:- import_module list.

/**
 * This type class represents objects capable of producing a collection of
 * values of type {@code T}.

 * @param F Represents the factory parameters.
  
 * @param T The type of values to be produced.
  
 */
:- typeclass factory(F, G, CS, P) <= (F -> (G, CS, P))
	where
[
	pred value(F, string, G, list({int, CS}), P),
	mode value(in, out, out, out, out) is nondet
].

:- implementation.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- end_module gfactory.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
