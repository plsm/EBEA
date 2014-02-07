/**
 * Provides the type class that represents a generic chromosome.

 * @author Pedro Mariano
 * @version 1.0 2012/06/23
 */
:- module chromosome.

:- interface.

:- import_module rng, rng.distribution.

:- typeclass chromosome(C, T, P) <= (C -> (T, P)) where
	[
	/**
	 * numberGenes(Chromosome) = Result
	 
	 * Return the number of genes in the given chromosome.  This is used to
	 * mutate a gene or to print a single gene.
	 */
	func numberGenes(C) = int,

	/**
	 * mutateGene(Index, !Distribution, !Random, !Chromosome)

	 * Mutate a single gene of the given chromosome.
	 */
	pred mutateGene(P, int, distribution, distribution, R, R, C, C) <= ePRNG(R),
	mode mutateGene(in, in, in, out, in, out, in, out) is det,

	/**
	 * Return the offspring that this chromosome can develop.
	 */
	func born(P, C) = T
].

:- typeclass rndChromosome(C, T, P) <= chromosome.chromosome(C, T, P) where
[
	pred born(P, C, T, R, R) <= ePRNG(R),
	mode born(in, in, out, in, out) is det
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

:- end_module chromosome.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
