/**
 * Provides the type class that represents a generic chromosome.

 * @author Pedro Mariano
 * @version 1.0 2012/06/23
 */
:- module chromosome.

:- interface.

:- import_module rng, rng.distribution.

/**
 * This type class represents a generic chromosome.  It provides generic
 * functionality such as querying the number of genes, mutating a single
 * gene, and initialise the phenotypic traits given a chromosome.

 * <p> Mutation of more than one gene can be implemented using the function
 * that returns the number of genes and the predicate that mutates a single
 * gene.  This predicate receives the gene index to mutate.

 * <p> Initialisation of phenotypic traits corresponds to the birth
 * process.  Besides the chromosome, there
 
 * @param C the type that represents the chromosome genes

 * @param T the type that represents the phenotypic traits.

 * @param P the type that represents the parameters used in the mutation
 * predication and the born function.
 */

:- typeclass chromosome(C, T, P) <= (C -> (T, P)) where
	[
	/**
	 * numberGenes(Chromosome) = Result
	 
	 * Return the number of genes in the given chromosome.  This is used to
	 * mutate a gene or to print a single gene.
	 */
	func numberGenes(C) = int,

	/**
	 * mutateGene(Parameter, Index, !Distribution, !Random, !Chromosome)

	 * Mutate a single gene of the given chromosome.
	 */
	pred mutateGene(P, int, distribution, distribution, R, R, C, C) <= ePRNG(R),
	mode mutateGene(in, in, in, out, in, out, in, out) is det,

	/**
	 * born(Parameters, Chromosome) = Result
	 
	 * Return the offspring that this chromosome can develop.  This function
	 * initialises the phenotypic traits given the parameters and the
	 * chromosome.
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
