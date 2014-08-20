/**
 * This module provides the definition of a player's chromosome.  It is
 * composed of a collection of sets of genes.  One gene collection deals
 * with a player's ageing process.  Another deals with a player's energy
 * flow process: how much energy is needed to reproduce.  A third
 * collection deals with the partner selection process: how other players
 * are selected to play a game.  The last collection represents the
 * strategy used in a game.

 * @author Pedro Mariano
 * @version 1.0 2014/03/22
 */
:- module ebea.player.chromosome.

:- interface.


/**
 * The chromosome of an EBEA player.  It is represented in a list type
 * structure.  The first node contains the genes related to the energy
 * process.  The second node contains the genes related to partner
 * selection process.  The last node contains the genes that control the
 * behaviour in the game being used.  Type parameter {@code C} represents
 * the game strategy.

 * @param S the type that represents the strategy genes.
 */
:- type chromosome(S) --->
	chromosome(
		ageGenes       :: ebea.player.age.chromosome,
		energyGenes    :: ebea.player.energy.chromosome,
		selectionGenes :: ebea.player.selection.chromosome.chromosome,
		strategyGenes  :: S
	).

/**
 * Return a default chromosome given the default strategy.
 */
:- func default(S) = chromosome(S).

/**
 * Return a dialog to edit a player's chromosome given a dialog to edit the
 * strategy gene collection.
  
 */
:- func dialog(list(dialogItem(CS))) = list(dialogItem(ebea.player.chromosome.chromosome(CS))).

/**
 * Update the accumulator with the new chromosome info.
 */
:- func fold(chromosome(S), ac(A)) = ac(A)
	<= foldable(S, A).

:- pred print(io.output_stream, chromosome(S), io, io)
	<= printable(S).
:- mode print(in, in, di, uo) is det.

:- pred parse(chromosome(S), parseable.state, parseable.state)
	<= parseable(S).
:- mode parse(in, out, in) is det.
:- mode parse(out, in, out) is semidet.

:- implementation.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

default(StrategyChromosome) =
	chromosome(
		ebea.player.age.defaultChromosome,
		ebea.player.energy.defaultChromosome,
		ebea.player.selection.chromosome.default,
		StrategyChromosome
	).


dialog(DialogStrategyGenes) =
	[
%	di(label("age genes"),        'new editField'(  get_ageGenes,        set(set_ageGenes),       ebea.player.age.dialogChromosome)),
%	di(label("energy genes"),     'new editField'(  get_energyGenes,     set(set_energyGenes),    ebea.player.energy.dialogChromosome)),
%	di(label("selection genes"),  'new editField'(  get_selectionGenes,  set(set_selectionGenes), [di(label("selection"), ebea.player.selection.dialogChromosome)])),
	di(label("selection genes"),  'new editField'(  get_selectionGenes,  set(set_selectionGenes), ebea.player.selection.chromosome.dialog)),
	di(label("strategy genes"),   'new editField'(  get_strategyGenes,   set(set_strategyGenes),  DialogStrategyGenes))
	].

fold(Chromosome, AC) = Result :-
	AC = ac(A, E, Se, St),
	Result = ac(
		foldable.fold(Chromosome^ageGenes, A),
		foldable.fold(Chromosome^energyGenes, E),
		foldable.fold(Chromosome^selectionGenes, Se),
		foldable.fold(Chromosome^strategyGenes, St)
	).

print(Stream, Chromosome, !IO) :-
	printable.print(Stream, Chromosome^energyGenes, !IO),
	io.print(Stream, " ", !IO),
	printable.print(Stream, Chromosome^selectionGenes, !IO),
	io.print(Stream, " ", !IO),
	printable.print(Stream, Chromosome^strategyGenes, !IO).

parse(Chromosome) -->
	ebea.player.age.parseChromosome(Chromosome^ageGenes),
	ebea.player.energy.parseChromosome(Chromosome^energyGenes),
	ebea.player.selection.chromosome.parse(Chromosome^selectionGenes),
	parseable.parse(Chromosome^strategyGenes).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions
:- func get_ageGenes(ebea.player.chromosome.chromosome(CS)) = ebea.player.age.chromosome.

get_ageGenes(P) = P^ageGenes.


:- func set_ageGenes(ebea.player.chromosome.chromosome(CS), ebea.player.age.chromosome) = ebea.player.chromosome.chromosome(CS).

set_ageGenes(P, V) = 'ageGenes :='(P, V).



:- func get_energyGenes(ebea.player.chromosome.chromosome(CS)) = ebea.player.energy.chromosome.

get_energyGenes(P) = P^energyGenes.


:- func set_energyGenes(ebea.player.chromosome.chromosome(CS), ebea.player.energy.chromosome) = ebea.player.chromosome.chromosome(CS).

set_energyGenes(P, V) = 'energyGenes :='(P, V).



:- func get_selectionGenes(ebea.player.chromosome.chromosome(CS)) = ebea.player.selection.chromosome.chromosome.

get_selectionGenes(P) = P^selectionGenes.


:- func set_selectionGenes(ebea.player.chromosome.chromosome(CS), ebea.player.selection.chromosome.chromosome) = ebea.player.chromosome.chromosome(CS).

set_selectionGenes(P, V) = 'selectionGenes :='(P, V).



:- func get_strategyGenes(ebea.player.chromosome.chromosome(CS)) = CS.

get_strategyGenes(P) = P^strategyGenes.


:- func set_strategyGenes(ebea.player.chromosome.chromosome(CS), CS) = ebea.player.chromosome.chromosome(CS).

set_strategyGenes(P, V) = 'strategyGenes :='(P, V).

:- end_module ebea.player.chromosome.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
