/**
 * 

 * @author Pedro Mariano
 * @version 1.0 2013/06/25
 */
:- module ebea.population.players.

:- interface.

:- import_module ebea.player.
:- import_module hash_table, io.

:- type players(C, T) == hash_table(int, player(C, T)).

:- pred debug(io.state, io.state).
:- mode debug(di, uo) is det.

:- implementation.

:- import_module unit.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

debug(!IO) :-
	hash_table.init(hash_table.string_hash, 8, 9000.0) = Table,
	fill(Table, TableFill),
	io.write(TableFill, !IO),
	io.nl(!IO).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- pred hashPlayer(player(_, _), int).
:- mode hashPlayer(in, out) is det.

hashPlayer(Player, Player^id).

:- pred fill(hash_table(string, unit), hash_table(string, unit)).
:- mode fill(hash_table_di, hash_table_uo) is det.

fill(!Table) :-
	hash_table.set("Pedro",   unit, !Table),
	hash_table.set("Mariano", unit, !Table),
	hash_table.set("Olá",     unit, !Table),
	hash_table.set("Mundo",   unit, !Table).

:- end_module ebea.population.players.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
