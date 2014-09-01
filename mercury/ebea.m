%% ************************************************************************
%% Root module of EBEA modules.
%%
%% Type parameters used in {@code ebea} submodules:
%%
%% <ul>
%%
%% <li><i>CS</i> @param CS Player's strategy genes and game strategies.</li>
%%
%% <li><i>G</i> The game used by players (values of the payoff matrix, number
%% of players...).</li>
%%
%% <li><i>P</i> Other parameters of the game (mutation operator
%% parameters...).</li>
%%
%% <li><i>AA</i> The game actions accumulator.</li>
%%
%% <li><i>CS</i> Players' strategy genes and game strategies.</li>
%%
%% <li><i>T</i> Player's phenotype that result from strategy genes.</li>
%%
%% <li><i>A</i> Game actions.</li>
%%
%% <li><i>AS</i> The strategy accumulator used to reduce the strategy genes
%% in every iteration.</li>
%%
%% <li><i></i></li>
%%
%% </ul>
%%
%% @author Pedro Mariano
%% @version 1.0 2012/09/18
%%
:- module ebea.

:- interface.

:- include_module player, population, core, streams.

:- implementation.

:- end_module ebea.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
