/**
 * This module contains all the modules that implement the tools in the
 * Energy Based Evolutionary Algorithm Toolkit.
 * 

 * @author Pedro Mariano
 * @version 1.0 2014/03/06
 */
:- module tools.

:- interface.

%:- include_module export_playerProfiles_gefx.
:- include_module export_playerProfiles_graphviz.
:- include_module 'PCVNetwork'.
:- include_module populationDynamics.
:- include_module processPlayerProfile.
:- include_module processPhenotype.
:- include_module utils.

:- implementation.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- end_module tools.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
