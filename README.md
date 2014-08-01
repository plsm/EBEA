EBEA
====

Energy Based Evolutionary Algorithm

EBEA is an artificial ecosystem where agents interact with each other,
reproduce and die of several causes.  Interaction is mediated by a game.
Every game has a set of strategies and a set of payoff functions.  Each
agent has a strategy that he uses when he plays the game.  The payoff
function is used as an energy transfer process.  Energy is essential for
reproduction as it only occurs when an agent reaches the reproduction
threshold.  Death events maintain population size.  An agent may die
because of overcrowding, old age or starvation (his energy drops below
zero).

There are several games currently implemented, two-person two-action games,
public good provision, ultimatum, investment, centipede and battle of
sexes.

The application has an interface where you can specify several parameters
(game, initial population, death events, data to record, ...).

There are two interfaces: one text based fully functional, a graphical
interface implemented in Java swing, that is not fully functional.

Instalation Instructions
------------------------

Requirements:

* Java 1.6
* Mercury compiler (http://mercurylang.org/)

You need to install the following components:

* mc4ap  (https://github.com/plsm/mc4ap.git)
* mercury-utils (https://github.com/plsm/mercury-utils.git)


Console Version
---------------

The text based version of EBEA is fully functional.  You can put all the
mercury files of every package (EBEA, mc4ap and mercury-utils) in a single
directory and run the following command to compile

`
mmc -m EBEAtk_console

`
