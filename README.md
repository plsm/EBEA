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

Compilation Instructions
------------------------

Requirements:

* Java 1.6
* Mercury compiler (http://mercurylang.org/)

You need to install the following components:

* mc4ap  (https://github.com/plsm/mc4ap.git)
* mercury-utils (https://github.com/plsm/mercury-utils.git)

Run the following commands

	 ./configure lib_mc4ap=DIR1 lib_mercury_utils=DIR2
	 make

DIR1 and DIR2 should be replaced by the directories where the components
mc4ap and mercury-utils were installed.  If you don't specify these
options, the components are searched in the directory where the mercury
compiler was installed.


