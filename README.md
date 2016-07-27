# uni-deskprog-projekt
Mensch-ärger-dich-nicht in ELM
==============================

Es soll Mensch-Ärger-Dich-Nicht in der klassischen Variante
für vier Spieler umsetzen werden. Dabei sollen Spieler 2-4 auch als
(einfacher) Bot angeboten werden.

Travis CI Status [![Build Status](https://travis-ci.org/pgampe/uni-deskprog-projekt.svg?branch=master)](https://travis-ci.org/pgampe/uni-deskprog-projekt)

Current Problems
----------------

 * List of active pieces needs to be updated if a player chooses not to move out on six (easy)
 * Missing collision detection and actually kicking out players (medium)
 * And game positions are properly off and might need some special handling (easy)
 * Concept for automatic player moves (medium)
 * Game board components for interacting (e.g. selecting number of bots) (easy)
 * Concept and implementation of a ranking after end of game (medium)
 
 
Concept
-------

Each player is represented by and offset, starting with zero and adding 10.
The numbering is done clockwise.

All positions on the board are numbered, starting with position one right
where green moves out. There are a total of 40 regular positions.

The end positions are numbered with 41-44 plus the offset of the player.
The out area is is number with 101-104 plus the offset of the player.

The dice is implemented with an own module. It is displayed at the lower
end of the game board and is colored in the players color if he needs to
make roll the dice and in white if has to move a piece.

Only the current players pieces are active and allow clicking and only
if the player is allowed to make a move.

If the player does not have any active pieces, then he must roll the dice
three times or until a six comes up. Upon a six, he can move the next
piece out, starting from the top-left and going clockwise. The player is
always moved out to his starting position labeled with ``A``. From there
he has to make it clockwise around to map.

The player may choose to move any of the currently active pieces.

 
