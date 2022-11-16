# cse230-galaxian

We aim to recreate the Galaxian arcade game using the Haskell [`brick`](https://github.com/jtdaugherty/brick/) library. We will shoot to implement the most basic features of the game at the very least, which are detailed below. If time permits, we will add on to these basics.

## Project Goals:
1. Starting the program should automatically load in the score, amount of lives the player has (3 at start), player-operated spaceship, and enemy Galaxians.
2. The player spaceship is controlled using A/D key to move horizontally along the bottom of the screen and space key to fire lasers.
3. The Galaxians should slowly move across the screen horizontally, switching directions once they reach an edge. They should occasionally decide to attack the player (frequency depends on the level), looping down in the player's direction and firing lasers at the player.
4. If a Galaxian flies towards the player and reaches the bottom of the screen without being killed by the player, they should rejoin the convoy of Galaxians at the top of the screen.
5. When a player's laser collides with an Galaxian, it should destroy it and award the player (20 + (10 * level #)) points to their score.
6. If a Galaxian hits the player with a laser or physically touches the player, the player loses a life.
7. If the player loses all of their lives, game over.
8. If the player destroys all of the Galaxians in a particular round, they win the round and progress to the next level. For each next level, the same amount of Galaxians spawn, but they attack more frequently, with up to 15 Galaxians attacking the player at once in the later rounds.
9. There is no end level, this process continues until the player gets game over.

## Less-crucial additions if time permits:
1. Different types of Galaxians that award a different number of points for being destroyed
