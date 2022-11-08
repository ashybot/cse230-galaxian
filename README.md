# cse230-space-invaders

We aim to recreate the Space Invaders arcade game using the Haskell [`brick`](https://github.com/jtdaugherty/brick/) library. We will shoot to implement the most basic features of the game at the very least, which are detailed below. If time permits, we will add on to these basics.

## Project Goals:
1. Starting the program should automatically load in the score, amount of lives the player has (3 at start), player-operated spaceship, 4 shield objects, and aliens.
2. The player spaceship should be able to move horizontally along the bottom of the screen and fire missiles.
3. The aliens should move down the screen in a snake-like way, first moving horizontally across the screen and then downwards towards the player. The aliens may also throw bombs at the player.
4. When a player's missile collides with an alien, it should destroy it and award the player (level # * 10) points to their score.
5. If an alien hits the player with a bomb, the player loses a life.
6. The 4 shield object respawn/repair every level. They can be damaged by both the player's missiles and the aliens' bombs.
7. If the aliens reach the bottom of the screen, game over (even if the player still has lives).
8. If the player loses all of their lives, game over.
9. If the player destroys all of the aliens, they win the round and progress to the next level. For each next level, the same amount of aliens spawn but they start closer to the player (exact amount closer undecided still).
10. There is no end level, this process continues until the player gets game over.
