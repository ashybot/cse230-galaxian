module Control where

import Model
import Linear.V2 (V2(..), _x, _y)
import Control.Lens ((^.))


move :: (Int -> Int) -> Game -> Game
move f g  = if dead g then g
            else g {playership = V2 x $playership g ^._y }
                where x = f(playership g ^._x) `mod` width

-- | Restart the game
restart :: Game -> Game
restart _ = game 0 3 (getLevel 0)

-- | Add new shot from the spaceship to the game
shoot :: Game -> Game
-- shoot g = if dead g || length s >= lShots l then g
-- shoot g = if dead g || length s >= lShots l || (curstep g) `mod` 4 /= 0 then g
shoot g = if dead g || length s >= lShots l || (curstep g) < 8 then g


--   else g {playerShots = n:s }
  else g {playerShots = n:s, curstep=0}
    where s = playerShots g
          n = fmap (\(V2 x y)  -> V2 x (y + 1)) playership g
          l = level g

updateScore :: Level -> Int -> Enemies -> Enemies -> Int
updateScore lev score1 olde newe = do
                                let old_el = enemyList olde
                                let old_ae = attackEnemy olde
                                let new_el = enemyList newe
                                let new_ae = attackEnemy newe
                                let sum1 = (length old_el) - (length new_el) + (length old_ae) - (length new_ae)
                                score1 + sum1 * (20 + (10 * (levelNumber lev)) )

updateLives :: Game -> Coord -> [Coord] -> [Enemy] -> Int
updateLives g player enemyLasers listAttackingEnemies = if (player `elem` enemyLasers) || (player `elem` map coord listAttackingEnemies)
                                                then lives g - 1
                                                else lives g