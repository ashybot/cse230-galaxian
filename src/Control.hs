module Control where

import Model
import Linear.V2 (V2(..), _x, _y)
import Control.Lens ((^.))


move :: (Int -> Int) -> Game -> Game
move f g  = if dead g then g
            else g {playership = V2 x $playership g ^._y }
                where x = f(playership g ^._x) `mod` width

-- | Add new shot from the canon to the game
shoot :: Game -> Game
-- shoot g = if dead g || length s >= lShots l then g
-- shoot g = if dead g || length s >= lShots l || (curstep g) `mod` 4 /= 0 then g
shoot g = if dead g || length s >= lShots l || (curstep g) < 8 then g


--   else g {playerShots = n:s }
  else g {playerShots = n:s, curstep=0}
    where s = playerShots g
          n = fmap (\(V2 x y)  -> V2 x (y + 1)) playership g
          l = level g
