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
shoot g = if dead g || length s >= lShots l then g
  else g {playerShots = n:s }
    where s = playerShots g
          n = fmap (\(V2 x y)  -> V2 x (y + 1)) playership g
          l = level g

-- shoot :: Game -> Game
-- shoot g = if stopped g || length s >= lShots l then g
--   else g {shots = n:s }
--     where s = shots g
--           n = fmap (\(V2 x y)  -> V2 x (y + 1)) canon g
--           l = level g