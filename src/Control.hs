module Control where

import Model
import Linear.V2 (V2(..), _x, _y)
import Control.Lens ((^.))


move :: (Int -> Int) -> Game -> Game
move f g  = if dead g then g
            else g {playership = V2 x $playership g ^._y }
                where x = f(playership g ^._x) `mod` width