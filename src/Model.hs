
module Model where
import Linear.V2 (V2(..))
import Data.Sequence (Seq(..))
import qualified Data.Sequence as S

type Name = ()
data Tick = Tick

-- Definition of types of cells in the game
data Cell = EmptyCell | PlayershipCell | EnemyCell
type Coord = V2 Int
type Playership = Seq Coord
type Enemy = Seq Coord

-- Definition of Game and Level attributes
data Game = Game
  { lives       :: Int
  , level       :: Level
  , score       :: Int
  , dead        :: Bool
  , playership  :: Playership
  , enemies     :: [Enemy]
  } deriving (Show)

data Level = Level
  { levelNumber :: Int
  , attackFrequency :: Int
  } deriving (Show)

-- | Initialize the game with the default values
game ::Int -> Int -> Level ->  Game
game s li l = Game
        { lives       = li
        , level       = l
        , score       = s
        , dead        = False
        , playership  = S.singleton (V2 (width `div` 2) 0)
        , enemies = map S.singleton (enemyCoords 10)
        }
enemyCoords :: Int -> [Coord]
enemyCoords n = [V2 (((width `div` 2) + ((n*2) `div` 2)) - (x*2)) (height - 2) | x <- [1..n]]

initGame :: IO Game
initGame = do
            let l = getLevel 0
            return $game 0 3 l

getLevel :: Int -> Level
getLevel n = Level {levelNumber = n, attackFrequency = 2*n}

-- Initialize screen size
height, width :: Int
height = 15
width = 35