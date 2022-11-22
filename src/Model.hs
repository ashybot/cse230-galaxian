
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
game ::Int -> Int -> Level -> Game
game s li l = Game
        { lives       = li
        , level       = l
        , score       = s
        , dead        = False
        , playership  = S.singleton (V2 (width `div` 2) 0)
        , enemies     = initEnemy 10 10
        }


-- | Enemy
-- coord: 2D Coordinate of the enemy
-- edead: Been shot or not
-- freq : Number of steps to its next shot - specified by game level
data Enemy = Enemy 
  { coord :: Coord
  , edead :: Bool
  , freq  :: Int
  } deriving (Show, Eq)

-- | Initialize enemies 
-- n: Number of enemies 
-- f: Frequency of shooting
initEnemy:: Int -> Int -> [Enemy]
initEnemy n f = [Enemy (V2 (((width `div` 2) + ((n*2) `div` 2)) - (x*2)) (height - 2)) False f | x <- [1..n]]

-- enemyCoords :: Int -> [Coord]
-- enemyCoords n = [V2 (((width `div` 2) + ((n*2) `div` 2)) - (x*2)) (height - 2) | x <- [1..n]]

-- TODO: move enemies: if hit edges change direction and move forward.
enemyMoves :: Game -> Game
enemyMoves g@Game { enemies = coords } = if null coords
                                          then error "No Enemy!"
                                          else error "Yay"

enemyCoords:: Game -> [Coord]
enemyCoords g = map coord $enemies g


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