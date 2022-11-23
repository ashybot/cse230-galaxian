
module Model where
import Linear.V2 (V2(..))

type Name = ()
data Tick = Tick

-- Definition of types of cells in the game
data Cell = EmptyCell | PlayershipCell | EnemyCell | ShotCell
type Coord = V2 Int
type Playership = Coord

-- Definition of Game and Level attributes
data Game = Game
  { lives       :: Int
  , level       :: Level
  , score       :: Int
  , dead        :: Bool
  , playership  :: Playership
  , shots       :: [Coord] -- shots of player
  , enemies     :: [Enemy]
  } deriving (Show)

data Level = Level
  { levelNumber :: Int
  , attackFrequency :: Int
  , lShots  :: Int
  } deriving (Show)

-- Left, Right, Up, Down
data Direction = L | R | U | D
  deriving (Show, Eq)

-- | Initialize the game with the default values
game ::Int -> Int -> Level -> Game
game s li l = Game
        { lives       = li
        , level       = l
        , score       = s
        , dead        = False
        , playership  = V2 (width `div` 2) 0
        , shots       = []
        , enemies     = initEnemy 10 10 L
        }

-- | Enemy
-- coord: 2D Coordinate of the enemy
-- edead: Been shot or not
-- freq : Number of steps to its next shot - specified by game level
-- direc: Current moving direction
data Enemy = Enemy 
  { coord :: Coord
  , edead :: Bool
  , freq  :: Int
  , direc :: Direction
  } deriving (Show, Eq)

instance Ord Enemy where 
  (< ) (Enemy c1 _ _ _) (Enemy c2 _ _ _) = c1 <  c2 
  (<=) (Enemy c1 _ _ _) (Enemy c2 _ _ _) = c1 <= c2 
  (> ) (Enemy c1 _ _ _) (Enemy c2 _ _ _) = c1 >  c2 
  (>=) (Enemy c1 _ _ _) (Enemy c2 _ _ _) = c1 >= c2 

-- | Initialize enemies 
-- n: Number of enemies 
-- f: Frequency of shooting
initEnemy:: Int -> Int -> Direction -> [Enemy]
initEnemy n f d = [Enemy (V2 (((width `div` 2) + ((n*2) `div` 2)) - (x*2)) (height - 2)) False f d | x <- [1..n]]

-- TODO: Shoot and being shot.
updateEnemy :: Game -> [Enemy]
updateEnemy (Game _ _ _ _ _ _ e) = if null e
                                then error "No Enemy!"
                                else updateEnemyMove e

updateEnemyMove :: [Enemy] -> [Enemy]
updateEnemyMove e = do
                    let (Enemy (V2 lx _) _ _ _) = minimum e
                    let (Enemy (V2 rx _) _ _ _) = maximum e
                    let       Enemy _ _ _ d = head e
                    if lx == 1
                      then mvEnemies R (mvEnemies D e) 
                      else if rx == width-1
                        then mvEnemies L (mvEnemies D e) 
                        else mvEnemies d e 
                    where mvEnemies d = map (moveEnemy d)

-- Move enemy according to directions
moveEnemy :: Direction -> Enemy -> Enemy
moveEnemy L (Enemy (V2 x y) e f _) = Enemy (V2 (x-1) y) e f L
moveEnemy R (Enemy (V2 x y) e f _) = Enemy (V2 (x+1) y) e f R
moveEnemy D (Enemy (V2 x y) e f d) = Enemy (V2 x (y-1)) e f d
moveEnemy U (Enemy (V2 x y) e f d) = Enemy (V2 x (y+1)) e f d

enemyCoords:: Game -> [Coord]
enemyCoords g = map coord $enemies g

updateShots :: Game -> [Coord]
updateShots (Game _ _ _ _ _ s _) = map (\(V2 x y) -> (V2 x (y+1))) s


initGame :: IO Game
initGame = do
            let l = getLevel 0
            return $game 0 3 l

getLevel :: Int -> Level
getLevel n = Level {levelNumber = n, attackFrequency = 2*n, lShots = 100000}

-- Initialize screen size
height, width :: Int
height = 15
width = 35