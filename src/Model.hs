
module Model where
import Linear.V2 (V2(..))

type Name = ()
data Tick = Tick

-- Definition of types of cells in the game
data Cell = EmptyCell | PlayershipCell | EnemyCell | PlayerShotCell | EnemyShotCell
type Coord = V2 Int
type Playership = Coord

-- Definition of Game and Level attributes
data Game = Game
  { lives       :: Int
  , level       :: Level
  , score       :: Int
  , dead        :: Bool
  , playership  :: Playership
  , playerShots :: [Coord] -- shots of player
  , enemies     :: Enemies
  , enemiesShots:: [Coord]
  , curstep     :: Int -- step of the game
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
game s li l@(Level _ af sf) = Game
        { lives        = li
        , level        = l
        , score        = s
        , dead         = False
        , playership   = V2 (width `div` 2) 0
        , playerShots        = []
        , enemies      = initEnemies 10 af L sf
        , enemiesShots = []
        , curstep = 100000000
        }

-- countdown   : Steps to the next attack - specified by game level
-- origPosition: (Only for reference) Original position of actual enemies
-- attackEnemy : The actual attack enemies 
data Enemies = Enemies {
    enemyList :: [Enemy]
  , countdown :: Int
  , origPosition :: [Enemy]
  , attackEnemy  :: [Enemy]
  , attackFreq   :: Int
} deriving (Show, Eq)

-- | Enemy
-- coord: 2D Coordinate of the enemy
-- edead: Been shot or not
-- direc: Current moving direction
data Enemy
  = E
    { coord :: Coord
    , edead :: Bool
    , direc :: Direction
    }
  deriving (Show, Eq)

instance Ord Enemy where
  (< ) (E c1 _ _) (E c2 _ _) = c1 <  c2
  (<=) (E c1 _ _) (E c2 _ _) = c1 <= c2
  (> ) (E c1 _ _) (E c2 _ _) = c1 >  c2
  (>=) (E c1 _ _) (E c2 _ _) = c1 >= c2

-- | Initialize enemies 
-- n: Number of enemies 
-- f: Countdown of next shooting with default set to game$level$attackFrequency
-- d: Direction
initEnemies:: Int -> Int -> Direction -> Int -> Enemies
initEnemies n f d sf = Enemies (initEnemyList n d) f [] [] sf
initEnemyList:: Int -> Direction -> [Enemy]
initEnemyList n d = [E (V2 (((width `div` 2) + ((n*2) `div` 2)) - (x*2)) enemyHeight) False d | x <- [1..n]]

setAttackFrequency :: Int -> Int
setAttackFrequency = (100-)

-- Move enemy according to directions
moveEnemy :: Direction -> Enemy -> Enemy
moveEnemy L (E (V2 x y) e _) = E (V2 (x-1) y) e L
moveEnemy R (E (V2 x y) e _) = E (V2 (x+1) y) e R
moveEnemy D (E (V2 x y) e d) = E (V2 x (y-1)) e d
moveEnemy U (E (V2 x y) e d) = E (V2 x (y+1)) e d

enemyCoords:: Game -> [Coord]
enemyCoords (Game _ _ _ _ _ _ (Enemies el _ _ ae _) _ _) = map coord (el++ae)

updateShots :: [Coord] -> Direction -> [Coord]
updateShots s U = map (\(V2 x y) -> (V2 x (y+1))) s 
updateShots s D = map (\(V2 x y) -> (V2 x (y-1))) s 
updateShots s L = map (\(V2 x y) -> (V2 (x-1) y)) s 
updateShots s R = map (\(V2 x y) -> (V2 (x+1) y)) s 


getCoord :: Enemy -> Coord
getCoord (E c _ _) = c

getCoord2 :: Enemy -> Coord
getCoord2 (E (V2 x y) _ _) = V2 x (y+1)

getCoord3 :: Enemy -> Coord
getCoord3 (E (V2 x y) _ _) = V2 x (y-1)

getEnemyLocationList :: Game -> [Coord]
getEnemyLocationList (Game _ _ _ _ _ _ (Enemies el _ _ _ _) _ _ ) = map getCoord el

getAEnemyLocationList :: Game -> [Coord]
getAEnemyLocationList (Game _ _ _ _ _ _ (Enemies _  _ _ ae _) _ _) = (map getCoord ae) ++ (map getCoord2 ae) ++ (map getCoord3 ae)



initGame :: IO Game
initGame = do
            let l = getLevel 0
            return $game 0 3 l

getLevel :: Int -> Level
getLevel n = Level {levelNumber = n, attackFrequency = 30 - 5*n, lShots = 3}

-- Initialize screen size
height, width :: Int
-- height = 15
-- width = 35
height = 20
width = 35

-- Initialize enemy position
enemyHeight :: Int
enemyHeight = height - 2