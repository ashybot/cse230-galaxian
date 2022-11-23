
module Model where
import Linear.V2 (V2(..))

type Name = ()
data Tick = Tick

-- Definition of types of cells in the game
data Cell = EmptyCell | PlayershipCell | EnemyCell
type Coord = V2 Int
type Playership = Coord

-- Definition of Game and Level attributes
data Game = Game
  { lives       :: Int
  , level       :: Level
  , score       :: Int
  , dead        :: Bool
  , playership  :: Playership
  , enemies     :: Enemies
  } deriving (Show)

data Level = Level
  { levelNumber :: Int
  , attackFrequency :: Int
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
        , enemies     = initEnemies 10 10 L
        }

-- countdown   : Steps to the next attack - specified by game level
-- origPosition: (Only for reference) Original position of actual enemies
-- attackEnemy : The actual attack enemies 
data Enemies = Enemies {
    enemyList :: [Enemy]
  , countdown :: Int
  , origPosition :: [Enemy]
  , attackEnemy  :: [Enemy]
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
initEnemies:: Int -> Int -> Direction -> Enemies
initEnemies n f d = Enemies (initEnemyList n d) f [] []
initEnemyList:: Int -> Direction -> [Enemy]
initEnemyList n d = [E (V2 (((width `div` 2) + ((n*2) `div` 2)) - (x*2)) enemyHeight) False d | x <- [1..n]]

setAttackFrequency :: Int -> Int 
setAttackFrequency = (100-)

-- TODO: 1. Shoot and being shot.
--       2. Attack frequently
updateEnemy :: Game -> Enemies
updateEnemy (Game _ (Level _ af) _ _ (V2 px _) es@(Enemies el f op ae)) = if null (el++ae)
                                          then error "No Enemy!"
                                          else do 
                                            let f'  = updateFrequency f af
                                            -- update patched positions 
                                            let op' = updateEnemyMove op (el++op)
                                            let el' = updateEnemyMove el (el++op)
                                            -- pick new attack enemy
                                            let es' = pickNewAttackEnemy (Enemies el' f' op' ae)
                                            -- attack enemy moves
                                            let es''= updateAttackMove px es' 
                                            -- return finished enemy
                                            returnFinishedAttack es''

-- Pick new attack enemy
pickNewAttackEnemy :: Enemies -> Enemies
pickNewAttackEnemy es@(Enemies el f op ae)
  | f /= 0 || null el = es
  | otherwise = do 
      let e_ = head el
      let el'= tail el
      let op'= e_:op 
      let ae' = e_:ae
      Enemies el' f op' ae'

-- Attack enemies move
updateAttackMove :: Int -> Enemies -> Enemies
updateAttackMove px es@(Enemies el f op ae)
  = do 
    -- put back attack enemy that arrives original patch
    let idx = tryGetAttackEnemyByY ae 0 (enemyHeight+1)  
    let (Enemies el' _ op' ae') = putBackAttackEnemy idx es
    Enemies el' f op' (map (horizontalMove . moveEnemy D) ae')
    where 
      horizontalMove e@(E (V2 ex ey) _ _) = do 
        -- on the way back to original position
        if ey-1 > enemyHeight
          then moveEnemy D e 
            -- on the way attacking player
            else if ex < px
              then moveEnemy R e 
              else if ex > px 
                then moveEnemy L e 
                else e 
  
-- Put back attack enemy to original position
putBackAttackEnemy :: Int -> Enemies -> Enemies
putBackAttackEnemy idx es@(Enemies el f op ae)
  = if idx == -1 then es 
      else do
        let op_ = op!!idx
        let el' = el ++ [op_]
        let ae' = removeEnemy idx ae 
        let op' = removeEnemy idx op 
        Enemies el' f op' ae'

-- Return the index of attack enemy at the bottom else -1
tryGetAttackEnemyByY :: [Enemy] -> Int -> Int -> Int
tryGetAttackEnemyByY [] _ _ = -1
tryGetAttackEnemyByY (a@(E (V2 _ y) _ _): as) idx y_val
  = do
    if y == y_val
      then idx
      else tryGetAttackEnemyByY as (idx+1) y_val

-- Remove enemy from list by index 
removeEnemy :: Int -> [Enemy] -> [Enemy]
removeEnemy idx el = lhs ++ rhs
  where (lhs, _ :rhs) = splitAt idx el

-- Return the finished enemies back to original position
returnFinishedAttack :: Enemies -> Enemies
returnFinishedAttack es@(Enemies el f op ae)
  = do
    let idx = tryGetAttackEnemyByY ae 0 1
    if idx == -1 
      then es
      else do 
        let (E (V2 x y) e d) = ae!!idx
        let ae' = E (V2 x height) e d:ae
        Enemies el f op ae'




-- Update attack frequency
updateFrequency :: Int -> Int -> Int 
updateFrequency currFreq freq
  | currFreq > 0     = currFreq - 1 
  | otherwise = freq

-- Update e1 according to e2 boundary enemies
updateEnemyMove :: [Enemy] -> [Enemy] -> [Enemy]
updateEnemyMove e1 e2 = do
                    let (E (V2 lx _) _ _) = minimum e2
                    let (E (V2 rx _) _ _) = maximum e2
                    let  E _ _ d          = head e2
                    if lx == 1
                      then mvEnemies R e1
                    else if rx == width-1
                      then mvEnemies L e1
                      else mvEnemies d e1
                    where mvEnemies d = map (moveEnemy d)

-- Move enemy according to directions
moveEnemy :: Direction -> Enemy -> Enemy
moveEnemy L (E (V2 x y) e _) = E (V2 (x-1) y) e L
moveEnemy R (E (V2 x y) e _) = E (V2 (x+1) y) e R
moveEnemy D (E (V2 x y) e d) = E (V2 x (y-1)) e d
moveEnemy U (E (V2 x y) e d) = E (V2 x (y+1)) e d

enemyCoords:: Game -> [Coord]
enemyCoords (Game _ _ _ _ _ (Enemies el _ _ ae)) = map coord (el++ae)

initGame :: IO Game
initGame = do
            let l = getLevel 0
            return $game 0 3 l

getLevel :: Int -> Level
getLevel n = Level {levelNumber = n, attackFrequency = 30 - 10*n}

-- Initialize screen size
height, width :: Int
height = 15
width = 35

-- Initialize enemy position
enemyHeight :: Int 
enemyHeight = height - 2