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


-- TODO: 1. Shoot and being shot.
--       2. Attack frequently
-- updateEnemy :: Game -> Enemies
updateEnemy :: Game -> Enemies
updateEnemy (Game _ (Level _ af sf) _ _ (V2 px _) _ (Enemies el f op ae s) _ _) = if null (el++ae)
                              
                                          then error "No Enemy!"
                                          else do
                                            -- update both frequency counter
                                            let f'  = updateFrequency f af
                                            let s'  = updateFrequency s sf
                                            -- update patched positions 
                                            let op' = updateEnemyMove op (el++op)
                                            let el' = updateEnemyMove el (el++op)
                                            -- pick new attack enemy
                                            let es' = pickNewAttackEnemy (Enemies el' f' op' ae s')
                                            -- attack enemy moves
                                            let es''= updateAttackMove px es'
                                            -- return finished enemy
                                            returnFinishedAttack es''

updateEnemyAfterShots :: Enemies -> [Coord] -> Enemies
updateEnemyAfterShots es@(Enemies el _ _ ae sf) shotsNew = if null (el++ae)
                                    then error "No Enemy!"
                                    else do
                                      let el_ = moveAndKill (enemyList es) shotsNew
                                      -- let ae_ = moveAndKill (attackEnemy es) shotsNew
                                      let ae_ = moveAndKill2 (attackEnemy es) shotsNew
                                      let es' = Enemies el_ (countdown es) (origPosition es) ae_ sf
                                      es'


-- attack enemies shot
attackEnemyNewShot :: Game -> [Coord]
attackEnemyNewShot (Game _ _ _ _ _ _ (Enemies _ _ _ ae s) esh _)
  = do
    if s /= 0 then esh
      else esh ++ map getCoord3 ae

-- coord :: Coord
--     , edead :: Bool
--     , direc :: Direction
moveAndKill :: [Enemy] -> [Coord] -> [Enemy]
-- moveAndKill a s = [x | x <- a', 0 /= hits x] -- remove dead aliens 
moveAndKill a s = [x | x <- a', True /= edead x] -- remove dead aliens 
      where a' = map (\(E coord' edead' dir) -> if coord' `elem` s then (E coord' True dir) else (E coord' edead' dir)) a  -- check for hits


getC :: [Coord] -> [Coord]
getC c = c ++ (map(\(V2 x y) -> (V2 x (y+1))) c) ++ (map(\(V2 x y) -> (V2 x (y-1))) c)

moveAndKill2 :: [Enemy] -> [Coord] -> [Enemy]
-- moveAndKill a s = [x | x <- a', 0 /= hits x] -- remove dead aliens 
moveAndKill2 a s = [x | x <- a', True /= edead x] -- remove dead aliens 
      where a' = map (\(E coord' edead' dir) -> if coord' `elem` (getC s) then (E coord' True dir) else (E coord' edead' dir)) a  -- check for hits


      -- where a' = map (\(E (V2 x_ y_) edead dir) -> if (V2 x_ (y)) `elem` s then (E (V2 x_ y_) True dir) else (E (V2 x_ y_) edead dir)) a  -- check for hits

-- Pick new attack enemy
pickNewAttackEnemy :: Enemies -> Enemies
pickNewAttackEnemy es@(Enemies el f op ae sf)
  | f /= 0 || null el = es
  | otherwise = do
      let e_ = head el
      let el'= tail el
      let op'= e_:op
      let ae' = e_:ae
      Enemies el' f op' ae' sf

-- Attack enemies move
updateAttackMove :: Int -> Enemies -> Enemies
updateAttackMove px es@(Enemies _ f _ ae sf)
  = do
    -- put back returning attack enemy that arrives original patch
    let idx = tryGetAttackEnemyByY ae 0 (enemyHeight+1)
    let (Enemies el' _ op' ae' _) = putBackAttackEnemy idx es
    -- update movement
    Enemies el' f op' (map (horizontalMove . moveEnemy D) ae') sf
    where
      horizontalMove e@(E (V2 ex ey) _ _) = do
        -- on the way returning to original position
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
putBackAttackEnemy idx es@(Enemies el f op ae sf)
  = if idx == -1 then es
      else do
        let op_ = op!!idx
        let el' = el ++ [op_]
        let ae' = removeEnemy idx ae
        let op' = removeEnemy idx op
        Enemies el' f op' ae' sf

-- Return the index of attack enemy at the bottom else -1
tryGetAttackEnemyByY :: [Enemy] -> Int -> Int -> Int
tryGetAttackEnemyByY [] _ _ = -1
tryGetAttackEnemyByY ((E (V2 _ y) _ _): as) idx y_val
  = do
    if y == y_val
      then idx
      else tryGetAttackEnemyByY as (idx+1) y_val

-- Remove enemy from list by index 
removeEnemy :: Int -> [Enemy] -> [Enemy]
removeEnemy idx el = lhs ++ rhs
  where (lhs, _ :rhs) = splitAt idx el

-- Return the bottom-reached enemy back to the top
returnFinishedAttack :: Enemies -> Enemies
returnFinishedAttack es@(Enemies el f op ae sf)
  = do
    let idx = tryGetAttackEnemyByY ae 0 1
    if idx == -1
      then es
      else do
        let (E (V2 x _) e d) = ae!!idx
        let ae' = E (V2 x height) e d:ae
        Enemies el f op ae' sf




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


handleShots :: Game ->  [Coord] -> [Coord]
handleShots g s =  do
      let s1 = [x | x <- s, not (x `elem` getEnemyLocationList g) && not (x `elem` getAEnemyLocationList g)] -- remove shots which hit
      [(V2 x y) | (V2 x y)  <- s1, y <= height] -- remove shots which are out                                            