module Main (main) where
import Model
import Control
import Test.Tasty.HUnit ( testCase, (@?=) )
import Linear.V2 (V2(..))
import Test.QuickCheck 
import Test.Tasty

import Data.List

main :: IO ()
-- main = defaultMain unitTests
main = do {quickCheck prop_e; defaultMain unitTests}
        -- defaultMain unitTests
        -- quickCheck prop_e

unitTests:: TestTree
unitTests = testGroup "Tests" [updateLivesTest, killEnemyTest, updateScoreTest]

updateLivesTest:: TestTree
updateLivesTest = testGroup "Tests to check if lives are decremented appropriately"
  [
    testCase "Playership hits enemy but not laser" $
      updateLives (genStaticGame 0) (V2 (width `div` 2) 0) [] [(genEnemyWithCoord (width `div` 2) 0), (genEnemyWithCoord 100 5)] @?= 2,

    testCase "Playership hits enemy's laser but not enemy" $
      updateLives (genStaticGame 0) (V2 (width `div` 2) 0) [(V2 (width `div` 2) 0)] [(genEnemyWithCoord 100 5)] @?= 2,

    testCase "No double penalty test with laser and enemy hitting player" $
      updateLives (genStaticGame 0) (V2 (width `div` 2) 0) [(V2 (width `div` 2) 0)] [(genEnemyWithCoord (width `div` 2) 0)] @?= 2,
    
    testCase "No double penalty test with 2 lasers hitting player" $
      updateLives (genStaticGame 0) (V2 (width `div` 2) 0) [(V2 (width `div` 2) 0), (V2 (width `div` 2) 0)] [] @?= 2,

    testCase "Playership is not hit by anything" $
      updateLives (genStaticGame 0) (V2 (width `div` 2) 0) [] [] @?= 3
  ]

killEnemyTest :: TestTree
killEnemyTest = testGroup "Tests to check if enemies die appropriately"
  [
    testCase "Non-attacking enemy collides with player laser" $
      moveAndKill [(genEnemyWithCoord 10 10), (genEnemyWithCoord 20 20)] [(V2 10 10)] @?= [genEnemyWithCoord 20 20],
  
    testCase "Attacking enemy collides with player laser" $
      moveAndKill2 [(genEnemyWithCoord 100 100)] [(V2 100 100)] @?= [],
    
    testCase "Enemy does not collide with any player lasers" $
       moveAndKill [(genEnemyWithCoord 20 20)] [(V2 10 10), (V2 30 30), (V2 100 100)] @?= [genEnemyWithCoord 20 20]
  ]

updateScoreTest :: TestTree
updateScoreTest = testGroup "Tests to check if player score updates correctly" $
  [
    testCase "Update score after killing 3 enemies on level 3, starting from score 100" $
      updateScore (getLevel 3) 100 (genTestEnemies [(genEnemyWithCoord 10 10), (genEnemyWithCoord 20 20), (genEnemyWithCoord 100 100)]) (genTestEnemies []) @?= 250,
    
    testCase "Update score after killing 1 enemy on level 0, starting from score 0" $ 
      updateScore (getLevel 0) 0 (genTestEnemies [(genEnemyWithCoord 10 10), (genEnemyWithCoord 20 20)]) (genTestEnemies [(genEnemyWithCoord 20 20)]) @?= 20,
    
    testCase "Update score after killing 0 enemies on level 5, starting from score 1000" $
      updateScore (getLevel 5) 1000 (genTestEnemies [(genEnemyWithCoord 20 20)]) (genTestEnemies [(genEnemyWithCoord 20 20)]) @?= 1000
  ]

genTestEnemies :: [Enemy] -> Enemies
genTestEnemies listEnemies = Enemies listEnemies 0 [] [] 0

-- Generate Enemy at a specific coordinate (V2 x y)
genEnemyWithCoord :: Int -> Int -> Enemy
genEnemyWithCoord x y = E (V2 x y) False R

-- Generate Game - specific to make testing easier
genStaticGame :: Int -> Game
genStaticGame levelNum =  Game 3 (getLevel levelNum) 0 False (V2 (width `div` 2) 0) [] testEnemies [] 0
               where 
                testEnemies = Enemies [] 0 [] [] 0
newEne :: Game -> Enemies
newEne g = updateEnemyAfterShots (enemies g) (playerShots g)

shotAreRemoved :: Game -> Bool
shotAreRemoved g = (checkl shots o_el n_el) && (checkl (getC shots) o_al n_al)
  where
    shots = playerShots g
    old_enemies = enemies g
    new_enemies = newEne g
    o_el = map coord (enemyList old_enemies)
    o_al = (map coord (attackEnemy old_enemies))
    n_el = map coord (enemyList new_enemies)
    n_al = map coord (attackEnemy new_enemies)

zip' :: [a]->[b]->[(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

getMatch :: Ord a => [a] -> [a] -> Int
getMatch l r = getMatch' (sort l) (sort r)
getMatch' :: Ord a => [a] -> [a] -> Int
getMatch' [] _ = 0
getMatch' _ [] = 0
getMatch' l@(a: as) r@(b: bs) | a < b = getMatch' as r
                              | a == b = getMatch' as bs + 1
                              | otherwise = getMatch' l bs

checkl :: [Coord] -> [Coord] -> [Coord] -> Bool
checkl s o n = ((length o) - (getMatch s o) == length n)

prop_e :: Property
prop_e = forAll genGame (shotAreRemoved)

-- Generate Coord with given maximum x and y
genCoord :: Int -> Int -> Gen Coord
genCoord xMax yMax
 = do
  x <- chooseInt (0, xMax)
  y <- chooseInt (0, yMax)
  return (V2 x y)

-- Generate shots
genShots :: Gen [Coord]
genShots = do
  shots <- listOf (genCoord width height)
  return shots

-- Generate an Alive Enemy
genAliveEnemy :: Gen Enemy
genAliveEnemy = do
  coord <- genCoord width height
  let edead = False
  -- direc <- genDirec
  let direc = L
  return (E coord edead direc)

-- Generate Alive Enemies
genAliveEnemies :: Gen Enemies
genAliveEnemies = do
  enemyList    <- listOf1 genAliveEnemy
  -- countdown    <- (arbitrary :: Int)
  let countdown = 10
  origPosition <- listOf1 genAliveEnemy
  attackEnemy  <- listOf1 genAliveEnemy
  -- attackFreq   <- (arbitrary :: Int)
  let attackFreq = 1
  return (Enemies enemyList countdown origPosition attackEnemy attackFreq)

-- -- Generate Direction
-- genDirec :: Gen Direction
-- genDirec = do
--   return oneof [L, R, U, D]

-- -- Generate Enemy
-- genEnemy :: Gen Enemy
-- genEnemy = do
--   coord <- genCoord
--   edead <- (arbitrary :: Bool)
--   direc <- genDirec
--   return (E coord edead direc)

-- Generate Enemies
-- genEnemies :: Gen Enemies
-- genEnemies = do
--   enemyList    <- listOf genEnemy
--   countdown    <- (arbitrary :: Int)
--   origPosition <- listOf genEnemy
--   attackEnemy  <- listOf genEnemy
--   attackFreq   <- (arbitrary :: Int)
--   return (Enemies enemyList countdown origPosition attackEnemy attackFreq)

-- -- Generate Level
-- genLevel :: Gen Level
-- genLevel = do
--   levelNumber     <- abs (arbitrary :: Int)
--   attackFrequency <- abs (arbitrary :: Int)
--   lShots          <- abs (arbitrary :: Int)
--   return (Level levelNumber attackFrequency lShots)

-- -- Generate Game
-- genGame :: Gen Game
-- genGame = do
--   li  <- chooseInt(0, 3)
--   l   <- genLevel
--   s   <- chooseInt(0, 3)
--   d   <- (arbitrary :: Bool)
--   psh <- genCoord
--   pSh <- listOf genCoord
--   es  <- genEnemies
--   eSh <- listOf genCoord
--   cs  <- abs (arbitrary :: Int)
--   return (Game li l s d psh pSh es eSh cs)

genGame :: Gen Game
genGame = do
  -- li  <- chooseInt(0, 3)
  let li = 3
  l <- genLevel
  let s = 0
  let d = False
  psh <- genCoord width height

  -- pSh <- listOf (genCoord width height)
  pSh <- listOf1 (genCoord width height)
  -- pSh <- (forAll (listOf $ listOf1 arbitrary)) (genCoord width height)
  -- pSh <- listOf nonEmptyL


  es  <- genAliveEnemies
  -- eSh <- listOf (genCoord width height)
  let eSh = []
  let cs = 8
  return (Game li l s d psh pSh es eSh cs)

-- Generate Level
genLevel :: Gen Level
genLevel = do
  -- levelNumber     <- abs (arbitrary :: Int)
  -- attackFrequency <- abs (arbitrary :: Int)
  -- lShots          <- abs (arbitrary :: Int)
  let levelNumber     = 1
  let attackFrequency = 1
  let lShots          = 1000
  return (Level levelNumber attackFrequency lShots)