module Main (main) where
import Model
import Control

import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=) )
import Linear.V2 (V2(..))

main :: IO ()
main = defaultMain unitTests

unitTests:: TestTree
unitTests = testGroup "Tests" [updateLivesTest]

updateLivesTest:: TestTree
updateLivesTest = testGroup "Tests to check if lives are decremented appropriately"
  [
    testCase "Playership hits enemy but not laser" $
      updateLives genStaticGame (V2 (width `div` 2) 0) [] [(genEnemyWithCoord (width `div` 2) 0), (genEnemyWithCoord 100 5)] @?= 2,

    testCase "Playership hits enemy's laser but not enemy" $
      updateLives genStaticGame (V2 (width `div` 2) 0) [(V2 (width `div` 2) 0)] [(genEnemyWithCoord 100 5)] @?= 2,

    testCase "Ensure no double penalty if player hits enemy and laser at the same time" $
      updateLives genStaticGame (V2 (width `div` 2) 0) [(V2 (width `div` 2) 0)] [(genEnemyWithCoord (width `div` 2) 0)] @?= 2,

    testCase "Playership is not hit by anything" $
      updateLives genStaticGame (V2 (width `div` 2) 0) [] [] @?= 3
  ]

-- Generate Enemy at a specific coordinate (V2 x y)
genEnemyWithCoord :: Int -> Int -> Enemy
genEnemyWithCoord x y = E (V2 x y) False R

-- Generate Game - specific to make testing easier
-- For updateLivesTest we only need to utilize the first argument (# of lives) so this will be fine to leave the other parts empty
genStaticGame :: Game
genStaticGame =  Game 3 (getLevel 0) 0 False (V2 (width `div` 2) 0) [] testEnemies [] 0
               where 
                testEnemies = Enemies [] 0 [] [] 0

-- Generate Coord with given maximum x and y
-- genCoord :: Int -> Int -> Gen Coord
-- genCoord xMax yMax
--  = do
--   x <- chooseInt (0, xMax)
--   y <- chooseInt (0, yMax)
--   return (V2 x y)

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

-- Generate Game
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