module Test where 
import System.Exit
import Main
import Control
import Model
import Control

import Test.QuickCheck 

-- Generate Coord with given maximum x and y
genCoord :: Int -> Int -> Gen Coord 
genCoord xMax yMax
  = do 
    if (xMax < 0) then xMax = 0
    else if (yMax < 0) then yMax = 0
    else 
      x <- chooseInt (0, xMax)
      y <- chooseInt (0, yMax)
      return (V2 x y)

-- Generate Direction
genDirec :: Gen Direction
genDirec = do 
  return oneof [Direction L, Direction R, Direction U, Direction D]

-- Generate Enemy
genEnemy :: Gen Enemy
genEnemy = do 
  coord <- genCoord
  edead <- (arbitrary :: Bool)
  direc <- genDirec
  return (E coord edead direc)

-- Generate Enemies
genEnemies :: Gen Enemies
genEnemies = do 
  enemyList    <- listOf genEnemy 
  countdown    <- (arbitrary :: Int)
  origPosition <- listOf genEnemy 
  attackEnemy  <- listOf genEnemy 
  attackFreq   <- (arbitrary :: Int)
  return (Enemies enemyList countdown origPosition attackEnemy attackFreq)

-- Generate Level
genLevel :: Gen Level
genLevel = do
  levelNumber     <- abs (arbitrary :: Int)
  attackFrequency <- abs (arbitrary :: Int)
  lShots          <- abs (arbitrary :: Int)
  return (Level levelNumber attackFrequency lShots)

-- Generate Game
genGame = do
  li  <- chooseInt(0, 3)
  l   <- genLevel
  s   <- chooseInt(0, 3)
  d   <- (arbitrary :: Bool)
  psh <- genCoord
  pSh <- listOf genCoord 
  es  <- genEnemies 
  eSh <- listOf genCoord 
  cs  <- abs (arbitrary :: Int)
  return (Game li l s d psh pSh es eSh cs)

main :: IO ()
main = do 
  putStrLn "\nRunning my tests... "
  putStrLn "\nDone Testing"
  exitWith ExitSuccess 