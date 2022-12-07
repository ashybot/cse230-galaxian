module Main (main) where
import Model
import Control

import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=) )
import Linear.V2 (V2(..))
import Test.QuickCheck 
import Test.Tasty

import Data.List

main :: IO ()
-- main = defaultMain unitTests
main = do 
        -- let a = 3
--         -- defaultMain unitTests
        quickCheck prop_e


-- main = runTests 
--   [ testShotEnemies
--   ]

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

-- test for enemies gone when lazer hits them
-- need to generate game where
  -- generate enemies
  -- generate lazers
  -- generate some lazers position == enemies
genStaticGame2 :: Gen Game
genStaticGame2 = do
                  t_shots <- genShots
                  t_enemies <- genAliveEnemies
                  return (Game 3 (getLevel 0) 0 False (V2 (width `div` 2) 0) t_shots t_enemies [] 0)

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
getMatch' [] _ = 0
getMatch' _ [] = 0
getMatch' l@(a: as) r@(b: bs) | a < b = getMatch' as r
                              | a == b = getMatch' as bs + 1
                              | otherwise = getMatch' l bs

checkl :: [Coord] -> [Coord] -> [Coord] -> Bool
checkl s o n = ((length o) - (getMatch s o) == length n)

-- checkl s o n = (length filtered == length n)
--   where 
--     filtered = [x | x <- a', x==False] -- remove dead aliens 
--     a' = map (\(a1,a2) -> if a1==a2 then True else False) (zip' s o)



prop_e :: Property
prop_e = forAll genGame (shotAreRemoved)

-- quickCheck prop_e

-- testShotEnemies :: TestTree
-- testShotEnemies sc = testGroup "whatttt"
--   [ 
--     -- testCase "WAAAAAAA" $
--     -- testProperty "what" prop_e

--     -- propertyRead prop_e @?= True
--     -- scoreProp sc ("prop_insert_bso"      , prop_e    , 3) 
--     -- testCase "Playership hits enemy but not laser" $
--     --   updateLives genStaticGame (V2 (width `div` 2) 0) [] [(genEnemyWithCoord (width `div` 2) 0), (genEnemyWithCoord 100 5)] @?= 2,
--   ]

-- --------------------------------------------------------------------------------
-- scoreProp :: (QC.Testable prop) => Score -> (String, prop, Int) -> TestTree
-- --------------------------------------------------------------------------------
-- scoreProp sc (name, prop, n) = scoreTest' sc (act, (), True, n, name)
--   where
--     act _                    = QC.isSuccess <$> QC.labelledExamplesWithResult args prop
--     args                     = QC.stdArgs { QC.chatty = False, QC.maxSuccess = 100 }

prop_what :: Property
prop_what = property (1==1)

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

-- nonEmptyL :: Gen String
nonEmptyL = listOf1 (genCoord width height)

-- genGame :: Gen Game
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