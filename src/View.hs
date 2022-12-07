module View where

import Model
import Brick
  ( Widget,
    Padding(..),
    AttrMap, AttrName, attrName, withAttr, attrMap
    , vBox, hBox, withBorderStyle, str
    , fg
    , emptyWidget, padRight, padTop, padAll
    , hLimit, (<+>), Padding(..), (<=>), (<+>)
  )
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V
import Linear.V2 (V2(..))

-- | Functions to draw UI components
drawUI :: Game -> [Widget Name]
drawUI g =
  [ C.center $ padRight (Pad 2) (drawStats g) <+> drawGrid g ]

drawStats :: Game -> Widget Name
drawStats g = hLimit 14
  $ vBox [(drawScore $score g) g,  padTop (Pad 2) $ drawGameOver (dead g)]

drawScore :: Int -> Game -> Widget Name
drawScore n g = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str ("Level " ++ show (levelNumber (level g))))
  $ C.hCenter
  $ padAll 1
  $ (str "Score: " <+> str (show n)) <=> (str "Lives: " <+> str (show li))
    where li = if lives g > 0 then lives g else 0

drawGameOver :: Bool -> Widget Name
drawGameOver isDead =
  if isDead
     then withAttr gameOverAttr $ C.hCenter $ str "GAME OVER"
     else emptyWidget

drawGrid :: Game -> Widget Name
drawGrid g = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "*+ Brick Galaxian +*")
  $ vBox rows
  where
    rows         = [hBox $ cellsInRow r | r <- [height-1,height-2..0]]
    cellsInRow y = [drawCoord (V2 x y) | x <- [0..width-1]]
    drawCoord    = drawCell . cellAt
    cellAt c
      | c == playership g               = PlayershipCell
      | c `elem` enemyCoords g          = EnemyCell
      | c `elem` playerShots g          = PlayerShotCell
      | c `elem` enemiesShots g         = EnemyShotCell
      | otherwise                       = EmptyCell

drawCell :: Cell -> Widget Name
drawCell PlayershipCell = withAttr playershipAttr $str " ▄▓▄ " <=> str "▀▀▓▀▀"
drawCell EnemyCell = withAttr enemyAttr $str ">▓<"
drawCell EmptyCell = withAttr emptyAttr $str "   " <=> str "   "
drawCell PlayerShotCell = withAttr playerShotAttr $str " ║ "
drawCell EnemyShotCell  = withAttr enemyShotAttr $str " ║ "

-- | Attribute style
attributeMap :: AttrMap
attributeMap = attrMap V.defAttr
  [
    (playerShotAttr, fg V.yellow  `V.withStyle` V.bold),
    (playershipAttr, fg V.green   `V.withStyle` V.bold),
    (enemyShotAttr,  fg V.magenta `V.withStyle` V.bold),
    (enemyAttr, fg V.red `V.withStyle` V.bold),
    (gameOverAttr, fg V.red `V.withStyle` V.bold)
  ]
  

-- | Attribute definitions
enemyShotAttr, playerShotAttr, gameOverAttr, emptyAttr, playershipAttr, enemyAttr :: AttrName
gameOverAttr = attrName "gameOver"
emptyAttr = attrName "emptyAttr"
playershipAttr = attrName "playershipAttr"
enemyAttr = attrName "enemyAttr"
playerShotAttr  = attrName "playerShotAttr"
enemyShotAttr   = attrName "enemyShotAttr"
