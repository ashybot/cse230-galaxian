module View where

import Model
import Brick
  ( Widget,
    Padding(..),
    AttrMap, AttrName, attrName, withAttr, attrMap
    , vBox, hBox, withBorderStyle, str
    , fg
    , emptyWidget, padRight, padTop, padAll
    , hLimit, (<+>), Padding(..), (<=>)
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
drawStats g = hLimit 11
  $ vBox [drawScore $score g,  padTop (Pad 2) $ drawGameOver (dead g)]

drawScore :: Int -> Widget Name
drawScore n = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Score")
  $ C.hCenter
  $ padAll 1
  $ str $ show n

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
      | c `elem` enemyCoords 10         = EnemyCell
      | otherwise                       = EmptyCell

drawCell :: Cell -> Widget Name
drawCell PlayershipCell = withAttr playershipAttr $str " ▄▓▄ " <=> str "▀▀▓▀▀"
drawCell EnemyCell = withAttr enemyAttr $str ">▓<"
drawCell EmptyCell      = withAttr emptyAttr $str "   " <=> str "   "

-- | Attribute style
attributeMap :: AttrMap
attributeMap = attrMap V.defAttr
  [
    (playershipAttr, fg V.green `V.withStyle` V.bold),
    (enemyAttr, fg V.red `V.withStyle` V.bold),
    (gameOverAttr, fg V.red `V.withStyle` V.bold)
  ]

-- | Attribute definitions
gameOverAttr, emptyAttr, playershipAttr, enemyAttr :: AttrName
gameOverAttr = attrName "gameOver"
emptyAttr = attrName "emptyAttr"
playershipAttr = attrName "playershipAttr"
enemyAttr = attrName "enemyAttr"