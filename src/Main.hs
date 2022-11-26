module Main
  ( main
  ) where

import Model
import View
import Control
import Brick
import qualified Graphics.Vty as V
import Control.Monad (forever, void)
import Brick.BChan (newBChan, writeBChan)
import Control.Concurrent (threadDelay, forkIO)

main :: IO ()
main = do
  chan <- newBChan 10
  _ <- forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 100000 -- decides how fast your game moves
  g <- initGame
  let builder = V.mkVty V.defaultConfig
  initialVty <- builder
  void $ customMain initialVty builder (Just chan) app g

-- | Initialize the app
app :: App Game Tick Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = do
                              g <- get
                              put g
          , appAttrMap = const attributeMap
          }

-- | Handle ticks and key press events
-- TODO: Figure out why VSCode gives a warning here for the function type
handleEvent :: BrickEvent Name Tick -> EventM Name Game ()
handleEvent (AppEvent Tick)                       = do
                                                        g <- get
                                                        put $ step g
-- Escape key to exit the game
handleEvent (VtyEvent (V.EvKey V.KEsc []))        = halt
handleEvent (VtyEvent (V.EvKey V.KRight []))      = do
                                                      g <- get
                                                      put $ move (+ 1) g
handleEvent (VtyEvent (V.EvKey (V.KChar 'd') [])) = do
                                                      g <- get
                                                      put $ move (+ 1) g
handleEvent (VtyEvent (V.EvKey V.KLeft []))       = do
                                                      g <- get
                                                      put $ move (subtract 1) g
handleEvent (VtyEvent (V.EvKey (V.KChar 'a') [])) = do
                                                      g <- get
                                                      put $ move (subtract 1) g   
-- handleEvent g (VtyEvent (V.EvKey (V.KChar ' ') [])) = continue $ shoot g  
handleEvent (VtyEvent (V.EvKey (V.KChar ' ') [])) = do
                                                      g <- get 
                                                      put $ shoot g                                                                                                                                                                
handleEvent _                                     = do
                                                      g <- get
                                                      put g

-- | Update the UI as events are handled (ex: Galaxians move, shots fired)
step :: Game -> Game
step g@(Game li l s d p sh _ esh) = do
  let playerShotsNew = updateShots g U -- update player shots, 
  -- Game li l s d p shotsNew e

  -- let tmp_ = handleShots g shotsNew -- handle out of bound shots

  -- let eNew = updateEnemy g shotsNew -- update aliens

  -- let tmp_ = handleShots g shotsNew -- handle out of bound shots
  -- let tmp_ = handleShots (Game li l s d p sh eNew) shotsNew -- handle out of bound shots

    
  let eNew = updateEnemy g -- update aliens
  let eNew' = updateEnemyAfterShots eNew playerShotsNew
  -- let eNew = updateEnemy g shotsNew -- update aliens

  -- let tmp_ = handleShots g shotsNew -- handle out of bound shots
  let tmp_ = handleShots (Game li l s d p sh eNew esh) playerShotsNew -- handle out of bound shots


  -- Game li l s d p shotsNew eNew'
  -- Game li l s d p tmp_ eNew
  Game li l s d p tmp_ eNew' esh

