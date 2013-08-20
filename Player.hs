{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}
module Player where

import Control.Lens
import Control.Monad.State

import Global

data Player = Player {
  _pos :: Pos, 
  _sp :: Pos
  } deriving (Eq, Show)

makeLenses ''Player

initPlayer :: Player
initPlayer = Player (0,0) (1,-1)

update :: Player -> Player
update = execState $ do
  updatePos

updatePos :: State Player ()
updatePos = do
  (x,y) <- use pos
  pos .= (x+1, y)

draw :: Player -> IO ()
draw p = do
  print $ p ^. pos
