{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}
module Player where

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as SDLI
import Control.Lens
import Control.Monad.State

import Global

data Player = Player {
  _pos :: Pos,
  _image :: IO SDL.Surface
  }

makeLenses ''Player

initPlayer :: Player
initPlayer = Player {
  _pos = (0,0),
  _image = SDLI.load "data/img/player_reimu.png"
  }

update :: Player -> Player
update = execState $ do
  updatePos

updatePos :: State Player ()
updatePos = do
  (x,y) <- use pos
  pos .= (x+1, y)

draw :: SDL.Surface -> Player -> IO ()
draw screen p = do
  image <- p ^. image
  SDL.blitSurface 
    image (Just $ SDL.Rect 0 0 50 50)
    screen (Just $ SDL.Rect 0 0 640 480)
  SDL.freeSurface image
  return ()
