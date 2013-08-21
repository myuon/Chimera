{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}
module Player where

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as SDLI
import Graphics.UI.SDL.Rect as SDLR
import Control.Bool
import Control.Lens
import Control.Arrow
import Control.Monad.State
import Debug.Trace

import Global
import qualified Key

data Player = Player {
  _pos :: Pos,
  _sp :: Int,
  _image :: IO SDL.Surface
  }

makeLenses ''Player

initPlayer :: Player
initPlayer = Player {
  _pos = (0,0),
  _sp = 2,
  _image = SDLI.load "data/img/player_reimu.png"
  }

update :: Key.Keys -> Player -> Player
update key = execState $ do
  updatePos key

updatePos :: Key.Keys -> State Player ()
updatePos key = do
  (x,y) <- use pos
  k <- use sp
  pos .= (x,y) $+ (k $* (dx,dy))
  pos %= clamp
  
  where
    (dx,dy) =
      addTup (key ^. Key.up    > 0) (0,-1) $
      addTup (key ^. Key.down  > 0) (0,1) $
      addTup (key ^. Key.right > 0) (1,0) $
      addTup (key ^. Key.left  > 0) (-1,0) $
      (0,0)

    addTup :: Bool -> Pos -> Pos -> Pos
    addTup b p q = bool q (p$+q) b
    
clamp :: Pos -> Pos
clamp = edgeX *** edgeY
  where
    edgeX :: Int -> Int
    edgeX = (\p -> bool p 0 (p < 0)) .
            (\p -> bool p 640 (p > 640))
    
    edgeY :: Int -> Int
    edgeY = (\p -> bool p 0 (p < 0)) .
            (\p -> bool p 480 (p > 480))

draw :: SDL.Surface -> Player -> IO ()
draw screen p = do
  image <- p ^. image
  rect <- SDL.getClipRect image
  let (px,py) = p ^. pos
  let (x,y) = center $ SDL.Rect px py 50 50
  
  SDL.blitSurface 
    image (Just $ SDL.Rect 0 0 50 50)
    screen (Just $ SDL.Rect x y 50 50)
  SDL.freeSurface image
  return ()


