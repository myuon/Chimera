{-# LANGUAGE TemplateHaskell, FlexibleContexts, ImplicitParams #-}
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
import Object
import qualified Key

update :: Key.Keys -> Player -> Player
update key = execState $ do
  updateCounter
  updatePos key
  
updateCounter :: State Player ()
updateCounter = chara.counter %= (+1)

updatePos :: Key.Keys -> State Player ()
updatePos key = do
  k <- use (chara.speed)
  (chara.pos) %= ($+ (k $* toNum dir))
  (chara.pos) %= clamp
  
  where
    dir :: (Int, Int)
    dir =
      addTup (key ^. Key.up    > 0) (0,-1) $
      addTup (key ^. Key.down  > 0) (0,1) $
      addTup (key ^. Key.right > 0) (1,0) $
      addTup (key ^. Key.left  > 0) (-1,0) $
      (0,0)

    addTup :: (Num a) => Bool -> Point a -> Point a -> Point a
    addTup b p q = bool q (p$+q) b

clamp :: Pos -> Pos
clamp = toNum . (edgeX *** edgeY) . toInt
  where
    edgeX :: Int -> Int
    edgeX = (\p -> bool p 0 (p < 0)) .
            (\p -> bool p 640 (p > 640))
    
    edgeY :: Int -> Int
    edgeY = (\p -> bool p 0 (p < 0)) .
            (\p -> bool p 480 (p > 480))

draw :: SDL.Surface -> SDL.Surface -> Player -> IO ()
draw screen img p = do
  let (px,py) = toInt $ p ^. (chara.pos)
  let (x,y) = center $ SDL.Rect px py 50 50
  
  SDL.blitSurface 
    img (Just $ SDL.Rect 0 0 50 50)
    screen (Just $ SDL.Rect x y 50 50)
  return ()
