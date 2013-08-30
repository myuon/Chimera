{-# LANGUAGE TemplateHaskell, FlexibleContexts, ImplicitParams #-}
module Player where

import qualified Graphics.UI.FreeGame as Game
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
  (chara.pos) %= (+ (k $* dir))
  (chara.pos) %= clamp
  
  where
    dir :: Pos
    dir = let addTup b p q = bool q (fromPair p+q) b in
      addTup (key ^. Key.up    > 0) (0,-1) $
      addTup (key ^. Key.down  > 0) (0,1) $
      addTup (key ^. Key.right > 0) (1,0) $
      addTup (key ^. Key.left  > 0) (-1,0) $
      fromPair (0,0)

clamp :: Pos -> Pos
clamp = toNum . fromPair . (edgeX *** edgeY) . toPair . toInt
  where
    edgeX :: Int -> Int
    edgeX = (\p -> bool p 0 (p < 0)) .
            (\p -> bool p 640 (p > 640))
    
    edgeY :: Int -> Int
    edgeY = (\p -> bool p 0 (p < 0)) .
            (\p -> bool p 480 (p > 480))

draw :: Game.Bitmap -> Player -> Game.Game ()
draw img p = do
  Game.translate (toFloat $ p ^. pos) $ Game.fromBitmap img
