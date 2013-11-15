{-# LANGUAGE TemplateHaskell, FlexibleContexts, ImplicitParams #-}
module Chimera.Player where

import qualified Graphics.UI.FreeGame as Game
import Control.Lens
import Control.Arrow
import Control.Monad.State
import Debug.Trace

import Chimera.Global
import Chimera.Object
import qualified Chimera.Key as Key

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
clamp = fromPair . (edgeX *** edgeY) . toPair
  where
--    edgeX :: Int -> Int
    edgeX = (\p -> bool p areaLeft (p < areaLeft)) .
            (\p -> bool p areaRight (p > areaRight))
    
--    edgeY :: Int -> Int
    edgeY = (\p -> bool p areaLeft (p < areaLeft)) .
            (\p -> bool p areaBottom (p > areaBottom))

draw :: Game.Bitmap -> Player -> Game.Game ()
draw img p = do
  Game.translate (fmap realToFrac $ p ^. pos) $ Game.fromBitmap img
