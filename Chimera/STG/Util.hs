module Chimera.STG.Util (
  Double'
  , V2, Vec, Pos
  , fromPair, toPair
  , areaTop, areaLeft, areaBottom, areaRight
  , fromPolar
  , isInside
  , boxVertex
  ) where

import Graphics.UI.FreeGame
import Control.Lens

type Double' = Float

winWidth = 640
winHeight = 480

areaTop = 16 :: Double'
areaLeft = 32 :: Double'
areaBottom = 444 :: Double'
areaRight = 416 :: Double'

type Vec = V2 Double'
type Pos = V2 Int

fromPair :: (a,a) -> V2 a
fromPair = uncurry V2

toPair :: V2 a -> (a,a)
toPair (V2 a b) = (a,b)

fromPolar :: (Double', Double') -> Vec
fromPolar (r,t) = r *^ fromPair (cos (-t), sin (-t))

isInside :: Vec -> Bool
isInside (V2 a b) = (areaLeft-40 <= a && a <= areaRight+40) && (areaTop-40 <= b && b <= areaBottom+40)

boxVertex :: Vec -> Vec -> [Vec]
boxVertex pos size = [pos - size,
                      pos + V2 (size^._x) (-size^._y),
                      pos + size,
                      pos + V2 (-size^._x) (size^._y)]
