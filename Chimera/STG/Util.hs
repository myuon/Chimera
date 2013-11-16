module Chimera.STG.Util (
  Double'
  , V2, Vec, Pos
  , fromPair, toPair
  , areaTop, areaLeft, areaBottom, areaRight
  , ($*)
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

-- import Control.Bool
bool :: a -> a -> Bool -> a
bool x _ False = x
bool _ y True = y

type Vec = V2 Double'
type Pos = V2 Int

toNum :: Pos -> Vec
toNum = fmap fromIntegral

toInt :: Vec -> Pos
toInt = fmap truncate

fromPair :: (a,a) -> V2 a
fromPair = uncurry V2

toPair :: V2 a -> (a,a)
toPair (V2 a b) = (a,b)

($*) :: (Num a) => a -> V2 a -> V2 a
($*) k = fmap (k*)

fromPolar :: (Double', Double') -> Vec
fromPolar (r,t) = r $* fromPair (cos (-t), sin (-t))

isInside :: Vec -> Bool
isInside (V2 a b) = (areaLeft <= a && a <= areaRight) && (areaTop <= b && b <= areaBottom)

absV :: (Num a) => V2 a -> a
absV v = let v' = v * v in (v'^._x)^2 + (v'^._y)
