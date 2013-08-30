{-# LANGUAGE TemplateHaskell #-}
module Global where

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as SDLI
import qualified Linear.V2 as V2
import Control.Lens
import Control.Arrow
import Control.Monad.State
import GHC.Float

-- import Control.Bool
bool :: a -> a -> Bool -> a
bool x _ False = x
bool _ y True = y

type Pos = V2.V2 Double
type PoInt = V2.V2 Int

toNum :: PoInt -> Pos
toNum = fmap fromIntegral

toFloat :: Pos -> V2.V2 Float
toFloat = fmap double2Float

toInt :: Pos -> PoInt
toInt = fmap truncate

fromPair :: (a,a) -> V2.V2 a
fromPair = uncurry V2.V2

toPair :: V2.V2 a -> (a,a)
toPair (V2.V2 a b) = (a,b)

($*) :: (Num a) => a -> V2.V2 a -> V2.V2 a
($*) k = fmap (*k)

fromPolar :: (Double, Double) -> Pos
fromPolar (r,t) = r $* (fromPair (cos t, -sin t))

isInside :: Pos -> Bool
isInside = uncurry (&&) .
           ((\p -> p >= 0 && p <= 640) *** (\p -> p >= 0 && p <= 480)) .
           toPair

absV :: (Num a) => V2.V2 a -> a
absV v = (v ^. V2._x) ^ 2 + (v ^. V2._y) ^2
