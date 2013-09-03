{-# LANGUAGE TemplateHaskell #-}
module Global where

import qualified Graphics.UI.FreeGame as Game
import qualified Linear.V2 as V2
import qualified Linear.Vector as Vec
import Control.Lens
import Control.Arrow
import Control.Monad.State
import Data.Foldable as F

-- import Control.Bool
bool :: a -> a -> Bool -> a
bool x _ False = x
bool _ y True = y

type Pos = V2.V2 Double
type PoInt = V2.V2 Int

toNum :: PoInt -> Pos
toNum = fmap fromIntegral

toInt :: Pos -> PoInt
toInt = fmap truncate

fromPair :: (a,a) -> V2.V2 a
fromPair = uncurry V2.V2

toPair :: V2.V2 a -> (a,a)
toPair (V2.V2 a b) = (a,b)

($*) :: (Num a) => a -> V2.V2 a -> V2.V2 a
($*) = (Vec.*^)

fromPolar :: (Double, Double) -> Pos
fromPolar (r,t) = r $* Game.unitV2 (-t)

isInside :: Pos -> Bool
isInside (V2.V2 a b) = (0 <= a && a <= 640) && (0 <= b && b <= 480)

absV :: (Num a) => V2.V2 a -> a
absV v = F.sum $ v * v
