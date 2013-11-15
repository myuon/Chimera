{-# LANGUAGE TemplateHaskell #-}
module Chimera.Global where

import qualified Graphics.UI.FreeGame as Game
import qualified Linear.Vector as Vec
import Control.Lens
import Control.Arrow
import Control.Monad.State
import Data.Foldable as F

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

type Pos = Game.V2 Double'
type PoInt = Game.V2 Int

toNum :: PoInt -> Pos
toNum = fmap fromIntegral

toInt :: Pos -> PoInt
toInt = fmap truncate

fromPair :: (a,a) -> Game.V2 a
fromPair = uncurry Game.V2

toPair :: Game.V2 a -> (a,a)
toPair (Game.V2 a b) = (a,b)

($*) :: (Num a) => a -> Game.V2 a -> Game.V2 a
($*) = (Vec.*^)

fromPolar :: (Double', Double') -> Pos
fromPolar (r,t) = r $* fromPair (cos (-t), sin (-t))

isInside :: Pos -> Bool
isInside (Game.V2 a b) = (areaLeft <= a && a <= areaRight) && (areaTop <= b && b <= areaBottom)

absV :: (Num a) => Game.V2 a -> a
absV v = F.sum $ v * v
