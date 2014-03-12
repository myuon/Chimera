{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances, FunctionalDependencies, ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
module Chimera.Core.Util (
  fromPair, toPair
  , areaTop, areaLeft, areaBottom, areaRight
  , fromPolar
  , isInside
  , boxVertex, boxVertexRotated
  , cutIntoN
  , when_, (><=)
  , rot2D
  , clamp
  , insertIM, insertIM'
  ) where

import FreeGame
import Control.Lens
import Control.Arrow ((***))
import Control.Monad.State.Strict (MonadState)
import qualified Data.Sequence as S
import qualified Data.IntMap.Strict as IM

areaTop, areaLeft, areaBottom, areaRight :: Double
areaTop = 16
areaLeft = 32
areaBottom = 444
areaRight = 416

fromPair :: (a,a) -> V2 a
fromPair = uncurry V2

toPair :: V2 a -> (a,a)
toPair (V2 a b) = (a,b)

fromPolar :: (Double, Double) -> Vec2
fromPolar (r,t) = r *^ fromPair (cos (-t), sin (-t))

isInside :: Vec2 -> Bool
isInside (V2 a b) = (areaLeft-40 <= a && a <= areaRight+40) && (areaTop-40 <= b && b <= areaBottom+40)

boxVertex :: Vec2 -> Vec2 -> [Vec2]
boxVertex pos size = [pos - size,
                      pos + V2 (size^._x) (-size^._y),
                      pos + size,
                      pos + V2 (-size^._x) (size^._y)]

boxVertexRotated :: Vec2 -> Vec2 -> Double -> [Vec2]
boxVertexRotated pos size angle = let r = rot2D angle in
  map (pos +) $ map (r !*) $ boxVertex 0 size

cutIntoN :: Int -> Bitmap -> [Bitmap]
cutIntoN n img = let (w,h) = bitmapSize img; w1 = w `div` n in
  [cropBitmap img (w1,h) (w1*i,0) | i <- [0..n-1]]

when_ :: (Monad m) => m Bool -> m () -> m ()
when_ mp m = mp >>= (\b -> when b m)

(><=) :: (MonadState s m) => 
         Setting (->) s s (S.Seq a) (S.Seq a) -> (S.Seq a) -> m ()
a ><= b = a %= (S.>< b)

rot2D :: Double -> M22 Double
rot2D r = V2
          (V2 (cos(-r)) (-sin(-r)))
          (V2 (sin(-r)) (cos(-r)))

clamp :: Vec2 -> Vec2
clamp = fromPair . (edgeX *** edgeY) . toPair
  where
    edgeX = (\p -> bool p areaLeft (p < areaLeft)) .
            (\p -> bool p areaRight (p > areaRight))
    
    edgeY = (\p -> bool p areaLeft (p < areaLeft)) .
            (\p -> bool p areaBottom (p > areaBottom))

insertIM :: a -> IM.IntMap a -> IM.IntMap a
insertIM a m = snd $ insertIM' a m

insertIM' :: a -> IM.IntMap a -> (Int, IM.IntMap a)
insertIM' a m = case n `IM.member` m of
  True -> (n', IM.insert n' a m)
  False -> (n, IM.insert n a m)
  
  where
    n = IM.size m
    n' = head [x | x <- [0..], x `IM.notMember` m]
