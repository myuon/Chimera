{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances, FunctionalDependencies, ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
module Chimera.Core.Util (
  areaTop, areaLeft, areaBottom, areaRight
  , isInside
  , boxVertex, boxVertexRotated
  , cutIntoN
  , when_, (><=)
  , rot2M, rotate2
  , clamp
  , insertIM, insertIM'
  ) where

import FreeGame
import Control.Lens
import Control.Monad.State.Strict (MonadState)
import qualified Data.Sequence as S
import qualified Data.IntMap.Strict as IM

areaTop, areaLeft, areaBottom, areaRight :: Double
areaTop = 16
areaLeft = 32
areaBottom = 444
areaRight = 416

isInside :: Vec2 -> Bool
isInside (V2 a b) = (areaLeft-40 <= a && a <= areaRight+40) && (areaTop-40 <= b && b <= areaBottom+40)

boxVertex :: Vec2 -> Vec2 -> [Vec2]
boxVertex pos size = [pos - size,
                      pos + V2 (size^._x) (-size^._y),
                      pos + size,
                      pos + V2 (-size^._x) (size^._y)]

boxVertexRotated :: Vec2 -> Vec2 -> Double -> [Vec2]
boxVertexRotated pos size angle = 
  map (pos +) $ map (\v -> v `rotate2` angle) $ boxVertex 0 size

cutIntoN :: Int -> Bitmap -> [Bitmap]
cutIntoN n img = let (w,h) = bitmapSize img; w1 = w `div` n in
  [cropBitmap img (w1,h) (w1*i,0) | i <- [0..n-1]]

when_ :: (Monad m) => m Bool -> m () -> m ()
when_ mp m = mp >>= (\b -> when b m)

(><=) :: (MonadState s m) => 
         Setting (->) s s (S.Seq a) (S.Seq a) -> (S.Seq a) -> m ()
a ><= b = a %= (S.>< b)

rot2M :: Double -> M22 Double
rot2M r = let c = cos(-r); s = sin(-r) in 
  V2 (V2 c (-s))
     (V2 s c)

rotate2 :: Vec2 -> Double -> Vec2
rotate2 v r = rot2M r !* v

clamp :: Vec2 -> Vec2
clamp (V2 x y) = V2 (edgeX x) (edgeY y)
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
