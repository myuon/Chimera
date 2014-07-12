{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances, FunctionalDependencies, ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
module Chimera.Engine.Core.Util (
  boxVertex, boxVertexRotated
  , cutIntoN
  , (><=)
  , rot2M, rotate2
  , insertIM, insertIM'
  ) where

import FreeGame
import Control.Lens
import Control.Monad.State.Strict (MonadState)
import qualified Data.Sequence as S
import qualified Data.IntMap.Strict as IM

boxVertex :: Vec2 -> Vec2 -> [Vec2]
boxVertex pos size = [pos - size,
                      pos + V2 (size^._x) (-size^._y),
                      pos + size,
                      pos + V2 (-size^._x) (size^._y)]

boxVertexRotated :: Vec2 -> Vec2 -> Double -> [Vec2]
boxVertexRotated pos size ang = 
  map (pos +) $ map (\v -> v `rotate2` ang) $ boxVertex 0 size

cutIntoN :: Int -> Bitmap -> [Bitmap]
cutIntoN n img = let (w,h) = bitmapSize img; w1 = w `div` n in
  [cropBitmap img (w1,h) (w1*i,0) | i <- [0..n-1]]

(><=) :: (MonadState s m) => 
         Setting (->) s s (S.Seq a) (S.Seq a) -> (S.Seq a) -> m ()
a ><= b = a %= (S.>< b)

rot2M :: Double -> M22 Double
rot2M r = let c = cos(-r); s = sin(-r) in 
  V2 (V2 c (-s))
     (V2 s c)

rotate2 :: Vec2 -> Double -> Vec2
rotate2 v r = rot2M r !* v

insertIM :: a -> IM.IntMap a -> IM.IntMap a
insertIM a m = snd $ insertIM' a m

insertIM' :: a -> IM.IntMap a -> (Int, IM.IntMap a)
insertIM' a m = case n `IM.member` m of
  True -> (n', IM.insert n' a m)
  False -> (n, IM.insert n a m)
  
  where
    n = IM.size m
    n' = head [x | x <- [0..], x `IM.notMember` m]
