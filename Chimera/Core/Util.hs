{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances, FunctionalDependencies, ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
module Chimera.Core.Util (
  Double'
  , V2, Vec, Pos
  , fromPair, toPair
  , areaTop, areaLeft, areaBottom, areaRight
  , fromPolar
  , isInside
  , boxVertex, boxVertexRotated
  , cutIntoN
  , sequence_', mapM_', when_, (><=)
  , rot2D
  , clamp
  ) where

import Graphics.UI.FreeGame
import Control.Lens
import Control.Arrow ((***))
import Control.Monad.State.Strict (MonadState)
import qualified Data.Sequence as S
import qualified Data.Foldable as F

type Double' = Float

areaTop, areaLeft, areaBottom, areaRight :: Double'
areaTop = 16
areaLeft = 32
areaBottom = 444
areaRight = 416

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

boxVertexRotated :: Vec -> Vec -> Double' -> [Vec]
boxVertexRotated pos size angle = let r = rot2D angle in
  map (pos +) $ map (r !*) $ boxVertex 0 size

cutIntoN :: Int -> Bitmap -> [Bitmap]
cutIntoN n img = let (w,h) = bitmapSize img; w1 = w `div` n in
  [cropBitmap img (w1,h) (w1*i,0) | i <- [0..n-1]]

sequence_' :: Monad m => S.Seq (m a) -> m () 
sequence_' ms = F.foldr (>>) (return ()) ms

mapM_' :: Monad m => (a -> m b) -> S.Seq a -> m ()
mapM_' f as = sequence_' (fmap f as)

when_ :: (Monad m) => m Bool -> m () -> m ()
when_ mp m = mp >>= (\b -> when b m)

(><=) :: (MonadState s m) => 
         Setting (->) s s (S.Seq a) (S.Seq a) -> (S.Seq a) -> m ()
a ><= b = a %= (S.>< b)

rot2D :: Double' -> M22 Double'
rot2D r = V2
          (V2 (cos(-r)) (-sin(-r)))
          (V2 (sin(-r)) (cos(-r)))

clamp :: Vec -> Vec
clamp = fromPair . (edgeX *** edgeY) . toPair
  where
    edgeX = (\p -> bool p areaLeft (p < areaLeft)) .
            (\p -> bool p areaRight (p > areaRight))
    
    edgeY = (\p -> bool p areaLeft (p < areaLeft)) .
            (\p -> bool p areaBottom (p > areaBottom))
