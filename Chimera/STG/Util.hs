module Chimera.STG.Util (
  Double'
  , V2, Vec, Pos
  , fromPair, toPair
  , areaTop, areaLeft, areaBottom, areaRight
  , fromPolar
  , isInside
  , boxVertex
  , cutIntoN
  , sequence_', mapM_'
  ) where

import Graphics.UI.FreeGame
import Control.Lens
import qualified Data.Sequence as S
import qualified Data.Foldable as F

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

cutIntoN :: Int -> Bitmap -> [Bitmap]
cutIntoN n img = let (w,h) = bitmapSize img; w1 = w `div` n in
  [cropBitmap img (w1,h) (w1*i,0) | i <- [0..n-1]]

sequence_' :: Monad m => S.Seq (m a) -> m () 
sequence_' ms = F.foldr (>>) (return ()) ms

mapM_' :: Monad m => (a -> m b) -> S.Seq a -> m ()
mapM_' f as = sequence_' (fmap f as)
