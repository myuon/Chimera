{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances, FunctionalDependencies, ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
module Chimera.STG.Util (
  Double'
  , V2, Vec, Pos
  , fromPair, toPair
  , areaTop, areaLeft, areaBottom, areaRight
  , fromPolar
  , isInside
  , boxVertex, boxVertexRotated
  , cutIntoN
  , sequence_', mapM_', (><=)
  , rot2D
  , Autonomie(..), autonomie, auto, runAuto, Autonomic
  ) where

import Graphics.UI.FreeGame
import Control.Lens
import Control.Monad.State.Strict (MonadState)
import Data.Default
import qualified Data.Sequence as S
import qualified Data.Foldable as F
import Data.Char (toLower)

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

(><=) :: (MonadState s m) => 
         Setting (->) s s (S.Seq a) (S.Seq a) -> (S.Seq a) -> m ()
a ><= b = a %= (S.><) b

rot2D :: Double' -> M22 Double'
rot2D r = V2
          (V2 (cos(-r)) (-sin(-r)))
          (V2 (sin(-r)) (cos(-r)))

data Autonomie m a = Autonomie {
  _auto :: a,
  _runAuto :: m ()
  }

makeLensesFor [("_auto", "__auto"),
               ("_runAuto", "__runAuto")] ''Autonomie

instance (Monad m, Default a) => Default (Autonomie m a) where
  def = Autonomie {
    _auto = def,
    _runAuto = return ()
    }

class Autonomic c m a | c -> a, c -> m where
  autonomie :: Lens' c (Autonomie m a)

instance Autonomic (Autonomie m a) m a where
  autonomie = id

auto :: forall c m a. (Autonomic c m a) => Lens' c a
auto = autonomie . __auto

runAuto :: forall c m a. (Autonomic c m a) => Lens' c (m ())
runAuto = autonomie . __runAuto

instance (Eq a) => Eq (Autonomie m a) where
  a == b = a^.auto == b^.auto

instance (Show a) => Show (Autonomie m a) where
  show a = show $ a^.auto
  
