{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts, RankNTypes #-}
module Chimera.Core.World (
  actPlayer, makeBullet, runDanmaku, scanAutonomies
  , module M
  ) where

import FreeGame
import Control.Lens
import Control.Monad.State.Strict (State, StateT, get, execState, lift, modify)
import qualified Data.Vector as V
import qualified Data.Sequence as S
import qualified Data.Map as M
import qualified Data.Traversable as T
import Data.Default
import Data.Functor.Product

import Chimera.Core.Types as M
import Chimera.Core.Util as M
import Chimera.Core.Load (picture, GetPicture, areaBullet, getBulletBitmap)

instance GetPicture Player where
  picture res _ = (res^.charaImg) V.! 0

instance GetPicture Bullet where 
  picture res b = getBulletBitmap (res^.bulletImg) (b^.kind) (b^.bcolor)

instance GetPicture Enemy where
  picture res _ = (res^.charaImg) V.! 1

instance GUIClass Player where
  update = do
    s <- use speed
    k <- use keysPlayer
    counter %= (+1)
    pos += case k M.! KeyLeftShift > 0 || k M.! KeyRightShift > 0 of
      True -> ((0.5 * s) *^ dir k)
      False -> s *^ dir k
    pos %= clamp

    where
      dir :: M.Map Key Int -> Vec2
      dir k = let addTup b p q = bool q (fromPair p+q) b in
        addTup (k M.! KeyUp    > 0) (0,-1) $
        addTup (k M.! KeyDown  > 0) (0,1) $
        addTup (k M.! KeyRight > 0) (1,0) $
        addTup (k M.! KeyLeft  > 0) (-1,0) $
        fromPair (0,0)

  paint res = do
    p <- get
    draw $ translate (p^.pos) $ bitmap (picture res p)

instance GUIClass Effect where
  update = do
    run <- use runAuto
    effectObject %= execState run
  
  paint res = do
    b <- get
    lift $ translate (b^.pos) $ rotateR (b^.angle) $ scale (b^.size) $ b^.img $ res

instance GUIClass Bullet where
  update = do
    r <- use speed
    t <- use angle
    pos %= (+ fromPolar (r,t))
    p <- use pos
    unless (isInside p) $ stateBullet .= Outside

  paint res = do
    b <- get
    case b^.size^._x /= b^.size^._y of
      True -> draw $ translate (b^.pos) $ 
              rotateR (b^.angle + pi/2) $ bitmap (picture res b)
      False -> draw $ translate (b^.pos) $ bitmap (picture res b)

instance GUIClass Enemy where
  update = do
    sp <- use spXY
    pos %= (+sp)
    counter %= (+1)
    h <- use hp
    when (h <= 0) $ stateChara .= Dead
  
  paint res = do
    c <- get
    draw $ translate (c^.pos) $ bitmap (picture res c)

actPlayer :: StateT Player Game ()
actPlayer = do
  pairs <- lift $ mapM (\k -> (,) k `fmap` fromEnum `fmap` keyPress k) keyList
  keysPlayer %= M.unionWith go (M.fromList pairs)
  where
    go a b
      | a == 0 = 0
      | otherwise = a + b

makeBullet :: (HasObject c, HasBulletObject c) => c -> c
makeBullet b = b & size .~ areaBullet (b^.kind)

runDanmaku :: c -> Field -> Danmaku c () -> Product (State c) (State Field) ()
runDanmaku = runLookAtAll

scanAutonomies :: Lens' Field (S.Seq (Autonomie (Danmaku a) a)) -> State Field ()
scanAutonomies member = do
  f <- use id
  let pairs = fmap (\c -> runDanmaku (c^.auto) f (c^.runAuto)) $ f^.member
  member .= (fmap (\(b,s) -> b & auto %~ execState s) $ 
             S.zip (f^.member) $ fmap (\(Pair a _) -> a) pairs)
  modify $ execState $ T.mapM (\(Pair _ b) -> b) pairs

