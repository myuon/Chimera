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
import Data.Functor.Product
import Data.Reflection (given)

import Chimera.Core.Types as M
import Chimera.Core.Util as M

instance GUIClass Player where
  update = do
    counter %= (+1)
    do
      sp <- use spXY
      pos += sp
    spXY .= 0
    pos %= clamp

    do
      s <- use speed
      k <- use keysPlayer
      spXY .= case k M.! KeyLeftShift > 0 || k M.! KeyRightShift > 0 of
        True -> ((0.5 * s) *^ dir k)
        False -> s *^ dir k

    where
      dir :: M.Map Key Int -> Vec2
      dir k = let addTup b p q = bool q (uncurry V2 p+q) b in
        addTup (k M.! KeyUp    > 0) (0,-1) $
        addTup (k M.! KeyDown  > 0) (0,1) $
        addTup (k M.! KeyRight > 0) (1,0) $
        addTup (k M.! KeyLeft  > 0) (-1,0) $
        0

  paint = do
    p <- get
    let resource = given :: Resource
    draw $ translate (p^.pos) $ bitmap $ (resource^.charaImg) V.! 0

instance GUIClass Effect where
  update = do
    run <- use runAuto
    effectObject %= execState run
  
  paint = do
    b <- get
    let res = given :: Resource
    lift $ translate (b^.pos) $ rotateR (b^.ang) $ scale (b^.size) $ b^.img $ res

instance GUIClass Bullet where
  update = do
    r <- use speed
    t <- use ang
    pos += rotate2 (V2 r 0) t
    p <- use pos
    unless (p `isInside` expand (config^.gameArea) (Box (-40) 40)) $ stateBullet .= Outside

    where
      config = given :: Config

      expand (Box v1 v2) (Box v3 v4) = Box (v1+v2) (v3+v4)

  paint = do
    let res = given :: Resource
    b <- get
    case b^.size^._x /= b^.size^._y of
      True -> draw $ translate (b^.pos) $ 
              rotateR (b^.ang + pi/2) $ bitmap $ getBulletBitmap (res^.bulletImg) (b^.kind) (b^.bcolor)
      False -> draw $ translate (b^.pos) $ bitmap $ getBulletBitmap (res^.bulletImg) (b^.kind) (b^.bcolor)

    where
      getBulletBitmap :: V.Vector (V.Vector Bitmap) -> BKind -> BColor -> Bitmap
      getBulletBitmap imgs bk bc = imgs V.! (fromEnum bk) V.! (fromEnum bc)

instance GUIClass Enemy where
  update = do
    sp <- use spXY
    pos %= (+sp)
    counter %= (+1)
    h <- use hp
    when (h <= 0) $ stateChara .= Dead
  
  paint = do
    let res = given :: Resource
    c <- get
    draw $ translate (c^.pos) $ bitmap $ (res^.charaImg) V.! 1

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

areaBullet :: BKind -> Vec2
areaBullet BallLarge = V2 15 15
areaBullet BallMedium = V2 7 7
areaBullet BallSmall = V2 4 4
areaBullet Oval = V2 7 3
areaBullet Diamond = V2 5 3
areaBullet BallFrame = V2 5 5
areaBullet Needle = V2 30 1
areaBullet BallTiny = V2 2 2
