module Chimera.Scripts (
  shots, getPlayer, get', put'
  , initEnemy
  , MotionCommon(..)
  , motionCommon
  , zakoCommon
  , debug
  ) where

import Graphics.UI.FreeGame
import Control.Lens
import Control.Monad
import Control.Monad.Operational.Mini
import Control.Monad.State.Strict (get, put, execState, State)

import Chimera.STG.Util
import Chimera.STG.World
import Chimera.Load

shots :: [Bullet] -> Danmaku ()
shots = singleton . Shots

getPlayer :: Danmaku Player
getPlayer = singleton $ GetPlayer

get' :: Danmaku Enemy
get' = singleton $ Get

put' :: Enemy -> Danmaku ()
put' = singleton . Put

initEnemy :: Vec -> Int -> Resource -> KindEnemy -> Enemy
initEnemy p h res k =
  pos .~ p $
  hp .~ h $
  img .~ (snd $ res^.charaImg) $
  kindEnemy .~ k $
  def

data MotionCommon = Straight | Affine Vec | Curve Vec | Stay

motionCommon :: Int -> MotionCommon -> State Enemy ()
motionCommon time (Straight) = do
  c <- use counter
  when (c == 0) $ spXY .= V2 0 1.5
  when (c == 120) $ do
    spXY .= 0
    stateEnemy .= Attack
  when (c == time + 120) $ do
    spXY .= V2 0 (-1.5)
    stateEnemy .= Alive
  when (c > time + 300) $ stateEnemy .= Dead
motionCommon time (Affine v) = do
  c <- use counter
  when (c == 0) $ spXY .= V2 0 1.5
  when (c == 120) $ do
    spXY .= 0
    stateEnemy .= Attack
  when (c == time + 120) $ do
    spXY .= v
    stateEnemy .= Alive
  when (c > time + 300) $ stateEnemy .= Dead
motionCommon _ (Curve acc) = do
  c <- use counter
  when (c == 0) $ do
    spXY .= V2 0 3
    stateEnemy .= Attack
  when (c > 300) $ stateEnemy .= Dead
  spXY %= (+ acc)
motionCommon _ (Stay) = do
  c <- use counter
  when (c == 0) $ spXY .= V2 0 1.5
  when (c == 120) $ do
    spXY .= 0
    stateEnemy .= Attack

zakoCommon :: Int -> State Enemy () -> Int -> BKind -> BColor -> Danmaku ()
zakoCommon 0 mot time bk c = do
  e <- get'
  put' $ mot `execState` e
  res <- getResource
  p <- getPlayer
  let ang = (+) (pi/2) $ uncurry atan2 $ toPair (e^.pos - p^.pos)

  when ((e^.counter) `mod` time == 0 && e^.stateEnemy == Attack) $
    shots $ [
      pos .~ (e^.pos) $ 
      speed .~ 3 $
      angle .~ ang $
      img .~ (bulletBitmap bk c (snd $ res^.bulletImg)) $
      def]

debug :: Danmaku ()
debug = do
  e <- get'
  put' $ motionCommon 0 Stay `execState` e
  res <- getResource
  p <- getPlayer
  let cnt = e ^. counter
  let n = 20

  when (cnt `mod` 4 == 0 && e ^. spXY == 0) $
    shots $ (flip map) [1..n] $ \i ->
      pos .~ (e^.pos) $ 
      speed .~ 0.5 $
      angle .~ i*2*pi/n + (fromIntegral cnt)/100 $
      img .~ (bulletBitmap BallTiny Red (snd $ res^.bulletImg)) $
      def

