module Chimera.Scripts (
  shots, getPlayer, get', put'
  , MotionCommon(..)
  , motionCommon
  , zakoCommon
  , doBulletCommon
  , debug
  ) where

import Graphics.UI.FreeGame
import Control.Lens
import Control.Monad
import Control.Monad.Operational.Mini
import Control.Monad.State.Strict (get, put, execStateT, State, StateT)

import Chimera.STG.Util
import Chimera.STG.Types
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

data MotionCommon = Straight | Affine Vec | Curve Vec | Stay

motionCommon :: Int -> MotionCommon -> Enemy -> Enemy
motionCommon stay (Straight) = go
  where
    go e | e^.counter == 0 = spXY .~ V2 0 1.5 $ e
         | e^.counter == 120 = spXY .~ 0 $ stateEnemy .~ Attack $ e
         | e^.counter == stay + 120 = spXY .~ V2 0 (-1.5) $ stateEnemy .~ Alive $ e
         | e^.counter == stay + 300 = stateEnemy .~ Dead $ e
         | otherwise = e
motionCommon stay (Affine v) = go
  where
    go e | e^.counter == 0 = spXY .~ V2 0 1.5 $ e
         | e^.counter == 120 = spXY .~ 0 $ stateEnemy .~ Attack $ e
         | e^.counter == stay + 120 = spXY .~ v $ e
         | e^.counter == stay + 300 = stateEnemy .~ Dead $ e
         | otherwise = e
motionCommon _ (Curve acc) = go
  where
    go e | e^.counter == 0 = spXY .~ V2 0 3 $ stateEnemy .~ Attack $ e
         | e^.counter == 300 = stateEnemy .~ Dead $ e
         | otherwise = spXY %~ (+ acc) $ e
motionCommon _ Stay = go
  where
    go e | e^.counter == 0 = spXY .~ V2 0 1.5 $ e
         | e^.counter == 120 = spXY .~ 0 $ stateEnemy .~ Attack $ e
         | otherwise = e

zakoCommon :: Int -> (Enemy -> Enemy) -> Int -> BKind -> BColor -> Danmaku ()
zakoCommon 0 mot time bk c = do
  e <- get'
  put' $ mot e
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

doBulletCommon :: Int -> State Bullet ()
doBulletCommon 0 = do
  r <- use speed
  t <- use angle
  pos %= (+ fromPolar (r,t))

debug :: Danmaku ()
debug = do
  e <- get'
  put' $ motionCommon 0 Stay e
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

