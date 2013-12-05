module Chimera.Scripts.Stage1 (
  load1, stage1, zako, boss
  )
  where

import Graphics.UI.FreeGame
import Control.Lens
import Control.Monad
import Control.Monad.Operational.Mini
import Control.Monad.State.Strict (get, put, execStateT, State, StateT)

import Chimera.STG.Util
import Chimera.STG.World
import Chimera.Load
import Chimera.Scripts

load1 :: Resource
load1 = def

stage1 :: Stage ()
stage1 = do
  res <- getResource

  appearAt 30 $ initEnemy (V2 320 (-40)) 2 res (Zako 1 10) & runAuto .~ zako 20
  appearAt 30 $ initEnemy (V2 240 (-40)) 2 res (Boss 1 0) & runAuto .~ boss 1

zako :: Int -> Danmaku ()
zako n
  | n >= 20 = zakoCommon 0 (motionCommon 100 (Curve (acc $ n `mod` 10))) 50 Needle Purple
  | n >= 10 = zakoCommon 0 (motionCommon 100 (Straight)) 50 BallMedium (toEnum $ n `mod` 10)
  | otherwise = return ()
  where
    acc :: Int -> Vec
    acc 0 = V2 (-0.05) 0.005
    acc 1 = V2 0.05 0.005

boss :: Int -> Danmaku ()
boss _ = do
  e <- get'
  put' $ motionCommon 100 Stay e
  res <- getResource
  p <- getPlayer

  let def' = pos .~ e^.pos $ angle .~ (fromIntegral $ e^.counter)/30 $ def
  when ((e^.counter) `mod` 15 == 0 && e^.stateEnemy == Attack) $ do
    shots $ (flip map) [1..4] $ \i ->
      speed .~ 3.15 $
      angle +~ 2*pi*i/4 $
      img .~ (bulletBitmap Oval Red (snd $ res^.bulletImg)) $
      runAuto %~ (\f -> go 190 300 >> f) $
      def'
    shots $ (flip map) [1..4] $ \i ->
      speed .~ 3 $
      angle +~ 2*pi*i/4 $
      img .~ (bulletBitmap Oval Yellow (snd $ res^.bulletImg)) $
      runAuto %~ (\f -> go 135 290 >> f) $
      def'
    shots $ (flip map) [1..4] $ \i ->
      speed .~ 2.5 $
      angle +~ 2*pi*i/4 $
      img .~ (bulletBitmap Oval Green (snd $ res^.bulletImg)) $
      runAuto %~ (\f -> go 120 280 >> f) $
      def'
    shots $ (flip map) [1..4] $ \i ->
      speed .~ 2.2 $
      angle +~ 2*pi*i/4 $
      img .~ (bulletBitmap Oval Blue (snd $ res^.bulletImg)) $
      runAuto %~ (\f -> go 70 270 >> f) $
      def'

  where
    go :: Double' -> Double' -> State BulletObject ()
    go t1 t2 = do
      counter %= (+1)
      cnt <- use counter
      when (30 < cnt && cnt < 200) $ do
        angle %= (+ pi/t1)
        speed %= (subtract (7.0/t2))
