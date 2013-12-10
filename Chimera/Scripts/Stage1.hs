module Chimera.Scripts.Stage1 (
  load1, stage1, zako, boss
  )
  where

import Graphics.UI.FreeGame
import Control.Lens
import Control.Monad
import Control.Monad.Operational.Mini
import Control.Monad.State.Strict (get, put, execState, State)
import qualified Data.Vector as V

import Chimera.STG.Util
import Chimera.STG.World
import Chimera.Load
import Chimera.Scripts

load1 :: Resource
load1 = def

stage1 :: Stage ()
stage1 = do
  res <- getResource

  appearAt 30 $ initEnemy (V2 320 (-40)) 2 res & runAuto .~ zako 20
  appearAt 30 $ initEnemy (V2 240 (-40)) 2 res & runAuto .~ boss 1

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
  put' $ motionCommon 100 Stay `execState` e
  res <- getResource
  p <- getPlayer
  when (e^.counter == 130) $ effs $ return $ effStart res (e^.pos)
  when (e^.counter == 200) $
    charaEffs $ [effAttack 0 res (e^.pos),
                 effAttack 1 res (e^.pos),
                 effAttack 2 res (e^.pos)]
  
  let go = go' res
  let def' = pos .~ e^.pos $ angle .~ (fromIntegral $ e^.counter)/30 $ def
  when ((e^.counter) >= 200 && (e^.counter) `mod` 15 == 0 && e^.stateInt == fromEnum Attack) $ do
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
      runAuto %~ (\f -> go 100 270 >> f) $
      def'

  where
    effStart :: Resource -> Vec -> Effect
    effStart res p =
      pos .~ p $
      ress .~ (res^.effectImg) V.! 2 $
      size .~ V2 0.8 0.8 $
      slowRate .~ 6 $
      runAuto .~ run $
      (def :: Effect)

      where
        run :: State EffectObject ()
        run = do
          f <- get
          res <- use ress
          let i = (f^.counter) `div` (f^.slowRate)
          img .= res V.! i
          size *= 1.01
          counter %= (+1)
          when (i == V.length res) $ stateInt .= fromEnum Inactive
    
    effAttack :: Int -> Resource -> Vec -> Effect
    effAttack i res p =
      pos .~ p $
      img .~ (res^.effectImg) V.! 3 V.! i $
      runAuto .~ run $
      (def :: Effect)

      where
        run :: State EffectObject ()
        run = do
          f <- get
          when (f^.counter <= 50) $ size += 1/50
          res <- use ress
          angle += anglePlus i
          counter %= (+1)
        
        anglePlus :: Int -> Double'
        anglePlus 0 = 1/300
        anglePlus 1 = -2/300
        anglePlus 2 = 3/300
        

    go' :: Resource -> Double' -> Double' -> State BulletObject ()
    go' res t1 t2 = do
      counter %= (+1)
      cnt <- use counter
      when (30 < cnt && cnt < 200) $ do
        angle %= (+ pi/t1)
        speed %= (subtract (7.0/t2))
      when (cnt == 170) $ do
        img .= bulletBitmap BallTiny Purple (snd $ res^.bulletImg)
