module Chimera.Scripts.Stage1 (
  load1, stage1
  )
  where

import Graphics.UI.FreeGame
import Control.Lens
import Control.Monad
import Control.Monad.Operational.Mini
import Control.Monad.State.Strict (get, put, execState, State)
import Control.Monad.Trans.Class (lift)
import qualified Data.Vector as V
import qualified Data.Sequence as S

import Chimera.STG.Util
import Chimera.STG.World
import Chimera.Load
import Chimera.Scripts

load1 :: Resource
load1 = def

stage1 :: Stage ()
stage1 = do
  res <- getResource

  keeper $ initEnemy (V2 240 (-40)) 2 res & runAuto .~ boss2
  appearAt 30 $ initEnemy (V2 320 (-40)) 2 res & runAuto .~ zako 20
  keeper $ initEnemy (V2 240 (-40)) 2 res & runAuto .~ boss1

zako :: Int -> Danmaku EnemyObject ()
zako n
  | n >= 20 = zakoCommon 0 (motionCommon 100 (Curve (acc $ n `mod` 10))) 50 Needle Purple
  | n >= 10 = zakoCommon 0 (motionCommon 100 (Straight)) 50 BallMedium (toEnum $ n `mod` 10)
  | otherwise = return ()
  where
    acc :: Int -> Vec
    acc 0 = V2 (-0.05) 0.005
    acc 1 = V2 0.05 0.005

boss1 :: Danmaku EnemyObject ()
boss1 = do
  e <- get'
  liftS $ motionCommon 100 Stay
  res <- getResource
  when (e^.counter == 130) $ effs $ return $ effStart res (e^.pos)
  when (e^.counter == 200) $
    effs $ [effAttack 0 res (e^.pos),
            effAttack 1 res (e^.pos),
            effAttack 2 res (e^.pos)]
  
  let go = go' res
  let def' = pos .~ e^.pos $ angle .~ (fromIntegral $ e^.counter)/30 $ (def :: Bullet)
  when ((e^.counter) >= 200 && (e^.counter) `mod` 15 == 0 && e^.stateChara == Attack) $ do
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
      def

      where
        run :: State EffectObject ()
        run = do
          f <- get
          res <- use ress
          let i = (f^.counter) `div` (f^.slowRate)
          img .= res V.! i
          size *= 1.01
          counter %= (+1)
          when (i == V.length res - 1) $ stateEffect .= Inactive
    
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
        
    go' :: Resource -> Double' -> Double' -> Danmaku BulletObject ()
    go' res t1 t2 = liftS $ do
      counter %= (+1)
      cnt <- use counter
      when (30 < cnt && cnt < 200) $ do
        angle %= (+ pi/t1)
        speed %= (subtract (7.0/t2))
      when (cnt == 170) $ do
        img .= bulletBitmap BallTiny Purple (snd $ res^.bulletImg)
  
boss2 :: Danmaku EnemyObject ()
boss2 = do
  e <- get'
  liftS $ motionCommon 100 Stay
  res <- getResource
  p <- getPlayer
  let ang = (+) (pi/2) $ uncurry atan2 $ toPair (e^.pos - p^.pos)
  let go = go' res
  when (e^.counter == 150) $
    effs $ [effAttack 0 res (e^.pos),
            effAttack 1 res (e^.pos),
            effAttack 2 res (e^.pos)]
  
  when (e^.counter `mod` 50 == 0 && e^.stateChara == Attack) $ do
    shots $ (flip map) [0..5] $ \i ->
      pos .~ e^.pos $
      speed .~ 2 $
      angle .~ ang + fromIntegral i*2*pi/5 $
      img .~ (bulletBitmap BallMedium (toEnum $ i*2 `mod` 8) (snd $ res^.bulletImg)) $
      runAuto %~ (\f -> go i >> f) $
      def
  when (e^.counter `mod` 100 == 0 && e^.stateChara == Attack) $ do
    shots $ return $
      pos .~ e^.pos $
      speed .~ 1.5 $
      angle .~ ang $
      img .~ (bulletBitmap BallLarge Purple (snd $ res^.bulletImg)) $
      def
  
  where
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

    go' :: Resource -> Int -> Danmaku BulletObject ()
    go' res n = do
      b <- get'
      let t = pi/(3)
      let time = 50
      when ((b^.counter) < 200 && (b^.counter) `mod` time == 0) $ do
        b <- getLocal
        shots $ return $ def & auto .~ b & angle +~ t
      
      liftS $ do
        counter %= (+1)
        cnt <- use counter
        when (cnt < 200 && cnt `mod` time == 0) $ do
          speed += 1.5
          angle -= t
        when (cnt < 200) $ do
          speed -= (fromIntegral $ time - cnt `mod` time)/1000
