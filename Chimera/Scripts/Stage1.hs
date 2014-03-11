module Chimera.Scripts.Stage1 (
  stage1
  )
  where

import FreeGame
import Control.Lens
import Control.Monad.State.Strict (modify)
import Data.Default (def)

import Chimera.Core.World
import Chimera.Scripts

stage1 :: Stage ()
stage1 = do
  liftTalk $ do
    say' $ aline "ルーフェはお母さんからおつかいを頼まれました。"
    lufe <- character 0 $ V2 500 300
    say lufe $ 
      aline "はぁ、面倒くさいな〜。" `click`
      aline "でもおつかい断るとさらに面倒なことになるしなぁ…。" `click`
      aline "何でもいいからさっさと終わらせよっと。"
    delCharacter lufe
    say' $ aline "ルーフェは妖精の森を抜けて隣街に向かいます。"
  
    say' $ 
      aline "このChimeraというゲームは全てHaskellという言語で書かれています。" `click`
      aline "そして使っているのはOpenGLをバックエンドにもつfree-gameライブラリです。" `click`
      aline "free-gameの名前の由来にもなっているFreeモナドは、functorから最低限の構造をもったMonadを創りだすことができる道具です。" `click`
      aline "これによってChimeraではGameができStageが記述されDanmakuを形作っているのです。" `click`
      aline "そしてこの弾幕STGはより自由に、高級に、簡単に弾幕を記述することを目的として制作されています。"

--  keeper $ initEnemy (V2 320 (-40)) 100 & runAuto .~ boss3
  keeper $ initEnemy (V2 240 (-40)) 100 & runAuto .~ debug
    
  appearAt 5 $ initEnemy (V2 320 (-40)) 10 & runAuto .~ zako 10
  appearAt 5 $ initEnemy (V2 350 (-40)) 10 & runAuto .~ zako 10
  appearAt 5 $ initEnemy (V2 370 (-40)) 10 & runAuto .~ zako 10
  appearAt 5 $ initEnemy (V2 390 (-40)) 10 & runAuto .~ zako 10
  
  appearAt 5 $ initEnemy (V2 220 (-40)) 10 & runAuto .~ zako 10
  appearAt 5 $ initEnemy (V2 200 (-40)) 10 & runAuto .~ zako 10
  appearAt 5 $ initEnemy (V2 180 (-40)) 10 & runAuto .~ zako 10
  appearAt 5 $ initEnemy (V2 160 (-40)) 10 & runAuto .~ zako 10
  
  wait 350
  
  appearAt 5 $ initEnemy (V2 320 (-40)) 5 & runAuto .~ zako 20
  appearAt 5 $ initEnemy (V2 350 (-40)) 5 & runAuto .~ zako 20
  appearAt 5 $ initEnemy (V2 370 (-40)) 5 & runAuto .~ zako 20
  appearAt 5 $ initEnemy (V2 390 (-40)) 5 & runAuto .~ zako 20
  
  appearAt 5 $ initEnemy (V2 220 (-40)) 5 & runAuto .~ zako 20
  appearAt 5 $ initEnemy (V2 200 (-40)) 5 & runAuto .~ zako 20
  appearAt 5 $ initEnemy (V2 180 (-40)) 5 & runAuto .~ zako 20
  appearAt 5 $ initEnemy (V2 160 (-40)) 5 & runAuto .~ zako 20
  
  wait 20
  
  keeper $ initEnemy (V2 240 (-40)) 100 & runAuto .~ boss2
  keeper $ initEnemy (V2 240 (-40)) 100 & runAuto .~ boss1

zako :: Int -> Danmaku EnemyObject ()
zako n
  | n >= 20 = zakoCommon 0 (motionCommon 100 (Curve (acc $ n `mod` 10))) 50 Needle Purple
  | n >= 10 = zakoCommon 0 (motionCommon 100 Straight) 50 BallMedium (toEnum $ n `mod` 10)
  | otherwise = return ()
  where
    acc :: Int -> Vec2
    acc 0 = V2 (-0.05) 0.005
    acc 1 = V2 0.05 0.005
    acc _ = error "otherwise case in acc"

boss1 :: Danmaku EnemyObject ()
boss1 = do
  setName "分裂弾"
  
  e <- self
  hook $ Left $ motionCommon 100 Stay
  res <- getResource
  when (e^.counter == 130) $ effs $ return $ effEnemyStart res (e^.pos)
  when (e^.counter == 200) $
    effs $ [effEnemyAttack 0 res (e^.pos),
            effEnemyAttack 1 res (e^.pos),
            effEnemyAttack 2 res (e^.pos)]
  
  let def' = def & pos .~ e^.pos & angle .~ (fromIntegral $ e^.counter)/30
  when ((e^.counter) >= 200 && (e^.counter) `mod` 15 == 0 && e^.stateChara == Attack) $ do
    shots $ (flip map) [1..4] $ \i -> 
      makeBullet $
      speed .~ 3.15 $
      angle +~ 2*pi*i/4 $
      kind .~ Oval $
      bcolor .~ Red $
      runAuto %~ (\f -> go 190 300 >> f) $
      def'
    shots $ (flip map) [1..4] $ \i ->
      makeBullet $
      speed .~ 3 $
      angle +~ 2*pi*i/4 $
      kind .~ Oval $
      bcolor .~ Yellow $
      runAuto %~ (\f -> go 135 290 >> f) $
      def'
    shots $ (flip map) [1..4] $ \i ->
      makeBullet $
      speed .~ 2.5 $
      angle +~ 2*pi*i/4 $
      kind .~ Oval $
      bcolor .~ Green $
      runAuto %~ (\f -> go 120 280 >> f) $
      def'
    shots $ (flip map) [1..4] $ \i ->
      makeBullet $
      speed .~ 2.2 $
      angle +~ 2*pi*i/4 $
      kind .~ Oval $
      bcolor .~ Blue $
      runAuto %~ (\f -> go 100 270 >> f) $
      def'

  where
    go :: Double -> Double -> Danmaku BulletObject ()
    go t1 t2 = hook $ Left $ do
      counter %= (+1)
      cnt <- use counter
      when (30 < cnt && cnt < 200) $ do
        angle %= (+ pi/t1)
        speed %= (subtract (7.0/t2))
      when (cnt == 170) $ do
        kind .= BallTiny
        bcolor .= Purple
        modify makeBullet

boss2 :: Danmaku EnemyObject ()
boss2 = do
  setName "回転弾"
  
  e <- self
  hook $ Left $ motionCommon 100 Stay
  res <- getResource
  p <- getPlayer
  let ang = (+) (pi/2) $ uncurry atan2 $ toPair (e^.pos - p^.pos)
  when (e^.counter == 150) $
    effs $ [effEnemyAttack 0 res (e^.pos),
            effEnemyAttack 1 res (e^.pos),
            effEnemyAttack 2 res (e^.pos)]
  
  when (e^.counter `mod` 50 == 0 && e^.stateChara == Attack) $
    shots $ (flip map) [0..5] $ \i ->
      makeBullet $
      pos .~ e^.pos $
      speed .~ 2 $
      angle .~ ang + fromIntegral i*2*pi/5 $
      bcolor .~ (toEnum $ i*2 `mod` 8) $
      runAuto %~ (\f -> go i >> f) $
      def
  when (e^.counter `mod` 100 == 0 && e^.stateChara == Attack) $
    shots $ return $
      makeBullet $
      pos .~ e^.pos $
      speed .~ 1.5 $
      angle .~ ang $
      kind .~ BallLarge $
      bcolor .~ Purple $
      def
  
  where
    go :: Int -> Danmaku BulletObject ()
    go _ = do
      b <- self
      let t = pi/3
      let time = 50
      when ((b^.counter) < 200 && (b^.counter) `mod` time == 0) $
        shots $ return $ def & auto .~ b & angle +~ t
      
      hook $ Left $ do
        counter %= (+1)
        cnt <- use counter
        when (cnt < 200 && cnt `mod` time == 0) $ do
          speed += 1.5
          angle -= t
        when (cnt < 200) $ speed -= (fromIntegral $ time - cnt `mod` time)/1000

boss3 :: Danmaku EnemyObject ()
boss3 = do
  e <- self
  hook $ Left $ motionCommon 100 Stay
  p <- getPlayer
  let ang = (+) (pi/2) $ uncurry atan2 $ toPair (e^.pos - p^.pos)
  
  let n = 8 :: Int
  when (e^.counter `mod` 50 == 0 && e^.stateChara == Attack) $
    shots $ (flip map) [0..n] $ \i ->
      makeBullet $
      pos .~ e^.pos $
      speed .~ 2 $
      angle .~ ang + fromIntegral i*2*pi/fromIntegral n $
      bcolor .~ (toEnum $ i*2 `mod` 8) $
      kind .~ Needle $
      runAuto %~ (\f -> go >> f) $
      def
  
  where
    go :: Danmaku BulletObject ()
    go = do
      b <- self
      let t = pi/3
      let time = 50
      when ((b^.counter) == 200) $
        shots $ return $ def & auto .~ b & angle +~ t & kind .~ Oval
      
      hook $ Left $ do
        counter %= (+1)
        cnt <- use counter
        when (cnt < 200 && cnt `mod` time == 0) $ speed += 1.5
        when (cnt < 200) $
          speed -= (fromIntegral $ time - cnt `mod` time)/1000
