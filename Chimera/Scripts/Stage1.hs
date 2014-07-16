{-# LANGUAGE FlexibleContexts #-}
module Chimera.Scripts.Stage1 (
  stage1
  )
  where

import FreeGame
import Control.Lens
import Control.Monad.State.Strict (modify)
import Data.Default (def)
import Data.Reflection (Given, given)

import Chimera.Engine.Core
import Chimera.Engine.Scripts
import Chimera.Scripts.Common

stage1 :: (Given Resource) => Stage ()
stage1 = do
  addEffect effPlayerBack
  talk $ do
    say' $ aline "メッセージのテスト"
    lufe <- character 0 $ V2 500 300
    say lufe $ 
      aline "こんにちは。" `click`
      aline "ルーフェです。"
    delCharacter lufe
    say' $ aline "メッセージのテストを終わります。"

  keeper $ initEnemy (V2 240 (-40)) 100 & runAuto .~ debug
  keeper $ initEnemy (V2 320 (-40)) 100 & runAuto .~ boss4
    
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

zako :: (Given Resource, HasChara c, HasPiece c, HasObject c) => Int -> Danmaku c ()
zako n
  | n >= 20 = zakoCommon 0 (motionCommon 100 (Curve (acc $ n `mod` 10))) 50 Needle Purple
  | n >= 10 = zakoCommon 0 (motionCommon 100 Straight) 50 BallMedium (toEnum $ n `mod` 10)
  | otherwise = return ()
  where
    acc :: Int -> Vec2
    acc 0 = V2 (-0.05) 0.005
    acc 1 = V2 0.05 0.005
    acc _ = error "otherwise case in acc"

boss1 :: (Given Resource, HasChara c, HasPiece c, HasObject c) => Danmaku c ()
boss1 = do
  setName "回転弾"
  
  e <- self
  hook $ Left $ motionCommon 100 Stay
  when (e^.counter == 130) $ effs $ return $ effEnemyStart (e^.pos)
  when (e^.counter == 200) $ do
    mapM_ enemyEffect $ [
      effEnemyAttack 0 (e^.pos),
      effEnemyAttack 1 (e^.pos),
      effEnemyAttack 2 (e^.pos)]
  
  let def' = def & pos .~ e^.pos & ang .~ (fromIntegral $ e^.counter)/30
  when ((e^.counter) >= 200 && (e^.counter) `mod` 15 == 0 && e^.statePiece == Attack) $ do
    shots $ (flip map) [1..4] $ \i -> 
      makeBullet Oval Red def'
        & speed .~ 3.15 & ang +~ 2*pi*i/4
        & runAuto %~ (go 190 300 >>)
    shots $ (flip map) [1..4] $ \i ->
      makeBullet Oval Yellow def'
        & speed .~ 3 & ang +~ 2*pi*i/4
        & runAuto %~ (go 135 290 >>)
    shots $ (flip map) [1..4] $ \i ->
      makeBullet Oval Green def'
        & speed .~ 2.5 & ang +~ 2*pi*i/4
        & runAuto %~ (go 120 280 >>)
    shots $ (flip map) [1..4] $ \i ->
      makeBullet Oval Blue def'
        & speed .~ 2.2 & ang +~ 2*pi*i/4
        & runAuto %~ (go 100 270 >>)

  where
    go t1 t2 = hook $ Left $ do
      cnt <- use counter
      when (30 < cnt && cnt < 200) $ do
        ang %= (+ pi/t1)
        speed %= (subtract (7.0/t2))
      when (cnt == 170) $ id %= makeBullet BallTiny Purple

boss2 :: (Given Resource, HasChara c, HasPiece c, HasObject c) => Danmaku c ()
boss2 = do
  setName "分裂弾"
  
  e <- self
  hook $ Left $ motionCommon 100 Stay
  p <- getPlayer
  ang' <- anglePlayer
  when (e^.counter == 150) $
    mapM_ enemyEffect $ [
      effEnemyAttack 0 (e^.pos),
      effEnemyAttack 1 (e^.pos),
      effEnemyAttack 2 (e^.pos)]
  
  when (e^.counter `mod` 50 == 0 && e^.statePiece == Attack) $
    shots $ (flip map) [0..5] $ \i ->
      makeBullet BallMedium (toEnum $ i*2 `mod` 8) def
        & pos .~ e^.pos & speed .~ 2
        & ang .~ ang' + fromIntegral i*2*pi/5 & runAuto %~ (go >>)
  when (e^.counter `mod` 100 == 0 && e^.statePiece == Attack) $
    shots $ return $ makeBullet BallLarge Purple def
      & pos .~ e^.pos & speed .~ 1.5 & ang .~ ang'
  
  where
    go = do
      b <- self
      let t = pi/3
      let time = 50
      when ((b^.counter) < 200 && (b^.counter) `mod` time == 0) $
        shots $ return $ def & auto .~ b & ang +~ t
      
      hook $ Left $ do
        cnt <- use counter
        when (cnt < 200 && cnt `mod` time == 0) $ do
          speed += 1.5
          ang -= t
        when (cnt < 200) $ speed -= (fromIntegral $ time - cnt `mod` time)/1000

boss3 :: (Given Resource, HasChara c, HasPiece c, HasObject c) => Danmaku c ()
boss3 = do
  setName "爆発弾"

  e <- self
  hook $ Left $ motionCommon 100 Stay
  p <- getPlayer
  ang' <- anglePlayer
  when (e^.counter == 150) $ do
    enemyEffect $ effEnemyAttack 0 (e^.pos)
    enemyEffect $ effEnemyAttack 1 (e^.pos)
    enemyEffect $ effEnemyAttack 2 (e^.pos)
  
  let n = 8 :: Int
  when (e^.counter `mod` 100 == 0 && e^.statePiece == Attack) $ do
    shots $ (flip map) [0..n] $ \i ->
      makeBullet Needle (toEnum $ i*2 `mod` 2) def
        & pos .~ e^.pos & speed .~ 3
        & ang .~ ang' + fromIntegral i*2*pi/fromIntegral n
        & runAuto %~ (go >>)
  
  where
    go = do
      hook $ Left $ do
        use counter >>= \c -> when (c <= 150) $ speed -= 0.01
        counter += 1

      b <- self
      let n = 8 :: Int
      when (b^.counter == 150) $
        shots $ flip map [0..n] $ \i ->
          makeBullet BallLarge Blue def
            & auto .~ b & speed .~ 1.5
            & ang .~ fromIntegral i*2*pi/fromIntegral n

boss4 :: (Given Resource, HasPiece c, HasObject c, HasChara c) =>
         Danmaku c ()
boss4 = do
  setName "ホーミング弾"

  e <- self
  hook $ Left $ motionCommon 100 Stay
  p <- getPlayer
  ang' <- anglePlayer
  when (e^.counter == 150) $ do
    enemyEffect $ effEnemyAttack 0 (e^.pos)
    enemyEffect $ effEnemyAttack 1 (e^.pos)
    enemyEffect $ effEnemyAttack 2 (e^.pos)
  
  let n = 3 :: Int
  when (e^.counter `mod` 50 == 0 && e^.statePiece == Attack) $ do
    shots $ (flip map) [0..n] $ \i ->
      makeBullet Oval (toEnum $ i*2 `mod` 2) def
        & pos .~ e^.pos & speed .~ 5
        & ang .~ ang' + fromIntegral i*2*pi/fromIntegral n
        & runAuto %~ (go >>)
  
  where
    go = do
      ang' <- anglePlayer
      hook $ Left $ do
        use counter >>= \c -> when (c <= 50) $ speed -= 0.02
        counter += 1

        b <- use ang
        let V2 ax ay = unitV2 ang'; V2 bx by = unitV2 b
        use counter >>= \c -> when (c `mod` 5 == 0) $ do
          ang += case ax*by - ay*bx > 0 of
                   True -> -10*pi/180
                   False -> 10*pi/180
