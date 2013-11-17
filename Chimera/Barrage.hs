{-# LANGUAGE TemplateHaskell, RankNTypes, GADTs, FlexibleContexts #-}
module Chimera.Barrage (
  barrage
  ) where

import Graphics.UI.FreeGame
import Control.Monad.Operational.Mini (singleton)
import Control.Monad
import Control.Monad.State (get, put, execStateT, State, StateT)
import Control.Lens

import Chimera.Load
import Chimera.STG.Types
import Chimera.STG.World
import Chimera.STG.Util

shots :: [Bullet] -> Danmaku ()
shots = singleton . Shots

getPlayer :: Danmaku Player
getPlayer = singleton $ GetPlayer

get' :: Danmaku Enemy
get' = singleton $ Get

put' :: Enemy -> Danmaku ()
put' = singleton . Put

data Motion = Straight | Affine Vec | Curve Vec

motion :: Motion -> StateT Enemy Danmaku ()
motion (Straight) = do
  c <- use counter
  when (c == 1) $ spXY .= V2 0 1.5
  when (c == 120) $ spXY .= 0
  when (c == 220) $ spXY .= V2 0 (-1.5)
  when (c > 400) $ state .= Dead
motion (Affine v) = do
  c <- use counter
  when (c == 1) $ spXY .= V2 0 1.5
  when (c == 120) $ spXY .= 0
  when (c == 220) $ spXY .= v
  when (c > 400) $ state .= Dead
motion (Curve acc) = do
  c <- use counter
  when (c == 1) $ spXY .= V2 0 3
  spXY %= (+ acc)
  when (c > 400) $ state .= Dead

barrage :: Kind -> Danmaku ()
barrage (Zako n)
  | 0 <= n && n < 10 = zako n
  | 10 <= n = zako2 $ n `mod` 10

zako :: Int -> Danmaku ()
zako n = do
  res <- getResource
  e <- get'
  p <- getPlayer
  let cnt = e ^. counter
  let ang = (+) (pi/2) $ uncurry atan2 $ toPair (e^.pos - p^.pos)

  put' =<< motion (Straight) `execStateT` e
  when (cnt `mod` 50 == 0 && e ^. spXY == 0) $
    shots $ [initBullet (e^.pos) 3 ang 
      (bulletBitmap BallMedium (color n) (snd $ res ^. bulletImg))]
  
  where
    color :: Int -> BColor
    color 0 = Blue
    color 1 = Red
  
zako2 :: Int -> Danmaku ()
zako2 n = do
  res <- getResource
  e <- get'
  p <- getPlayer
  let cnt = e ^. counter
  let ang = (+) (pi/2) $ uncurry atan2 $ toPair (e^.pos - p^.pos)

  put' =<< motion (Curve (acc n)) `execStateT` e
  when (cnt `mod` 50 == 0) $
    shots $ [initBullet (e^.pos) 3 ang 
      (bulletBitmap Needle Purple (snd $ res ^. bulletImg))]
  
  where
    acc :: Int -> Vec
    acc 0 = V2 (-0.05) 0.005
    acc 1 = V2 0.05 0.005
  
{-

barrage :: BarrangeIndex -> Pattern
barrage BPlayer = Pattern normalBullet undefined undefined
barrage bindex@BDebug = Pattern normalBullet (normalEnemy danmaku) danmaku
  where
    danmaku :: Enemy -> Player -> [Bullet]
    danmaku e p = do
      let cnt = e ^. counter
      let posE = e ^. pos
      let ang = (fromIntegral $ cnt) / 10
      if_ (cnt `mod` 1 == 0) $ for [1] $ \i -> do
        initBullet posE 0.15 ang BallTiny Green bindex

barrage bindex@(BZako 0) = Pattern normalBullet (normalEnemy danmaku) danmaku
  where
    danmaku :: Enemy -> Player -> [Bullet]
    danmaku e p = do
      let cnt = e ^. counter
      let posE = e ^. pos; posP = p ^. pos
      let ang = (fromIntegral $ cnt) / 10
      let pAngle = atan2 ((posE - posP) ^. Game._y) (-(posE - posP) ^. Game._x)
      if_ (cnt `mod` 5 == 0) $ concat $ for [1] $ \i -> do
        let bullet t color = initBullet posE 3 (t + sin ang / 2) BallMedium color bindex
        [bullet pAngle Red, bullet (pAngle + pi/2.5) Green, bullet (pAngle - pi/2.5) Blue]

barrage bindex@(BZako 1) = Pattern normalBullet (normalEnemy danmaku) danmaku
  where
    danmaku :: Enemy -> Player -> [Bullet]
    danmaku e p = do
      let cnt = e ^. counter
      let posE = e ^. pos
      let offset = (fromIntegral cnt) / 10
      if_ (cnt `mod` 15 == 0) $ for [1..10] $ \i -> do
        let ang = i*2*pi/10
        initBullet
          (posE + fromPolar (150, offset + ang)) (-0.7) ang
          Oval (toEnum $ cnt `mod` 8) bindex
barrage bindex@(BZako 2) = Pattern normalBullet (normalEnemy danmaku) danmaku
  where
    danmaku :: Enemy -> Player -> [Bullet]
    danmaku e p = do
      let cnt = e ^. counter
      let posP = p ^. pos; posE = e ^. pos
      let pAngle = atan2 ((posE - posP) ^. Game._y) (-(posE - posP) ^. Game._x)
      if_ (cnt `mod` 50 == 0) $ for [1] $ \i -> do
        initBullet posE 2 pAngle BallLarge Blue bindex
barrage bindex@(BZako 3) = Pattern normalBullet (normalEnemy danmaku) danmaku
  where
    danmaku :: Enemy -> Player -> [Bullet]
    danmaku e p = do
      let cnt = e ^. counter
      let posE = e ^. pos
      let ang = (fromIntegral $ cnt) / 10
      if_ (cnt `mod` 3 == 0) $ for [1] $ \i -> do
        initBullet posE 3 ang Needle Green bindex

barrage bindex@(BZako 4) = Pattern normalBullet (normalEnemy danmaku) danmaku
  where
    danmaku :: Enemy -> Player -> [Bullet]
    danmaku e p = do
      let cnt = e ^. counter
      let posE = e ^. pos
      let time = 15; outerN = 10; innerN = 20; strain = 18/10;
      let theta = (fromIntegral cnt)*pi/(fromIntegral $ time*outerN)
      if_ (cnt `mod` time == 0 && 0 < cnt && cnt < time*outerN) $ concat $
        for [1..innerN] $ \i -> do
          let phi = i*strain*pi/innerN - pi/2*(1+strain)
          let bullet k color = initBullet (posE + fromPolar (150, k * theta - pi/2) + fromPolar (15, phi)) (1.5) phi BallSmall color bindex
          [bullet 1 Yellow, bullet (-1) Cyan]

barrage bindex@(BZako 5) = Pattern normalBullet (normalEnemy danmaku) danmaku
  where
    danmaku :: Enemy -> Player -> [Bullet]
    danmaku e p = do
      let cnt = e ^. counter
      let posE = e ^. pos; posP = p ^. pos
      let ang = (fromIntegral $ cnt) / 10
      let pAngle = atan2 ((posE - posP) ^. Game._y) (-(posE - posP) ^. Game._x)
      if_ (cnt `mod` 5 == 0) $ concat $ for [1] $ \i -> do
        let bullet t color = initBullet posE 3 (t + sin ang / 2) BallMedium color bindex
        [bullet pAngle Red, bullet (pAngle + pi/2.5) Green, bullet (pAngle - pi/2.5) Blue]

barrage bindex@(BBoss 0) = Pattern bullet (normalEnemy danmaku) danmaku
  where
    bullet :: State Bullet ()
    bullet = do
      cnt <- use counter
      when (30 < cnt && cnt < 120) $ do
        p <- use param
        angle %= (+ (fromP p) * pi/400)
        speed %= (subtract (3.0/100))
      normalBullet
      where
        fromP :: Int -> Double'
        fromP 0 = -1
        fromP 1 = 1
    danmaku :: Enemy -> Player -> [Bullet]
    danmaku e p = do
      let cnt = e ^. counter
      let posE = e ^. pos
      let innerN = 50
      if_ (cnt `mod` 90 == 0) $ concat $
        [[initBullet' posE
          3.5 (i*2*pi/innerN) Oval Purple bindex 0,
          initBullet' posE
          3.5 (-i*2*pi/innerN) Oval Purple bindex 1] | i <- [1..innerN]]

barrage bindex@(BBoss 1) = Pattern bullet (normalEnemy danmaku) danmaku
  where
    bullet :: State Bullet ()
    bullet = do
      cnt <- use counter
      when (30 < cnt && cnt < 120) $ do
        p <- use param
        angle %= (+ (fromP p) * pi/400)
        speed %= (subtract (3.0/100))
      normalBullet
      where
        fromP :: Int -> Double'
        fromP 0 = -1
        fromP 1 = 1
    danmaku :: Enemy -> Player -> [Bullet]
    danmaku e p = do
      let cnt = e ^. counter
      let posE = e ^. pos
      let innerN = 50
      if_ (cnt `mod` 90 == 0) $ concat $
        [[initBullet' posE
          3.5 (i*2*pi/innerN) Oval Purple bindex 0,
          initBullet' posE
          3.5 (-i*2*pi/innerN) Oval Purple bindex 1] | i <- [1..innerN]]

barrage bindex@(BBoss 2) = Pattern normalBullet (normalEnemy danmaku) danmaku
  where
    danmaku :: Enemy -> Player -> [Bullet]
    danmaku e p = do
      let cnt = e ^. counter
      let posE = e ^. pos
      let innerN = 16
      let theta = (fromIntegral cnt)*pi/2
      if_ (cnt `mod` 15 == 0) $
        [initBullet posE 2.0 
        (theta*2*pi/fromIntegral innerN + fromIntegral i*2*pi/fromIntegral innerN)
        Oval (toEnum $ i `mod` 8) bindex | i <- [1..innerN]]
barrange _ = undefined

-}
