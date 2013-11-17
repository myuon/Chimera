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

motion :: Int -> Motion -> StateT Enemy Danmaku ()
motion stay (Straight) = do
  c <- use counter
  when (c == 0) $ spXY .= V2 0 1.5
  when (c == 120) $ spXY .= 0
  when (c == stay + 120) $ spXY .= V2 0 (-1.5)
  when (c > stay + 300) $ state .= Dead
motion stay (Affine v) = do
  c <- use counter
  when (c == 0) $ spXY .= V2 0 1.5
  when (c == 120) $ spXY .= 0
  when (c == stay + 120) $ spXY .= v
  when (c > stay + 300) $ state .= Dead
motion _ (Curve acc) = do
  c <- use counter
  when (c == 0) $ spXY .= V2 0 3
  spXY %= (+ acc)
  when (c > 300) $ state .= Dead

barrage :: Kind -> Danmaku ()
barrage (Zako n)
  | 60 <= n = zako6 $ n `mod` 10
  | 50 <= n = zako5 $ n `mod` 10
  | 40 <= n = zako4 $ n `mod` 10
  | 30 <= n = zako3 $ n `mod` 10
  | 20 <= n = zako2 $ n `mod` 10
  | 10 <= n = zako  $ n `mod` 10
  | otherwise = return ()
barrage (Boss n) = boss n
barrage (Debug) = debug

debug :: Danmaku ()
debug = do
  res <- getResource
  e <- get'
  p <- getPlayer
  let cnt = e ^. counter
  let n = 16

  when (cnt `mod` 5 == 0) $
    shots $ (flip map) [1..n] $ \i -> initBullet (e^.pos) 0.5 (i*2*pi/n + (fromIntegral cnt)/100) 
      (bulletBitmap BallTiny Red (snd $ res ^. bulletImg))

zako :: Int -> Danmaku ()
zako n = do
  put' =<< (\e -> motion 100 (Straight) `execStateT` e) =<< get'
  res <- getResource
  e <- get'
  p <- getPlayer
  let cnt = e ^. counter
  let ang = (+) (pi/2) $ uncurry atan2 $ toPair (e^.pos - p^.pos)

  when (cnt `mod` 50 == 0 && e ^. spXY == 0) $
    shots $ [initBullet (e^.pos) 3 ang 
      (bulletBitmap BallMedium (color n) (snd $ res ^. bulletImg))]
  
  where
    color :: Int -> BColor
    color 0 = Blue
    color 1 = Red
  
zako2 :: Int -> Danmaku ()
zako2 n = do
  put' =<< (\e -> motion 100 (Curve (acc n)) `execStateT` e) =<< get'
  res <- getResource
  e <- get'
  p <- getPlayer
  let cnt = e ^. counter
  let ang = (+) (pi/2) $ uncurry atan2 $ toPair (e^.pos - p^.pos)

  when (cnt `mod` 50 == 0) $
    shots $ [initBullet (e^.pos) 3 ang 
      (bulletBitmap Needle Purple (snd $ res ^. bulletImg))]
  
  where
    acc :: Int -> Vec
    acc 0 = V2 (-0.05) 0.005
    acc 1 = V2 0.05 0.005

zako3 :: Int -> Danmaku ()
zako3 _ = do
  put' =<< (\e -> motion 300 (Straight) `execStateT` e) =<< get'
  res <- getResource
  e <- get'
  p <- getPlayer
  let cnt = e ^. counter
  let ang = (+) (pi/2) $ uncurry atan2 $ toPair (e^.pos - p^.pos)
  let ang' = (fromIntegral cnt) / 10

  when (cnt `mod` 5 == 0 && e ^. spXY == 0) $
    shots $ [
      initBullet (e^.pos) 3 (ang + pi/2.5 + sin ang'/2) (bulletBitmap BallMedium Red   (snd $ res ^. bulletImg)),
      initBullet (e^.pos) 3 (ang          + sin ang'/2) (bulletBitmap BallMedium Green (snd $ res ^. bulletImg)),
      initBullet (e^.pos) 3 (ang - pi/2.5 + sin ang'/2) (bulletBitmap BallMedium Blue  (snd $ res ^. bulletImg))
      ]

zako4 :: Int -> Danmaku ()
zako4 _ = do
  put' =<< (\e -> motion 300 (Straight) `execStateT` e) =<< get'
  res <- getResource
  e <- get'
  p <- getPlayer
  let cnt = e ^. counter
  let offset = (fromIntegral cnt) / 10

  when (cnt `mod` 15 == 0 && e ^. spXY == 0) $
    shots $ (flip map) [1..10] $ \i ->
      initBullet (e^.pos + fromPolar (150, offset + i*2*pi/10)) (-0.7) (i*2*pi/10) (bulletBitmap Oval (toEnum $ cnt `mod` 8) (snd $ res ^. bulletImg))

zako5 :: Int -> Danmaku ()
zako5 _ = do
  put' =<< (\e -> motion 300 (Straight) `execStateT` e) =<< get'
  res <- getResource
  e <- get'
  p <- getPlayer
  let cnt = e ^. counter
  let ang' = (fromIntegral cnt) / 10

  when (cnt `mod` 3 == 0 && e ^. spXY == 0) $
    shots $ (flip map) [1] $ \i ->
      initBullet (e^.pos) 3 ang' (bulletBitmap Needle Green (snd $ res ^. bulletImg))

zako6 :: Int -> Danmaku ()
zako6 _ = do
  put' =<< (\e -> motion 150 (Straight) `execStateT` e) =<< get'
  res <- getResource
  e <- get'
  p <- getPlayer
  let cnt = e ^. counter
  let posE = e ^. pos
  let time = 15; outerN = 10; innerN = 20; strain = 18/10;
  let theta = (fromIntegral cnt)*pi/(fromIntegral $ time*outerN) + pi

  when (cnt `mod` time == 0 && e ^. spXY == 0) $
    shots $ (flip concatMap) [1..innerN] $ \i ->
      let phi = i*strain*pi/innerN - pi/2*(1+strain) in
      [initBullet (posE + fromPolar (150,  theta - pi/2) + fromPolar (15, phi)) (1.5) phi (bulletBitmap BallSmall Yellow (snd $ res ^. bulletImg)),
       initBullet (posE + fromPolar (150, -theta - pi/2) + fromPolar (15, phi)) (1.5) phi (bulletBitmap BallSmall Cyan (snd $ res ^. bulletImg))]

boss :: Int -> Danmaku ()
boss _ = undefined

{-
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
