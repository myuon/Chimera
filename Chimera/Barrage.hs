{-# LANGUAGE RankNTypes, GADTs, FlexibleContexts #-}
module Chimera.Barrage (
  barrage, runBullet
  ) where

import Graphics.UI.FreeGame
import Control.Monad.Operational.Mini (singleton)
import Control.Monad
import Control.Monad.State.Strict (get, put, execStateT, State, StateT)
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

data Motion = Straight | Affine Vec | Curve Vec | Stay

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
motion _ (Stay) = do
  c <- use counter
  when (c == 0) $ spXY .= V2 0 1.5
  when (c == 120) $ spXY .= 0

runBullet :: KindBullet -> StateT Bullet Game ()
runBullet (KindBullet 0) = do
  r <- use speed
  t <- use angle
  pos %= (+ fromPolar (r,t))
runBullet (KindBullet 1) = do
  counter %= (+1)
  cnt <- use counter
  when (30 < cnt && cnt < 120) $ do
    p <- use param
    angle %= (+ (fromP p) * pi/400)
    speed %= (subtract (3.0/100))
  runBullet (KindBullet 0)
  where
    fromP :: Int -> Double'
    fromP 0 = -1
    fromP 1 = 1
runBullet (KindBullet 2) = do
  counter %= (+1)
  cnt <- use counter
  when (30 < cnt && cnt < 120) $ do
    angle %= (+ pi/400)
    speed %= (subtract (1.0/100))
  runBullet (KindBullet 0)

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

initNormalBullet :: Vec -> Double' -> Double' -> BKind -> BColor -> Resource -> Bullet
initNormalBullet p sp ang bk bc res = initBullet' p sp ang bk bc res (KindBullet 0) 0

debug :: Danmaku ()
debug = do
  put' =<< (\e -> motion 0 Stay `execStateT` e) =<< get'
  res <- getResource
  e <- get'
  p <- getPlayer
  let cnt = e ^. counter
  let n = 20

  when (cnt `mod` 4 == 0 && e ^. spXY == 0) $
    shots $ (flip map) [1..n] $ 
      \i -> initNormalBullet (e^.pos) 0.5 (i*2*pi/n + (fromIntegral cnt)/100) BallTiny Red res

zako :: Int -> Danmaku ()
zako n = do
  put' =<< (\e -> motion 100 (Straight) `execStateT` e) =<< get'
  res <- getResource
  e <- get'
  p <- getPlayer
  let cnt = e ^. counter
  let ang = (+) (pi/2) $ uncurry atan2 $ toPair (e^.pos - p^.pos)

  when (cnt `mod` 50 == 0 && e ^. spXY == 0) $
    shots $ [initNormalBullet (e^.pos) 3 ang BallMedium (color n) res]
  
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
    shots $ [initNormalBullet (e^.pos) 3 ang Needle Purple res]
  
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
    shots $ [initNormalBullet (e^.pos) 3 (ang + pi/2.5 + sin ang'/2) BallMedium Red   res,
             initNormalBullet (e^.pos) 3 (ang          + sin ang'/2) BallMedium Green res,
             initNormalBullet (e^.pos) 3 (ang - pi/2.5 + sin ang'/2) BallMedium Blue  res]

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
      initNormalBullet (e^.pos + fromPolar (150, offset + i*2*pi/10)) (-0.7) (i*2*pi/10) Oval (toEnum $ cnt `mod` 8) res

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
      initNormalBullet (e^.pos) 3 ang' Needle Green res

zako6 :: Int -> Danmaku ()
zako6 _ = do
  put' =<< (\e -> motion 150 (Straight) `execStateT` e) =<< get'
  res <- getResource
  e <- get'
  p <- getPlayer
  let cnt = e ^. counter
  let posE = e ^. pos
  let time = 5; outerN = 20; innerN = 15; strain = 1.6;
  let theta = (fromIntegral cnt)*pi/(fromIntegral $ time*outerN) + pi

  when (cnt `mod` time == 0 && e ^. spXY == 0) $
    shots $ (flip concatMap) [1..innerN] $ \i ->
      let phi = i*strain*pi/innerN - pi/2*(1+strain) + pi in
      [initNormalBullet (posE + fromPolar (150,  theta - pi/2) + fromPolar (15, phi)) (1.5) phi BallSmall Yellow res,
       initNormalBullet (posE + fromPolar (150, -theta - pi/2) + fromPolar (15, phi)) (1.5) phi BallSmall Cyan res]

boss :: Int -> Danmaku ()
boss 0 = do
  put' =<< (\e -> motion 0 Stay `execStateT` e) =<< get'
  res <- getResource
  e <- get'
  p <- getPlayer
  let cnt = e ^. counter
  let posE = e ^. pos
  let innerN = 50
  
  when (cnt `mod` 90 == 0 && e ^. spXY == 0) $
    shots $ (flip concatMap) [1..innerN] $ \i ->
      [initBullet' (e^.pos) 3.5 (i*2*pi/innerN) Oval Purple res (KindBullet 1) 0,
       initBullet' (e^.pos) 3.5 (i*2*pi/innerN) Oval Purple res (KindBullet 1) 1]
boss 1 = do
  put' =<< (\e -> motion 0 Stay `execStateT` e) =<< get'
  res <- getResource
  e <- get'
  p <- getPlayer
  let cnt = e ^. counter
  let posE = e ^. pos
  let innerN = 16
  let theta = (fromIntegral cnt)*pi/2
  
  when (cnt `mod` 15 == 0 && e ^. spXY == 0) $
    shots $ (flip concatMap) [1..innerN] $ \i ->
      [initNormalBullet (e^.pos) 2.0 (theta*2*pi/fromIntegral innerN + fromIntegral i*2*pi/fromIntegral innerN) Oval (toEnum $ i `mod` 8) res]
boss 2 = do
  put' =<< (\e -> motion 0 Stay `execStateT` e) =<< get'
  res <- getResource
  e <- get'
  p <- getPlayer
  let cnt = e ^. counter
  let ang = fromIntegral cnt/10
  let offset = (fromIntegral cnt) / 10
  
  when (cnt `mod` 15 == 0 && e ^. spXY == 0) $
    shots $ (flip map) [1..10] $ \i ->
      initBullet' (e^.pos + fromPolar (50, ang+2*pi*i/10)) 1.9 (2*pi*i/10) Oval (toEnum $ cnt `mod` 8) res (KindBullet 2) 0

