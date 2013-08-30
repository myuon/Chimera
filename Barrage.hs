{-# LANGUAGE TemplateHaskell, RankNTypes #-}
module Barrage where

import qualified Graphics.UI.FreeGame as Game

import qualified Linear.V2 as V2
import Control.Lens
import Control.Monad.State

import Object
import Global

data Pattern = Pattern {
  _bullet :: State Bullet (),
  _enemy :: Player -> State Enemy (),
  _danmaku :: Enemy -> Player -> [Bullet]
  }

makeLenses ''Pattern

if_ :: Bool -> [a] -> [a]
if_ True = id
if_ False = const []

moveMState :: MotionState -> Pos -> Pos -> Pos
moveMState Go v = (+v)
moveMState Stay v = id
moveMState Back v = (subtract v)

checkStateMotion :: Motion -> Enemy -> MotionState -> MotionState
checkStateMotion (Mono go stay) e
  | e ^. counter == go = const Stay
  | e ^. counter == go + stay = const Back
  | go + stay < e ^. counter && not (isInside (e ^. pos)) = const Dead 
  | otherwise = id
checkStateMotion (WaitMono go) e
  | e ^. counter == go = const Stay
  | otherwise = id
checkStateMotion _ _ = undefined

normalBullet :: State Bullet ()
normalBullet = do
  r <- use speed
  t <- use angle
  pos %= (+ fromPolar (r,t))
  counter %= (+1)

normalEnemy :: (Enemy -> Player -> [Bullet]) -> Player -> State Enemy ()
normalEnemy danmaku p = do
  e <- get
  pos %= moveMState (e ^. mstate) ((e ^. speed) $* fromPair (0,1))
  counter %= (+1)
  let m = checkStateMotion (e ^. motion) e $ e ^. mstate
  when (e ^. mstate == Go && m == Stay) $ do
    counter .= 0
  mstate .= m
  when (e ^. mstate == Stay) $ do
    shotQ .= danmaku e p

barrage :: BarrangeIndex -> Pattern
barrage BPlayer = Pattern normalBullet undefined undefined
barrage bindex@(BZako 1) = Pattern normalBullet (normalEnemy danmaku) danmaku
  where
    danmaku :: Enemy -> Player -> [Bullet]
    danmaku e p = do
      let cnt = e ^. counter
      let posE = e ^. pos
      let ang = (fromIntegral $ cnt) / 10
      if_ (cnt `mod` 15 == 0) $
        let ang = \i -> i*2*pi/10
            offset = (fromIntegral cnt) / 10 in
        [initBullet (posE + fromPolar (150, offset + ang i)) (-0.7) (ang i)
          Oval (toEnum $ cnt `mod` 8) bindex | i <- [1..10]]
barrage bindex@(BZako 2) = Pattern normalBullet (normalEnemy danmaku) danmaku
  where
    danmaku :: Enemy -> Player -> [Bullet]
    danmaku e p = do
      let cnt = e ^. counter
      let posP = p ^. pos; posE = e ^. pos
      let pAngle = atan2 ((posE - posP) ^. V2._y) (-(posE - posP) ^. V2._x)
      if_ (cnt `mod` 50 == 0) $
        [initBullet posE 2 pAngle BallLarge Blue bindex]
barrage bindex@(BZako 3) = Pattern normalBullet (normalEnemy danmaku) danmaku
  where
    danmaku :: Enemy -> Player -> [Bullet]
    danmaku e p = do
      let cnt = e ^. counter
      let posE = e ^. pos
      let ang = (fromIntegral $ cnt) / 10
      if_ (cnt `mod` 3 == 0) $
        [initBullet posE 3 ang Needle Green bindex]
barrage bindex@(BZako 4) = Pattern normalBullet (normalEnemy danmaku) danmaku
  where
    danmaku :: Enemy -> Player -> [Bullet]
    danmaku e p = do
      let cnt = e ^. counter - 60
      let posE = e ^. pos
      let ang = (fromIntegral $ cnt) / 10
      let time = 15; outerN = 10; innerN = 20; strain = 18/10;
          theta i = (fromIntegral cnt)*pi/(fromIntegral $ time*outerN)
          phi i = i*strain*pi/innerN - pi/2*(1+strain)
      if_ (cnt `mod` time == 0 && 0 < cnt && cnt < time*outerN) $ concat $
        [[initBullet (posE + fromPolar (150, theta i - pi/2) + fromPolar (15, phi i))
          (1.5) (phi i) BallSmall Yellow bindex,
          initBullet (posE + fromPolar (150, -theta i - pi/2) + fromPolar (15, phi i))
          (1.5) (phi i) BallSmall Cyan bindex] | i <- [1..innerN]]
barrage bindex@(BZako 5) = Pattern normalBullet (normalEnemy danmaku) danmaku
  where
    danmaku :: Enemy -> Player -> [Bullet]
    danmaku e p = do
      let cnt = e ^. counter - 60
      let posE = e ^. pos
      if_ (cnt `mod` 3 == 0) $
        let ang = (fromIntegral $ cnt) / 10 in
        [initBullet posE 3 ang Needle Green bindex]
barrage bindex@(BBoss 1) = Pattern bullet (normalEnemy danmaku) danmaku
  where
    bullet :: State Bullet ()
    bullet = do
      cnt <- use counter
      when (30 < cnt && cnt < 170) $ do
        p <- use param
        angle %= (+ (fromP p) * pi/400)
        speed %= (subtract (2.5/200))
      normalBullet
      where
        fromP :: Int -> Double
        fromP 0 = -1
        fromP 1 = 1

    danmaku :: Enemy -> Player -> [Bullet]
    danmaku e p = do
      let cnt = e ^. counter - 60
      let posE = e ^. pos
      let innerN = 60
      if_ (cnt `mod` 50 == 0) $ concat $
        [[initBullet' posE
          3.0 (i*2*pi/innerN) Oval Purple bindex 0,
          initBullet' posE
          3.0 (-i*2*pi/innerN) Oval Purple bindex 1] | i <- [1..innerN]]
barrage bindex@(BBoss 2) = Pattern normalBullet (normalEnemy danmaku) danmaku
  where
    danmaku :: Enemy -> Player -> [Bullet]
    danmaku e p = do
      let cnt = e ^. counter - 60
      let posE = e ^. pos
      let innerN = 16
      let theta = (fromIntegral cnt)*pi/2
      if_ (cnt `mod` 15 == 0) $
        [initBullet posE 2.0 
        (theta*2*pi/fromIntegral innerN + fromIntegral i*2*pi/fromIntegral innerN)
        Oval (toEnum $ i `mod` 8) bindex | i <- [1..innerN]]
barrange _ = undefined

