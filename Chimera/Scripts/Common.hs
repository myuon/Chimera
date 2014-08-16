{-# LANGUAGE FlexibleContexts #-}
module Chimera.Scripts.Common where

import FreeGame
import Control.Lens
import Control.Monad.State.Strict
import Data.Default (def)
import qualified Data.Vector as V
import qualified Data.IntMap as IM
import Data.Reflection (Given, given)

import Chimera.Engine.Core
import Chimera.Engine.Scripts

data MotionCommon = Straight | Affine Vec2 | Curve Vec2 | Stay

enemyEffect :: (HasPiece c, HasChara c) => Effect -> Danmaku c ()
enemyEffect e = do
  n <- addEffect e
  hook $ Left $ effectIndexes %= (n:)

effFadeIn :: Int -> Effect -> Effect
effFadeIn n e = let y x = sin $ x*(pi/2) in
  effColored (Color 1 1 1 . y) (drawing .= (e^.drawing)) n e

effFadeOut :: Int -> Effect -> Effect
effFadeOut n e = let y x = cos $ (x*pi/2) in
  effColored (Color 1 1 1 . y) (drawing .= (color (Color 1 1 1 0) $ e^.drawing)) n e
  & runAuto %~ (>> go)
  where
    go = do
      c <- (^.counter) `fmap` self
      hook $ Left $ when (n == c) $ statePiece .= Dead

effEnemyStart :: (Given Resource) => Vec2 -> Effect
effEnemyStart = go . effCommonAnimated 2 where 
  go e = e & size .~ V2 0.8 0.8 & slowRate .~ 6 & runAuto %~ (>> (hook $ Left $ size *= 1.01))

effEnemyAttack :: (Given Resource) => Int -> Vec2 -> Effect
effEnemyAttack i p = def & pos .~ p & scaleRate .~ 0 & runAuto .~ run
  where
    resource = given :: Resource

    run :: Danmaku EffectPiece ()
    run = hook $ Left $ do
      use counter >>= \c -> when (c <= 50) $ scaleRate += 1/50
      ang += anglePlus i
      use ang >>= \a -> drawing .= do
        rotateR a $ bitmap $ (resource^.effectImg) V.! 3 V.! i
    
    anglePlus :: Int -> Double
    anglePlus 0 = 1/300
    anglePlus 1 = -2/300
    anglePlus 2 = 3/300
    anglePlus _ = error "otherwise case in anglePlus"

effPlayerBack :: (Given Resource) => Effect
effPlayerBack = def & runAuto .~ do
  p <- (^.player) `fmap` env
  c <- (^.counter) `fmap` self
  let n = p^.bombCount
  hook $ Left $ do
    let r = 65
    pos .= (p^.pos)
    scaleRate .= 0.6
    drawing .= do
      color white $ thickness 1.0 $ circleOutline r
      forM_ [1..n] $ \i ->
        translate (V2 r 0 `rotate2` (2*pi*fromIntegral i/fromIntegral n + fromIntegral c*5*pi/360))
          $ color white $ (makeBullet BallMedium Yellow def :: Bullet)^.drawing

character :: (Given Resource) => Int -> Vec2 -> Stage Int
character n p = do
  k <- lift $ addEffect $ effFadeIn 30 $ eff
  lift $ hook $ Right $ sceneEffects %= (k:)
  return k
  where
    resource = given :: Resource

    eff = def 
          & pos .~ p
          & drawing .~ (draw $ bitmap $ (resource^.portraits) V.! n)
          & zIndex .~ Foreground

delCharacter :: Int -> Stage ()
delCharacter c = lift $ hook $ Right $ effects %= IM.adjust (effFadeOut 30) c

motionCommon :: (HasPiece c, HasObject c) => Int -> MotionCommon -> State c ()
motionCommon time (Straight) = do
  c <- use counter
  when (c == 0) $ spXY .= V2 0 1.5
  when (c == 120) $ do
    spXY .= 0
    statePiece .= Attack
  when (c == time + 120) $ do
    spXY .= V2 0 (-1.5)
    statePiece .= Alive
  when (c > time + 300) $ statePiece .= Dead
motionCommon time (Affine v) = do
  c <- use counter
  when (c == 0) $ spXY .= V2 0 1.5
  when (c == 120) $ do
    spXY .= 0
    statePiece .= Attack
  when (c == time + 120) $ do
    spXY .= v
    statePiece .= Alive
  when (c > time + 300) $ statePiece .= Dead
motionCommon _ (Curve acc) = do
  c <- use counter
  when (c == 0) $ spXY .= V2 0 3
  when (c == 20) $ statePiece .= Attack
  when (c > 300) $ statePiece .= Dead
  spXY %= (+ acc)
motionCommon _ (Stay) = do
  c <- use counter
  when (c == 0) $ spXY .= V2 0 1.5
  when (c == 120) $ do
    spXY .= 0
    statePiece .= Attack

initEnemy :: (Given Resource) => Vec2 -> Int -> Enemy
initEnemy p h = def
  & pos .~ p & hp .~ h & size .~ V2 10 10 & ang .~ -pi/2
  & statePiece .~ Standby & drawing .~ (bitmap $ (resource^.charaImg) V.! 1)
  where
    resource = given :: Resource

zakoCommon :: (Given Resource, HasChara c, HasPiece c, HasObject c) =>
              Int -> State c () -> Int -> BKind -> BColor -> Danmaku c ()
zakoCommon _ mot time bk c = do
  e <- self
  hook $ Left mot
  ang' <- anglePlayer

  when ((e^.counter) `mod` time == 0 && e^.statePiece == Attack) $
    shots $ return $ makeBullet bk c def
      & pos .~ (e^.pos) & speed .~ 2 & ang .~ ang'

debug :: (Given Resource) => Danmaku Chara ()
debug = do
  setName "デバッグ用弾幕"
  e <- self
  hook $ Left $ motionCommon 100 Stay  
  let cnt = e ^. counter
  let n = 20

  when (cnt `mod` 4 == 0 && e ^. spXY == 0) $
    shots $ flip map [1..n] $ \i ->
      makeBullet BallTiny Red def
        & pos .~ (e^.pos) & speed .~ 0.5
        & ang .~ i*2*pi/n + (fromIntegral cnt)/100

chaosBomb :: (Given Resource) => Vec2 -> Bullet
chaosBomb p = makeBullet BallFrame Magenta def
  & pos .~ p & size .~ 100 & group .~ None & runAuto .~ run
  
  where
    bomb :: (Given Resource, HasPiece c, HasObject c) => c -> Bullet -> Bullet
    bomb e b = case b^.group == GEnemy && 
                    (e^.size^._x)^^(2 :: Int) > (quadrance $ b^.pos - e^.pos) of 
      True -> chaosBomb (b^.pos) & size .~ (e^.size) / 1.4
      False -> b
    
    run :: (Given Resource, HasPiece c, HasObject c) => Danmaku c ()
    run = do
      e <- self
      when (e^.counter == 1) $ effs $ [eff e]
      when (e^.counter == 5) $ hook $ Right $ bullets %= fmap (bomb e)
      
      hook $ Left $ do
        c <- use counter
        when (c == 10) $ statePiece .= Dead

    eff :: (Given Resource, HasPiece c, HasObject c) => c -> Effect
    eff b = effCommonAnimated 4 (b^.pos)
      & scaleRate .~ (b^.size^._x / 120) & zIndex .~ Background

silentBomb :: (Given Resource, HasChara c, HasObject c) => State c [Bullet]
silentBomb = use pos >>= return . return . chaosBomb

fourDiamond :: (Given Resource, HasChara c, HasObject c) => State c [Bullet]
fourDiamond = use pos >>= \p -> return $
  [def' & pos .~ p + V2 5 0,
   def' & pos .~ p + V2 15 0,
   def' & pos .~ p - V2 5 0,
   def' & pos .~ p - V2 15 0]
  where
    def' = makeBullet Diamond Red def
      & speed .~ 15 & ang .~ pi/2 & group .~ GPlayer

stageTest :: (Given Resource) => Stage ()
stageTest = do
  let e r = keeper $ initEnemy (V2 320 (-40)) 10 & runAuto .~ r
  e $ zakoCommon 0 (motionCommon 100 Stay) 50 BallLarge Red
  e $ zakoCommon 0 (motionCommon 100 Stay) 50 BallMedium Red
  e $ zakoCommon 0 (motionCommon 100 Stay) 50 BallSmall Red
  e $ zakoCommon 0 (motionCommon 100 Stay) 50 Oval Red
  e $ zakoCommon 0 (motionCommon 100 Stay) 50 Diamond Red
  e $ zakoCommon 0 (motionCommon 100 Stay) 50 BallFrame Red
  e $ zakoCommon 0 (motionCommon 100 Stay) 50 Needle Red
  e $ zakoCommon 0 (motionCommon 100 Stay) 50 BallTiny Red

anglePlayer :: (HasObject c) => Danmaku c Double
anglePlayer = do
  e <- self
  p <- getPlayer
  return $ (pi/2 +) $ (\(V2 x y) -> atan2 x y) $ (e^.pos - p^.pos)
