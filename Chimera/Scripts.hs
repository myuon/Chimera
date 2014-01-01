{-# LANGUAGE TemplateHaskell, GADTs, TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE TypeOperators, DataKinds #-}
module Chimera.Scripts (
  Line(..), Stage, runStage
  , appearAt, keeper,
  liftS, getPlayer, shots, effs, globalEffs, get', put', wait
  , initEnemy
  , MotionCommon(..)
  , motionCommon
  , zakoCommon
  , debug
  , effEnemyDead, effPlayerDead, effEnemyStart, effEnemyAttack
  , chaosBomb
  , liftTalk, say', character, say
  , stageTest
  , module M
  ) where

import Graphics.UI.FreeGame
import Control.Lens
import Control.Monad.State.Strict (get, State)
import Control.Monad.Operational.Mini
import Data.Monoid ((<>))
import qualified Data.Vector as V
import qualified Data.Sequence as S

import Chimera.Core.World
import Chimera.Layers as M

data Line p where
  GetResourceLine :: Line Resource
  AppearEnemy :: Enemy -> Line ()
  LiftField :: State Field () -> Line ()
  GetField :: Line Field
  Wait :: Int -> Line ()
  Stop :: Line ()
  Talk :: Line ()
  Speak :: Expr -> Line ()
  Endtalk :: Line ()

type Stage = ReifiedProgram Line

runStage :: Stage () -> State Field (Stage ())
runStage (GetResourceLine :>>= next) = next `fmap` use resource
runStage (AppearEnemy e :>>= next) = enemy %= (S.|> e) >> return (next ())
runStage (Wait n :>>= next) = do
  case n == 0 of
    True -> return (next ())
    False -> return (Wait (n-1) :>>= next)
runStage u@(Stop :>>= next) = do
  es <- use enemy
  case S.length es == 0 of
    True -> return (next ())
    False -> return u
runStage (Talk :>>= next) = stateField .= Talking >> return (next ())
runStage (LiftField f :>>= next) = f >> return (next ())
runStage (GetField :>>= next) = next `fmap` get
runStage u = return u

instance HasGetResource Stage where getResource = singleton GetResourceLine

-- APIs for Stage Monad
wait :: Int -> Stage ()
wait = singleton . Wait
                                    
appearAt :: Int -> Enemy -> Stage ()
appearAt n e = wait n >> (singleton . AppearEnemy) e

keeper :: Enemy -> Stage ()
keeper e = (singleton . AppearEnemy) e >> (singleton Stop)

addEffect :: Effect -> Stage Int
addEffect e = do
  f <- singleton GetField
  singleton . LiftField $ effects %= (S.|> e)
  return $ S.length $ f^.effects

-- APIs for Danmaku Monad
liftS :: State c () -> Danmaku c ()
liftS = liftLocal

getPlayer :: Danmaku c Player
getPlayer = (^.player) `fmap` readGlobal

shots :: [Bullet] -> Danmaku c ()
shots bs = liftGlobal $ bullets ><= (S.fromList bs)

effs :: [Effect] -> Danmaku EnemyObject ()
effs es = liftS $ effectEnemy ><= (S.fromList es)

globalEffs :: [Effect] -> Danmaku c ()
globalEffs es = liftGlobal $ effects ><= (S.fromList es)

get' :: Danmaku c c
get' = getLocal

put' :: c -> Danmaku c ()
put' = putLocal

initEnemy :: Vec -> Int -> Enemy
initEnemy p h =
  pos .~ p $
  hp .~ h $
  def

data MotionCommon = Straight | Affine Vec | Curve Vec | Stay

motionCommon :: Int -> MotionCommon -> State EnemyObject ()
motionCommon time (Straight) = do
  c <- use counter
  when (c == 0) $ spXY .= V2 0 1.5
  when (c == 120) $ do
    spXY .= 0
    stateChara .= Attack
  when (c == time + 120) $ do
    spXY .= V2 0 (-1.5)
    stateChara .= Alive
  when (c > time + 300) $ stateChara .= Dead
motionCommon time (Affine v) = do
  c <- use counter
  when (c == 0) $ spXY .= V2 0 1.5
  when (c == 120) $ do
    spXY .= 0
    stateChara .= Attack
  when (c == time + 120) $ do
    spXY .= v
    stateChara .= Alive
  when (c > time + 300) $ stateChara .= Dead
motionCommon _ (Curve acc) = do
  c <- use counter
  when (c == 0) $ do
    spXY .= V2 0 3
    stateChara .= Attack
  when (c > 300) $ stateChara .= Dead
  spXY %= (+ acc)
motionCommon _ (Stay) = do
  c <- use counter
  when (c == 0) $ spXY .= V2 0 1.5
  when (c == 120) $ do
    spXY .= 0
    stateChara .= Attack

zakoCommon :: Int -> State EnemyObject () -> Int -> BKind -> BColor -> Danmaku EnemyObject ()
zakoCommon _ mot time bk c = do
  e <- get'
  liftS mot
  p <- getPlayer
  let ang = (+) (pi/2) $ uncurry atan2 $ toPair (e^.pos - p^.pos)

  when ((e^.counter) `mod` time == 0 && e^.stateChara == Attack) $
    shots $ return $
      makeBullet $
      pos .~ (e^.pos) $ 
      speed .~ 2 $
      angle .~ ang $
      kind .~ bk $
      color .~ c $
      def

debug :: Danmaku EnemyObject ()
debug = do
  e <- get'
  liftS $ motionCommon 100 Stay  
  let cnt = e ^. counter
  let n = 20

  when (cnt `mod` 4 == 0 && e ^. spXY == 0) $
    shots $ (flip map) [1..n] $ \i ->
      makeBullet $
      pos .~ (e^.pos) $ 
      speed .~ 0.5 $
      angle .~ i*2*pi/n + (fromIntegral cnt)/100 $
      kind .~ BallTiny $
      color .~ Red $
      def

moveSmooth :: (Autonomic c (State a) a, HasObject a, HasObject c) => 
              Vec -> Int -> c -> c
moveSmooth v time a = a & runAuto %~ (>> go) where
  go :: (HasObject c) => State c ()
  go = do
    let ang = pi / fromIntegral time
    c' <- use counter
    let c = c' - (a^.counter)
    when (0 <= c && c <= time) $ do
      let t = ang * (fromIntegral $ c)
      pos += ((ang * 0.5 * sin t) *^ v)

effColored :: (Double' -> Color) -> Int -> Effect -> Effect
effColored f time e = e & runAuto %~ (>> go) where
  go :: State EffectObject ()
  go = do
    c' <- use counter
    let c = c' - (e^.counter)
    when (0 <= c && c <= time) $ do
      let x = fromIntegral c/fromIntegral time
      img .= (colored (f x) . (e^.img))

effFadeIn :: Int -> Effect -> Effect
effFadeIn = effColored (\x -> Color 1 1 1 (sin (x*pi/2)))

effFadeOut :: Int -> Effect -> Effect
effFadeOut = effColored (\x -> Color 1 1 1 (-sin (x*pi/2)))

effCommonAnimated :: Int -> Resource -> Vec -> Effect
effCommonAnimated k res p = def & pos .~ p & zIndex .~ OnObject & runAuto .~ run where
  run :: State EffectObject ()
  run = do
    f <- get
    let i = (f^.counter) `div` (f^.slowRate)
    img .= \r -> fromBitmap $ (r^.effectImg) V.! k V.! i
    counter %= (+1)
    when (i == V.length ((res^.effectImg) V.! k)) $ stateEffect .= Inactive

effEnemyDead :: Resource -> Vec -> Effect
effEnemyDead = effCommonAnimated 0

effPlayerDead :: Resource -> Vec -> Effect
effPlayerDead res = go . effCommonAnimated 1 res where
  go :: Effect -> Effect
  go e = e & size .~ V2 0.8 0.8 & slowRate .~ 5 & runAuto %~ (>> size *= 1.01)

effEnemyStart :: Resource -> Vec -> Effect
effEnemyStart res = go . effCommonAnimated 2 res where 
  go :: Effect -> Effect
  go e = e & size .~ V2 0.8 0.8 & slowRate .~ 6 & runAuto %~ (>> size *= 1.01)
    
effEnemyAttack :: Int -> Resource -> Vec -> Effect
effEnemyAttack i _ p =
  pos .~ p $
  img .~ (\r -> fromBitmap $ (r^.effectImg) V.! 3 V.! i) $
  size .~ 0 $
  runAuto .~ run $
  def

  where
    run :: State EffectObject ()
    run = do
      f <- get
      when (f^.counter <= 50) $ size += 1/50
      angle += anglePlus i
      counter %= (+1)
        
    anglePlus :: Int -> Double'
    anglePlus 0 = 1/300
    anglePlus 1 = -2/300
    anglePlus 2 = 3/300
    anglePlus _ = undefined

chaosBomb :: Resource -> Vec -> Bullet
chaosBomb res p =
  pos .~ p $
  kind .~ BallFrame $
  color .~ Magenta $
  size .~ V2 100 0 $
  stateBullet .~ PlayerB $
  runAuto .~ run $
  def
  
  where
    bomb :: (HasObject c) => c -> Bullet -> Bullet
    bomb e b = case b^.stateBullet == EnemyB && 
                    (e^.size^._x)^2 > (quadrance $ b^.pos - e^.pos) of 
      True -> chaosBomb res (b^.pos) & size .~ V2 ((e^.size^._x) / 1.4) 0
      False -> b
    
    run :: Danmaku BulletObject ()
    run = do
      e <- get'
      when (e^.counter == 0) $ globalEffs $ [eff e]
      when (e^.counter == 5) $ liftGlobal $ bullets %= fmap (bomb e)
      
      liftS $ do
        counter %= (+1)
        c <- use counter
        when (c == 10) $ stateBullet .= Outside

    eff :: BulletObject -> Effect
    eff b = go $ zIndex .~ Background $ effCommonAnimated 4 res (b^.pos) where
      go :: Effect -> Effect
      go e = let ratio = (b^.size^._x) / 120 in
        e & size .~ V2 ratio ratio & slowRate .~ 3

liftTalk :: Stage () -> Stage ()
liftTalk m = do
  singleton $ Talk
  m
  singleton $ Endtalk
  singleton $ LiftField $ effects .= S.empty
  
say' :: Expr -> Stage ()
say' m = singleton . Speak $ m <> ClickWait :+: Empty

character :: Int -> Vec -> Stage Int
character n p = addEffect $ effFadeIn 30 $ eff
  where
    eff :: Effect
    eff = def 
          & pos .~ p
          & img .~ (\res -> fromBitmap $ (res^.portraits) V.! n)
          & zIndex .~ Foreground
          & runAuto .~ (counter %= (+1))

say :: Int -> Expr -> Stage ()
say c m = do
  singleton . LiftField $ effects %= S.adjust (moveSmooth (V2 (-80) 0) 50) c
  singleton . Speak $ m <> clickend
  singleton . LiftField $ effects %= S.adjust (moveSmooth (V2 80 0) 50) c
  
stageTest :: Stage ()
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
