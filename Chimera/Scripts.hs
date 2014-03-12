{-# LANGUAGE GADTs, TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE TypeOperators, DataKinds, Rank2Types #-}
module Chimera.Scripts (
  Controller(..), Stage, runStage, isShooting
  , appearAt, keeper
  , getPlayer, shots, effs, globalEffs, wait
  , initEnemy
  , MotionCommon(..)
  , motionCommon
  , zakoCommon
  , debug
  , effEnemyDead, effPlayerDead, effEnemyStart, effEnemyAttack
  , chaosBomb
  , talk, say', character, say, delCharacter
  , stageTest
  , setName
  , module M
  ) where

import FreeGame
import Control.Lens
import Control.Monad.State.Strict
import Control.Monad.Reader (ask, runReader, Reader)
import Data.Monoid ((<>))
import Data.Default (def)
import qualified Data.Vector as V
import qualified Data.Sequence as S
import qualified Data.IntMap as IM
import Data.Functor.Product

import Chimera.Core.World
import Chimera.Layers as M

data Controller = Wait Int | Stop | Go | Speak Expr | Talk deriving (Eq, Show)
data MotionCommon = Straight | Affine Vec2 | Curve Vec2 | Stay

type Stage = ReifiedLookAt Controller Field

runStage :: Controller -> Stage () -> State Field (Controller, Stage ())
runStage ctrl stg = do
  ctrl <- runReader (runController ctrl) `fmap` use id
  case isStageRunning ctrl of
    True -> do
      (k, f') <- tickStage `fmap` use id
      id .= f'
      return k
    False -> return (ctrl, stg)
  
  where
    run p q m = runLookAt' p q m `runState` Pair (return ()) (return ())
    
    tickStage field = let (stg', Pair f g) = run ctrl field stg in
      ((f `execState` ctrl, stg'), g `execState` field)

runController :: Controller -> Reader Field Controller
runController (Wait n) = case n == 0 of
  True -> return Go
  False -> return $ Wait (n-1)
runController Stop = do
  es <- (S.length . (^.enemy)) `fmap` ask
  case es == 0 of
    True -> return Go
    False -> return Stop
runController Go = return Go
runController u = return u

isShooting :: Controller -> Bool
isShooting (Speak _) = False
isShooting Talk = False
isShooting _ = True

isStageRunning :: Controller -> Bool
isStageRunning Stop = False
isStageRunning (Wait _) = False
isStageRunning _ = True

-- APIs for Stage Monad
appearEnemy :: Enemy -> Stage ()
appearEnemy e = hook $ Right $ enemy %= (S.|> e)

wait :: Int -> Stage ()
wait n = do
  hook $ Left $ id .= Wait n
  yield

stop :: Stage ()
stop = do
  hook $ Left $ id .= Stop
  yield

appearAt :: Int -> Enemy -> Stage ()
appearAt n e = wait n >> appearEnemy e

keeper :: Enemy -> Stage ()
keeper e = appearEnemy e >> stop

addEffect :: Effect -> Stage Int
addEffect e = do
  m <- (^.effects) `fmap` env
  let (n,m') = insertIM' e m
  hook $ Right $ effects .= m'
  return n

speak :: Expr -> Stage ()
speak expr = do
  hook $ Left $ id .= Speak expr
  yield

talk :: Stage () -> Stage ()
talk m = do
  startTalk
  yield
  m
  endTalk
  yield
  hook $ Right $ effects .= IM.empty
  
  where
    startTalk = hook $ Left $ id .= Talk
    endTalk = hook $ Left $ id .= Go

-- APIs for Danmaku Monad
getPlayer :: Danmaku c Player
getPlayer = (^.player) `fmap` env

shots :: [Bullet] -> Danmaku c ()
shots bs = hook $ Right $ bullets ><= S.fromList bs

effs :: [Effect] -> Danmaku EnemyObject ()
effs es = forM_ es $ \e -> do  
  m <- (^.effects) `fmap` env
  let (n, m') = insertIM' e m
  hook $ Left $ effectIndexes %= (S.|> n)
  hook $ Right $ effects .= m'

globalEffs :: [Effect] -> Danmaku c ()
globalEffs es = hook $ Right $ 
  forM_ es $ \e -> effects %= insertIM e

initEnemy :: Vec2 -> Int -> Enemy
initEnemy p h =
  pos .~ p $
  hp .~ h $
  def

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
  e <- self
  hook $ Left mot
  p <- getPlayer
  let ang = (+) (pi/2) $ uncurry atan2 $ toPair (e^.pos - p^.pos)

  when ((e^.counter) `mod` time == 0 && e^.stateChara == Attack) $
    shots $ return $
      makeBullet $
      pos .~ (e^.pos) $ 
      speed .~ 2 $
      angle .~ ang $
      kind .~ bk $
      bcolor .~ c $
      def

debug :: Danmaku EnemyObject ()
debug = do
  setName "デバッグ用弾幕"
  e <- self
  hook $ Left $ motionCommon 100 Stay  
  let cnt = e ^. counter
  let n = 20

  when (cnt `mod` 4 == 0 && e ^. spXY == 0) $
    shots $ flip map [1..n] $ \i ->
      makeBullet $
      pos .~ (e^.pos) $ 
      speed .~ 0.5 $
      angle .~ i*2*pi/n + (fromIntegral cnt)/100 $
      kind .~ BallTiny $
      bcolor .~ Red $
      def

moveSmooth :: (Autonomic c (State a) a, HasObject a, HasObject c) => 
              Vec2 -> Int -> c -> c
moveSmooth v time a = a & runAuto %~ (>> go) where
  go :: (HasObject c) => State c ()
  go = do
    let ang = pi / fromIntegral time
    c' <- use counter
    let c = c' - (a^.counter)
    when (0 <= c && c <= time) $ do
      let t = ang * (fromIntegral $ c)
      pos += ((ang * 0.5 * sin t) *^ v)

effColored :: (Float -> Color) -> State EffectObject () -> Int -> Effect -> Effect
effColored f g time e = e & runAuto %~ (>> go) where
  go :: State EffectObject ()
  go = do
    c1 <- use counter
    let c = c1 - (e^.counter)
    when (0 <= c && c <= time) $ do
      let x = fromIntegral c/fromIntegral time
      img .= (color (f x) . (e^.img))
    when (c == time) $ g

effFadeIn :: Int -> Effect -> Effect
effFadeIn n e = let y x = sin $ x*(pi/2) in
  effColored (Color 1 1 1 . y) (img .= (e^.img)) n e

effFadeOut :: Int -> Effect -> Effect
effFadeOut n e = let y x = cos $ (x*pi/2) in
  effColored (Color 1 1 1 . y) (img .= (color (Color 1 1 1 0) . (e^.img))) n e

effCommonAnimated :: Int -> Resource -> Vec2 -> Effect
effCommonAnimated k res p = def & pos .~ p & zIndex .~ OnObject & runAuto .~ run where
  run :: State EffectObject ()
  run = do
    f <- get
    let i = (f^.counter) `div` (f^.slowRate)
    img .= \r -> bitmap $ (r^.effectImg) V.! k V.! i
    counter %= (+1)
    when (i == V.length ((res^.effectImg) V.! k)) $ stateEffect .= Inactive

effEnemyDead :: Resource -> Vec2 -> Effect
effEnemyDead = effCommonAnimated 0

effPlayerDead :: Resource -> Vec2 -> Effect
effPlayerDead res = go . effCommonAnimated 1 res where
  go :: Effect -> Effect
  go e = e & size .~ V2 0.8 0.8 & slowRate .~ 5 & runAuto %~ (>> size *= 1.01)

effEnemyStart :: Resource -> Vec2 -> Effect
effEnemyStart res = go . effCommonAnimated 2 res where 
  go :: Effect -> Effect
  go e = e & size .~ V2 0.8 0.8 & slowRate .~ 6 & runAuto %~ (>> size *= 1.01)
    
effEnemyAttack :: Int -> Resource -> Vec2 -> Effect
effEnemyAttack i _ p =
  pos .~ p $
  img .~ (\r -> bitmap $ (r^.effectImg) V.! 3 V.! i) $
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
        
    anglePlus :: Int -> Double
    anglePlus 0 = 1/300
    anglePlus 1 = -2/300
    anglePlus 2 = 3/300
    anglePlus _ = error "otherwise case in anglePlus"

chaosBomb :: Resource -> Vec2 -> Bullet
chaosBomb res p =
  pos .~ p $
  kind .~ BallFrame $
  bcolor .~ Magenta $
  size .~ V2 100 0 $
  stateBullet .~ PlayerB $
  runAuto .~ run $
  def
  
  where
    bomb :: (HasObject c) => c -> Bullet -> Bullet
    bomb e b = case b^.stateBullet == EnemyB && 
                    (e^.size^._x)^(2 :: Int) > (quadrance $ b^.pos - e^.pos) of 
      True -> chaosBomb res (b^.pos) & size .~ V2 ((e^.size^._x) / 1.4) 0
      False -> b
    
    run :: Danmaku BulletObject ()
    run = do
      e <- self
      when (e^.counter == 0) $ globalEffs $ [eff e]
      when (e^.counter == 5) $ hook $ Right $ bullets %= fmap (bomb e)
      
      hook $ Left $ do
        counter %= (+1)
        c <- use counter
        when (c == 10) $ stateBullet .= Outside

    eff :: BulletObject -> Effect
    eff b = go $ zIndex .~ Background $ effCommonAnimated 4 res (b^.pos) where
      go :: Effect -> Effect
      go e = let ratio = (b^.size^._x) / 120 in
        e & size .~ V2 ratio ratio & slowRate .~ 3

say' :: Expr -> Stage ()
say' m = speak $ m <> clickend

character :: Int -> Vec2 -> Stage Int
character n p = addEffect $ effFadeIn 30 $ eff where
  eff :: Effect
  eff = def 
        & pos .~ p
        & img .~ (\res -> bitmap $ (res^.portraits) V.! n)
        & zIndex .~ Foreground
        & runAuto .~ (counter %= (+1))

delCharacter :: Int -> Stage ()
delCharacter c = hook $ Right $ effects %= IM.adjust (effFadeOut 30) c

say :: Int -> Expr -> Stage ()
say c m = do
  hook $ Right $ effects %= IM.adjust (moveSmooth (V2 (-80) 0) 50) c
  speak $ m <> clickend
  hook $ Right $ effects %= IM.adjust (moveSmooth (V2 80 0) 50) c
  
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

setName :: String -> Danmaku c ()
setName s = hook $ Right $ danmakuTitle .= s
