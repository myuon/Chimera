module Chimera.Scripts.Common where

import FreeGame
import Control.Lens
import Control.Monad.State.Strict
import Data.Default (def)
import qualified Data.Vector as V
import qualified Data.IntMap as IM
import qualified Data.Sequence as S

import Chimera.Scripts
import Chimera.Core.World

data MotionCommon = Straight | Affine Vec2 | Curve Vec2 | Stay

enemyEffect :: Effect -> Danmaku EnemyObject ()
enemyEffect e = do
  n <- addEffect e
  hook $ Left $ effectIndexes %= (S.|> n)

effFadeIn :: Int -> Effect -> Effect
effFadeIn n e = let y x = sin $ x*(pi/2) in
  effColored (Color 1 1 1 . y) (img .= (e^.img)) n e

effFadeOut :: Int -> Effect -> Effect
effFadeOut n e = let y x = cos $ (x*pi/2) in
  effColored (Color 1 1 1 . y) (img .= (color (Color 1 1 1 0) . (e^.img))) n e

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
      ang += anglePlus i
      counter %= (+1)
        
    anglePlus :: Int -> Double
    anglePlus 0 = 1/300
    anglePlus 1 = -2/300
    anglePlus 2 = 3/300
    anglePlus _ = error "otherwise case in anglePlus"

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

initEnemy :: Vec2 -> Int -> Enemy
initEnemy p h =
  pos .~ p $
  hp .~ h $
  def

zakoCommon :: Int -> State EnemyObject () -> Int -> BKind -> BColor -> Danmaku EnemyObject ()
zakoCommon _ mot time bk c = do
  e <- self
  hook $ Left mot
  ang' <- anglePlayer

  when ((e^.counter) `mod` time == 0 && e^.stateChara == Attack) $
    shots $ return $
      makeBullet $
      pos .~ (e^.pos) $ 
      speed .~ 2 $
      ang .~ ang' $
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
      ang .~ i*2*pi/n + (fromIntegral cnt)/100 $
      kind .~ BallTiny $
      bcolor .~ Red $
      def

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
      when (e^.counter == 0) $ do
        effs $ [eff e]
        return ()
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

anglePlayer :: (HasObject c) => Danmaku c Double
anglePlayer = do
  e <- self
  p <- getPlayer
  return $ (pi/2 +) $ (\(V2 x y) -> atan2 x y) $ (e^.pos - p^.pos)
