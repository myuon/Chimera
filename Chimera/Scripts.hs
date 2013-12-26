module Chimera.Scripts (
  appearAt, keeper,
  liftS, getPlayer, shots, effs, globalEffs, get', put'
  , initEnemy
  , MotionCommon(..)
  , motionCommon
  , zakoCommon
  , debug
  , effEnemyDead, effPlayerDead, effEnemyStart, effEnemyAttack
  , chaosBomb
  ) where

import Graphics.UI.FreeGame
import Control.Lens
import Control.Monad.State.Strict (get, State)
import qualified Data.Vector as V
import qualified Data.Sequence as S

import Chimera.STG.Util
import Chimera.STG.World

-- APIs for Stage Monad
appearAt :: Int -> Enemy -> Stage ()
appearAt n e = wait n >> appear e

keeper :: Enemy -> Stage ()
keeper e = appear e >> stop

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
      pos .~ (e^.pos) $ 
      speed .~ 3 $
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
      pos .~ (e^.pos) $ 
      speed .~ 0.5 $
      angle .~ i*2*pi/n + (fromIntegral cnt)/100 $
      kind .~ BallTiny $
      color .~ Red $
      def

effEnemyDead :: Resource -> Vec -> Effect
effEnemyDead res p =
  pos .~ p $
  size .~ V2 1 1 $
  runAuto .~ run $
  def

  where
    run :: State EffectObject ()
    run = do
      f <- get
      let i = (f^.counter) `div` (f^.slowRate)
      img .= \r -> (r^.effectImg) V.! 0 V.! i
      counter %= (+1)
      when (i == V.length ((res^.effectImg) V.! 0)) $ stateEffect .= Inactive

effPlayerDead :: Resource -> Vec -> Effect
effPlayerDead res p =
  pos .~ p $
  size .~ V2 0.8 0.8 $
  slowRate .~ 5 $
  runAuto .~ run $
  def

  where
    run :: State EffectObject ()
    run = do
      f <- get
      let i = (f^.counter) `div` (f^.slowRate)
      img .= \r -> (r^.effectImg) V.! 1 V.! i
      size *= 1.01
      counter %= (+1)
      when (i == V.length ((res^.effectImg) V.! 1)) $ stateEffect .= Inactive

effEnemyStart :: Resource -> Vec -> Effect
effEnemyStart res p =
  pos .~ p $
  size .~ V2 0.8 0.8 $
  slowRate .~ 6 $
  runAuto .~ run $
  def

  where
    run :: State EffectObject ()
    run = do
      f <- get
      let i = (f^.counter) `div` (f^.slowRate)
      img .= \r -> (r^.effectImg) V.! 2 V.! i
      size *= 1.01
      counter %= (+1)
      when (i == V.length ((res^.effectImg) V.! 2)) $ stateEffect .= Inactive
    
effEnemyAttack :: Int -> Resource -> Vec -> Effect
effEnemyAttack i _ p =
  pos .~ p $
  img .~ (\r -> (r^.effectImg) V.! 3 V.! i) $
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
    eff b = let ratio = (b^.size^._x) / 120 in
      pos .~ (b^.pos) $
      size .~ V2 ratio ratio $
      slowRate .~ 3 $
      runAuto .~ run' $
      def
      
      where
        run' :: State EffectObject ()
        run' = do
          f <- get
          let i = (f^.counter) `div` (f^.slowRate)
          img .= \r -> (r^.effectImg) V.! 4 V.! i
          counter %= (+1)
          when (i == V.length ((res^.effectImg) V.! 4)) $ stateEffect .= Inactive
