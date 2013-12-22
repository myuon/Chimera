module Chimera.Scripts (
  appearAt, keeper,
  liftS, getPlayer, shots, effs, globalEffs, get', put'
  , initEnemy
  , MotionCommon(..)
  , motionCommon
  , zakoCommon
  , debug
  , effEnemyDead, effPlayerDead
  , chaosBomb
  ) where

import Graphics.UI.FreeGame
import Control.Lens
import Control.Monad
import Control.Monad.Operational.Mini
import Control.Monad.State.Strict (get, put, execState, State)
import qualified Data.Vector as V
import qualified Data.Sequence as S

import Chimera.STG.Util
import Chimera.STG.World
import Chimera.Load

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

initEnemy :: Vec -> Int -> Resource -> Enemy
initEnemy p h res =
  pos .~ p $
  hp .~ h $
  img .~ (snd $ res^.charaImg) $
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
zakoCommon 0 mot time bk c = do
  e <- get'
  liftS mot
  res <- getResource
  p <- getPlayer
  let ang = (+) (pi/2) $ uncurry atan2 $ toPair (e^.pos - p^.pos)

  when ((e^.counter) `mod` time == 0 && e^.stateChara == Attack) $
    shots $ return $
      pos .~ (e^.pos) $ 
      speed .~ 3 $
      angle .~ ang $
      img .~ (bulletBitmap bk c (snd $ res^.bulletImg)) $
      def

debug :: Danmaku EnemyObject ()
debug = do
  e <- get'
  liftS $ motionCommon 100 Stay  
  res <- getResource
  let cnt = e ^. counter
  let n = 20

  when (cnt `mod` 4 == 0 && e ^. spXY == 0) $
    shots $ (flip map) [1..n] $ \i ->
      pos .~ (e^.pos) $ 
      speed .~ 0.5 $
      angle .~ i*2*pi/n + (fromIntegral cnt)/100 $
      img .~ (bulletBitmap BallTiny Red (snd $ res^.bulletImg)) $
      def

effEnemyDead :: Resource -> Vec -> Effect
effEnemyDead res p =
  pos .~ p $
  ress .~ (res^.effectImg) V.! 0 $
  size .~ V2 1 1 $
  runAuto .~ run $
  (def :: Effect)

  where
    run :: State EffectObject ()
    run = do
      f <- get
      res <- use ress
      let i = (f^.counter) `div` (f^.slowRate)
      img .= res V.! i
      counter %= (+1)
      when (i == V.length res) $ stateEffect .= Inactive

effPlayerDead :: Resource -> Vec -> Effect
effPlayerDead res p =
  pos .~ p $
  ress .~ (res^.effectImg) V.! 1 $
  size .~ V2 0.8 0.8 $
  slowRate .~ 5 $
  runAuto .~ run $
  (def :: Effect)

  where
    run :: State EffectObject ()
    run = do
      f <- get
      res <- use ress
      let i = (f^.counter) `div` (f^.slowRate)
      img .= res V.! i
      size *= 1.01
      counter %= (+1)
      when (i == V.length res) $ stateEffect .= Inactive

chaosBomb :: Resource -> Vec -> Bullet
chaosBomb res p =
  pos .~ p $
  img .~ (bulletBitmap BallFrame Magenta (snd $ res^.bulletImg)) $
  runAuto .~ run $
  stateBullet .~ PlayerB $
  def
  
  where
    bomb :: (HasObject c) => c -> Bullet -> Bullet
    bomb e b = case b^.stateBullet == EnemyB && 
                    100^2 > (quadrance $ b^.pos - e^.pos) of 
      True -> chaosBomb res (b^.pos)
      False -> b
    
    run :: Danmaku BulletObject ()
    run = do
      res <- getResource
      e <- get'
      when (e^.counter == 0) $ globalEffs $ [effEnemyDead res p]
      when (e^.counter == 5) $ liftGlobal $ bullets %= fmap (bomb e)
      
      liftS $ do
        counter %= (+1)
        c <- use counter
        when (c == 10) $ stateBullet .= Outside
