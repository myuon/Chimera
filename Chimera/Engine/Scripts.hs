{-# LANGUAGE GADTs, TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE TypeOperators, DataKinds, Rank2Types #-}
module Chimera.Engine.Scripts (
  Controller(..), Stage, runStage, isShooting
  , appearAt, keeper
  , getPlayer, shots, wait, addEffect, effs
  , effColored -- , effCommonAnimated
  , talk, say', say
  , setName
  ) where

import FreeGame
import Control.Lens
import Control.Monad.State.Strict
import Control.Monad.Operational.Mini
import Control.Monad.Reader (ask, runReader, Reader)
import Data.Monoid ((<>))
import qualified Data.Sequence as S
import qualified Data.IntMap.Strict as IM
import Data.Functor.Product
import Data.Reflection (Given, given)

import Chimera.Engine.Core

data Controller = Wait Int | Stop | Go | Speak Expr | Talk deriving (Eq, Show)

type Stage = LookAt Controller Field

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
    run p q m = runLookAt p q m `runState` Pair (return ()) (return ())
    
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

getPlayer :: Danmaku c Player
getPlayer = (^.player) `fmap` env

shots :: [Bullet] -> Danmaku c ()
shots bs = hook $ Right $ bullets ><= S.fromList bs

addEffect :: (Pattern p q :! m (Pattern p q), q ~ Field, Functor (m (Pattern p q))) =>
             Effect -> m (Pattern p q) Int
addEffect e = do
  m <- (^.effects) `fmap` env
  let (n,m') = insertIM' e m
  hook $ Right $ effects .= m'
  return n

effs :: (Pattern p q :! m (Pattern p q), q ~ Field, Functor (m (Pattern p q))) =>
           [Effect] -> m (Pattern p q) ()
effs es = hook $ Right $ effects %= \s -> foldl (flip insertIM) s es

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

{-
effCommonAnimated :: (Given Resource) => Int -> Vec2 -> Effect
effCommonAnimated k p = def & pos .~ p & zIndex .~ OnObject & runAuto .~ run where
  run = do
    f <- get
    let i = (f^.counter) `div` (f^.slowRate)
    img .= \r -> bitmap $ (r^.effectImg) V.! k V.! i
    counter %= (+1)
    let resource = given :: Resource
    when (i == V.length ((resource^.effectImg) V.! k)) $ stateEffect .= Inactive
-}

say' :: Expr -> Stage ()
say' m = speak $ m <> clickend

say :: Int -> Expr -> Stage ()
say c m = do
  hook $ Right $ effects %= IM.adjust (moveSmooth (V2 (-80) 0) 50) c
  speak $ m <> clickend
  hook $ Right $ effects %= IM.adjust (moveSmooth (V2 80 0) 50) c
  
setName :: String -> Danmaku c ()
setName s = hook $ Right $ danmakuTitle .= s
