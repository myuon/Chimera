{-# LANGUAGE GADTs, TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE TypeOperators, DataKinds, Rank2Types, DeriveFunctor #-}
module Chimera.Engine.Scripts (
  Controller(..), Stage, isShooting, isStageRunning
  , runStage, runController, appearAt, keeper
  , getPlayer, shots, wait, addEffect, effs
  , effColored -- , effCommonAnimated
  , talk, say', say
  , setName
  ) where

import FreeGame
import Control.Lens
import Control.Monad.Reader
import Control.Monad.Coroutine
import Data.Monoid ((<>))
import qualified Data.IntMap.Strict as IM

import Chimera.State
import Chimera.Engine.Core

data Controller = Wait Int | Stop | Go | Speak Expr | Talk deriving (Eq, Show)

data Yield x = Yield x deriving (Functor)

yield :: (Monad m) => Coroutine Yield m ()
yield = suspend (Yield $ return ())

type Stage a = Coroutine Yield (LookAt Controller Field) a

runStage :: Controller -> Field -> Stage () -> ((Controller, Field), Stage ())
runStage c f m = let (r,s) = runState (resume m) (c,f) in
  case r of
    Left (Yield k) -> (s,k)
    _ -> (s,return ())

runController :: Controller -> Reader Field Controller
runController (Wait n) = case n == 0 of
  True -> return Go
  False -> return $ Wait (n-1)
runController Stop = do
  es <- (IM.size . (^.enemies)) `fmap` ask
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
appearEnemy e = lift $ zoom _2 $ enemies %= (insertIM e)

wait :: Int -> Stage ()
wait n = do
  lift $ zoom _1 $ id .= Wait n
  yield

stop :: Stage ()
stop = do
  lift $ zoom _1 $ id .= Stop
  yield

appearAt :: Int -> Enemy -> Stage ()
appearAt n e = wait n >> appearEnemy e

keeper :: Enemy -> Stage ()
keeper e = appearEnemy e >> stop

speak :: Expr -> Stage ()
speak expr = do
  lift $ zoom _1 $ id .= Speak expr
  yield

talk :: Stage () -> Stage ()
talk m = do
  startTalk
  yield
  m
  endTalk
  yield
  lift $ zoom _2 $ do
    ks <- use sceneEffects
    effects %= \es -> foldl (flip IM.delete) es ks
    sceneEffects .= []
  
  where
    startTalk = lift $ zoom _1 $ id .= Talk
    endTalk = lift $ zoom _1 $ id .= Go

getPlayer :: Danmaku c Player
getPlayer = use (env.player)

shots :: [Bullet] -> Danmaku c ()
shots bs = zoom _2 $ bullets %= insertsIM' bs

addEffect :: Effect -> LookAt c Field Int
addEffect e = do
  m <- use (env.effects)
  let (n,m') = insertIM' e m
  zoom _2 $ effects .= m'
  return n

effs :: [Effect] -> LookAt c Field ()
effs es = zoom _2 $ effects %= \s -> foldr insertIM s es

moveSmooth :: (HasObject a) => 
              Vec2 -> Int -> Autonomie (Danmaku a) a -> Autonomie (Danmaku a) a
moveSmooth v time a = a & runAuto %~ (>> go) where
  go = zoom _1 $ do
    let k = pi / fromIntegral time
    c' <- use counter
    let c = c' - (a^.auto^.counter)
    when (0 <= c && c <= time) $ do
      let t = k * (fromIntegral $ c)
      pos += ((k * 0.5 * sin t) *^ v)

effColored :: (Float -> Color) -> State EffectPiece () -> Int -> Effect -> Effect
effColored f g time e = e & runAuto %~ (>> go) where
  go = zoom _1 $ do
    c1 <- use counter
    let c = c1 - (e^.counter)
    when (0 <= c && c <= time) $ do
      let x = fromIntegral c/fromIntegral time
      drawing .= (color (f x) $ e^.drawing)
    when (c == time) $ g

say' :: Expr -> Stage ()
say' m = speak $ m <> clickend

say :: Int -> Expr -> Stage ()
say c m = do
  lift $ zoom _2 $ effects %= IM.adjust (moveSmooth (V2 (-80) 0) 50) c
  speak $ m <> clickend
  lift $ zoom _2 $ effects %= IM.adjust (moveSmooth (V2 80 0) 50) c
  
setName :: String -> Danmaku c ()
setName s = zoom _2 $ danmakuTitle .= s
