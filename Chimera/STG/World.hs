{-# LANGUAGE TemplateHaskell, GADTs, RankNTypes, FlexibleContexts, TypeSynonymInstances, FlexibleInstances #-}
module Chimera.STG.World (
  Danmaku
  , Bullet, Enemy, Stage, Field(..)
  , player, enemy, bullets, effects
  , stage, resource, counterF, isDebug
  , liftLocal, liftGlobal, readGlobal
  , runDanmaku, runStage
  , module Chimera.STG.Types
  ) where

import Graphics.UI.FreeGame
import Control.Arrow
import Control.Lens
import Control.Monad.State.Strict (get, put, execState, runState, State)
import Control.Monad.Operational.Mini
import qualified Data.Sequence as S
import qualified Data.Vector as V
import Data.Default

import Chimera.STG.Types
import Chimera.STG.Util
import qualified Chimera.STG.UI as UI

type Danmaku c = Runner c (Field, S.Seq (State Field ()))

type Bullet = Autonomie (Danmaku BulletObject) BulletObject
type Enemy = Autonomie (Danmaku EnemyObject) EnemyObject

type Stage = ReifiedProgram (Line Enemy)

instance HasObject Bullet where object = auto . object
instance HasBulletObject Bullet where bulletObject = auto

instance HasObject Enemy where object = auto . object
instance HasChara Enemy where chara = auto . chara
instance HasEnemyObject Enemy where enemyObject = auto

data Field = Field {
  _player :: Player,
  _enemy :: S.Seq Enemy,
  _bullets :: S.Seq Bullet,
  _effects :: S.Seq Effect,
  
  _stage :: Stage (),
  _resource :: Resource,
  _counterF :: Int,
  _isDebug :: Bool
  }

makeLenses ''Field

instance Default Field where
  def = Field {
    _player = def,
    _enemy = S.empty,
    _bullets = S.empty,
    _effects = S.empty,
    
    _stage = return (),
    _resource = undefined,
    _counterF = 0,
    _isDebug = False
    }

runDanmaku :: Danmaku c () -> State (LookAt c (Field, S.Seq (State Field ()))) ()
runDanmaku = runPattern

instance HasGetResource (Danmaku c) where
  getResource = (^.resource) `fmap` (fst `fmap` getGlobal)

liftGlobal :: State Field () -> Danmaku c ()
liftGlobal u = do
  f <- getGlobal
  putGlobal (fst f, u S.<| snd f)
  
readGlobal :: Danmaku c Field
readGlobal = fst `fmap` getGlobal

liftLocal :: State c a -> Danmaku c a
liftLocal = liftState getLocal putLocal

runStage :: Stage () -> State (LookAt (Maybe Enemy) Field) (Stage ())
runStage (GetResourceLine :>>= next) = next `fmap` use (global.resource)
runStage (Appear e :>>= next) = local .= (Just e) >> return (next ())
runStage line@(Wait n :>>= next) = do
  case n == 0 of
    True -> return (next ())
    False -> return (Wait (n-1) :>>= next)
runStage line@(Stop :>>= next) = do
  es <- use (global.enemy)
  case S.length es == 0 of
    True -> return (next ())
    False -> return line
runStage line@(Return _) = return line

instance HasGetResource Stage where getResource = getResourceLine

