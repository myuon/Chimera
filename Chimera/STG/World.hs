{-# LANGUAGE TemplateHaskell, GADTs, RankNTypes, FlexibleContexts, TypeSynonymInstances, FlexibleInstances #-}
module Chimera.STG.World (
  Danmaku
  , Bullet, Enemy, Field(..), StateField(..)
  , player, enemy, bullets, effects, stateField
  , resource, counterF, isDebug
  , liftLocal, liftGlobal, readGlobal
  , runDanmaku
  , module Chimera.STG.Types
  ) where

import Graphics.UI.FreeGame
import Control.Lens
import Control.Monad.State.Strict (State)
import qualified Data.Sequence as S
import Data.Default

import Chimera.STG.Types
import Chimera.STG.Util

type Danmaku c = Runner c (Field, S.Seq (State Field ()))

type Bullet = Autonomie (Danmaku BulletObject) BulletObject
type Enemy = Autonomie (Danmaku EnemyObject) EnemyObject

instance HasObject Bullet where object = auto . object
instance HasBulletObject Bullet where bulletObject = auto

instance HasObject Enemy where object = auto . object
instance HasChara Enemy where chara = auto . chara
instance HasEnemyObject Enemy where enemyObject = auto

data StateField = Shooting | Talking deriving (Eq, Show)

data Field = Field {
  _player :: Player,
  _enemy :: S.Seq Enemy,
  _bullets :: S.Seq Bullet,
  _effects :: S.Seq Effect,
  
  _resource :: Resource,
  _counterF :: Int,
  _isDebug :: Bool,
  _stateField :: StateField
  }

makeLenses ''Field

instance Default Field where
  def = Field {
    _player = def,
    _enemy = S.empty,
    _bullets = S.empty,
    _effects = S.empty,
    
    _resource = undefined,
    _counterF = 0,
    _isDebug = False,
    _stateField = Shooting
    }

runDanmaku :: Danmaku c () -> State (LookAt c (Field, S.Seq (State Field ()))) ()
runDanmaku = runPattern

instance HasGetResource (Danmaku c) where
  getResource = (^.resource) `fmap` (fst `fmap` getGlobal)

liftGlobal :: State Field () -> Danmaku c ()
liftGlobal u = do
  f <- getGlobal
  putGlobal (fst f, snd f S.|> u)
  
readGlobal :: Danmaku c Field
readGlobal = fst `fmap` getGlobal

liftLocal :: State c a -> Danmaku c a
liftLocal = liftState getLocal putLocal
