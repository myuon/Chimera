{-# LANGUAGE TemplateHaskell, GADTs, TypeSynonymInstances, FlexibleInstances #-}
module Chimera.STG.Types where

import Graphics.UI.FreeGame
import Control.Lens
import Control.Monad.Operational.Mini (Program, ReifiedProgram, singleton)
import Control.Monad.State.Strict (State)
import qualified Data.Sequence as S
import qualified Data.Vector as V
import Data.Default

import Chimera.STG.Util
import Chimera.Load
import qualified Chimera.STG.UI as UI

data LookAt p q = LookAt {
  _local :: p,
  _global :: q
  }

makeLenses ''LookAt

data Autonomie m a = Autonomie {
  _auto :: a,
  _runAuto :: m ()
  }

makeLenses ''Autonomie

instance (Eq a) => Eq (Autonomie m a) where
  a == b = a^.auto == b^.auto

data Object = Object {
  _pos :: Vec,
  _spXY :: Vec,
  _speed :: Double',
  _angle :: Double',
  
  _counter :: Int,
  _img :: Bitmap,
  _size :: Vec
  } deriving (Eq, Show)

makeClassy ''Object

instance Default Object where
  def = Object {
    _pos = V2 0 0,
    _spXY = V2 0 0,
    _speed = 0,
    _angle = 0,
    _counter = 0,
    _img = undefined,
    _size = V2 0 0
    }

class HasStateInt c where stateInt :: Lens' c Int

data StateEffect = Active | Inactive deriving (Eq, Enum, Show)

data EffectObject = EffectObject {
  _objectEffect :: Object,
  _ress :: V.Vector Bitmap,
  _stateEffect :: StateEffect,
  _slowRate :: Int
  }

makeClassy ''EffectObject

instance HasObject EffectObject where object = objectEffect
instance HasStateInt EffectObject where
  stateInt = lens (fromEnum . _stateEffect) (\f a -> f & stateEffect .~ toEnum a)

instance Default EffectObject where
  def = EffectObject {
    _objectEffect = def,
    _ress = V.empty,
    _stateEffect = Active,
    _slowRate = 3
  }

type Effect = Autonomie (State EffectObject) EffectObject

instance HasObject Effect where object = auto . objectEffect
instance HasEffectObject Effect where effectObject = auto
instance HasStateInt Effect where stateInt = auto . stateInt

instance Default Effect where
  def = Autonomie {
    _auto = def,
    _runAuto = return ()
  }


data KindBullet = KindBullet Int Int deriving (Eq, Show)

data BulletObject = BulletObject {
  _objectBullet :: Object,
  _kindBullet :: KindBullet
  } deriving (Eq, Show)

makeClassy ''BulletObject

instance HasObject BulletObject where object = objectBullet

instance Default BulletObject where
  def = BulletObject {
    _objectBullet = size .~ V2 3 3 $ def,
    _kindBullet = KindBullet 0 0
    }

type Bullet = Autonomie (State BulletObject) BulletObject

instance HasObject Bullet where object = auto . object
instance HasBulletObject Bullet where bulletObject = auto

instance Default Bullet where
  def = Autonomie {
    _auto = def,
    _runAuto = do
      r <- use speed
      t <- use angle
      pos %= (+ fromPolar (r,t))
    }

data StateChara = Alive | Attack | Damaged | Dead deriving (Eq, Enum, Show)

data Chara = Chara {
  _objectChara :: Object,
  _stateChara :: StateChara,
  _hp :: Int,
  _charaEffects :: S.Seq Effect
  }

makeClassy ''Chara

instance HasStateInt Chara where
  stateInt = lens (fromEnum . _stateChara) (\f a -> f & stateChara .~ toEnum a)

instance HasObject Chara where
  object = objectChara

instance Default Chara where
  def = Chara {
    _objectChara = def,
    _stateChara = Alive,
    _hp = 0,
    _charaEffects = S.empty
    }

data Player = Player {
  _charaPlayer :: Chara,
  _keys :: UI.Keys
  }

makeLenses ''Player

instance HasStateInt Player where stateInt = chara . stateInt
instance HasChara Player where chara = charaPlayer
instance HasObject Player where object = chara . object

instance Default Player where
  def = Player {
    _charaPlayer =
      pos .~ V2 320 420 $ 
      speed .~ 2.5 $
      size .~ V2 5 5 $
      hp .~ 10 $
      def,
    _keys = def
    }

data KindEnemy = Zako Int Int | Boss Int Int | Debug deriving (Eq, Show)

data EnemyObject = EnemyObject {
  _charaEnemy :: Chara,
  _kindEnemy :: KindEnemy,
  _shotQ :: S.Seq Bullet,
  _effQ :: S.Seq Effect
  }

makeClassy ''EnemyObject

instance HasStateInt EnemyObject where stateInt = chara . stateInt
instance HasChara EnemyObject where chara = charaEnemy
instance HasObject EnemyObject where object = chara . object

instance Default EnemyObject where
  def = EnemyObject {
    _charaEnemy =
      spXY .~ V2 0 0 $
      size .~ V2 15 15 $
      def,
    _kindEnemy = undefined,
    _shotQ = S.empty,
    _effQ = S.empty
    }

