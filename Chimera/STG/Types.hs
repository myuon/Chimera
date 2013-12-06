{-# LANGUAGE TemplateHaskell, GADTs, TypeSynonymInstances, FlexibleInstances #-}
module Chimera.STG.Types where

import Graphics.UI.FreeGame
import Control.Lens
import Control.Monad.Operational.Mini (Program, ReifiedProgram, singleton)
import Control.Monad.State.Strict (State)
import qualified Data.Sequence as S
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

data Chara = Chara {
  _objectChara :: Object,
  _hp :: Int
  } deriving (Show)

makeClassy ''Chara

instance HasObject Chara where
  object = objectChara

instance Default Chara where
  def = Chara {
    _objectChara = def,
    _hp = 0
    }

data Player = Player {
  _charaPlayer :: Chara,
  _keys :: UI.Keys
  }

makeLenses ''Player

instance HasChara Player where
  chara = charaPlayer

instance HasObject Player where
  object = chara . object

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

data StateEnemy = Dead | Alive | Attack deriving (Eq, Show)

data KindEnemy = Zako Int Int | Boss Int Int | Debug deriving (Eq, Show)

data EnemyObject = EnemyObject {
  _charaEnemy :: Chara,
  _stateEnemy :: StateEnemy,
  _kindEnemy :: KindEnemy,
  _shotQ :: S.Seq Bullet
  }

makeClassy ''EnemyObject

instance HasChara EnemyObject where chara = charaEnemy
instance HasObject EnemyObject where object = chara . object

instance Default EnemyObject where
  def = EnemyObject {
    _charaEnemy =
      spXY .~ V2 0 0 $
      size .~ V2 15 15 $
      def,
    _stateEnemy = Alive,
    _kindEnemy = undefined,
    _shotQ = S.empty
    }

