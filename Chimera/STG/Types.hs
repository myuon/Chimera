{-# LANGUAGE TemplateHaskell, GADTs, TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Chimera.STG.Types where

import Graphics.UI.FreeGame
import Control.Lens
import Control.Monad.Operational.Mini (Program, interpret)
import Control.Monad.Operational.TH (makeSingletons)
import Control.Monad.State.Strict (State, StateT, runState)
import qualified Data.Sequence as S
import qualified Data.Vector as V
import Data.Default

import Chimera.STG.Util
import qualified Chimera.STG.UI as UI

data Resource = Resource {
  _charaImg :: V.Vector Bitmap,
  _bulletImg :: V.Vector (V.Vector Bitmap),
  _effectImg :: V.Vector (V.Vector Bitmap),
  _board :: Bitmap,
  _font :: Font,
  _layerBoard :: Bitmap,
  _portraits :: V.Vector Bitmap
  }

makeLenses ''Resource

class HasGetResource c where getResource :: c Resource

data LookAt p q = LookAt {
  _local :: p,
  _global :: q
  }

makeLenses ''LookAt

class GUIClass c where
  update :: State c ()
  draw :: Resource -> StateT c Game ()

data Pattern p q x where
  GetLocal :: Pattern p q p
  PutLocal :: p -> Pattern p q ()
  GetGlobal :: Pattern p q q
  PutGlobal :: q -> Pattern p q ()

makeSingletons ''Pattern

type Runner p q = Program (Pattern p q)

runPattern :: Runner p q () -> State (LookAt p q) ()
runPattern = interpret step
  where
    step :: Pattern p q a -> State (LookAt p q) a
    step GetLocal = use local
    step (PutLocal p) = local .= p
    step GetGlobal = use global
    step (PutGlobal p) = global .= p

liftState :: Runner p q x -> (x -> Runner p q ()) -> State x a -> Runner p q a
liftState getS putS s = do
  f <- getS
  let (s', f') = s `runState` f
  putS f'
  return s'

data Object = Object {
  _pos :: Vec,
  _spXY :: Vec,
  _speed :: Double',
  _angle :: Double',
  
  _counter :: Int,
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
    _size = V2 1 1
    }

data StateEffect = Active | Inactive deriving (Eq, Enum, Show)
data ZIndex = Background | OnObject | Foreground deriving (Eq, Show)

data EffectObject = EffectObject {
  _objectEffect :: Object,
  _stateEffect :: StateEffect,
  _slowRate :: Int,
  _img :: Resource -> Bitmap,
  _zIndex :: ZIndex
  }

makeClassy ''EffectObject

type Effect = Autonomie (State EffectObject) EffectObject

instance HasObject EffectObject where object = objectEffect
instance Default EffectObject where
  def = EffectObject { 
    _objectEffect = def, 
    _stateEffect = Active,
    _slowRate = 3,
    _img = undefined,
    _zIndex = Background
    }

instance HasEffectObject Effect where effectObject = auto
instance HasObject Effect where object = auto . objectEffect

data StateChara = Alive | Attack | Damaged | Dead deriving (Eq, Enum, Show)

data Chara = Chara {
  _objectChara :: Object,
  _stateChara :: StateChara,
  _hp :: Int,
  _charaEffects :: S.Seq Effect
  }

makeClassy ''Chara

instance HasObject Chara where object = objectChara
instance Default Chara where def = Chara def Alive 0 S.empty

data Player = Player {
  _charaPlayer :: Chara,
  _keys :: UI.Keys
  }

makeLenses ''Player

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

data StateBullet = PlayerB | EnemyB | Outside deriving (Eq, Ord, Enum, Show)
data BKind = BallLarge | BallMedium | BallSmall | 
             Oval | Diamond | Needle | BallFrame | BallTiny
  deriving (Eq, Ord, Enum, Show)

data BColor = Red | Orange | Yellow | Green | Cyan | Blue | Purple | Magenta
  deriving (Eq, Ord, Enum, Show)

data BulletObject = BulletObject {
  _objectBullet :: Object,
  _stateBullet :: StateBullet,
  _kind :: BKind,
  _color :: BColor
  } deriving (Eq, Show)

makeClassy ''BulletObject

instance HasObject BulletObject where object = objectBullet
instance Default BulletObject where
  def = BulletObject { 
    _objectBullet = (size .~ V2 3 3 $ def),
    _stateBullet = EnemyB,
    _kind = BallMedium,
    _color = Red
    }

data EnemyObject = EnemyObject {
  _charaEnemy :: Chara,
  _effectEnemy :: S.Seq Effect
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
    _effectEnemy = S.empty
    }

