{-# LANGUAGE TemplateHaskell, GADTs, TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Chimera.STG.Types where

import Graphics.UI.FreeGame
import Control.Lens
import Control.Monad.Operational.Mini (Program, ReifiedProgram, singleton, interpret)
import Control.Monad.Operational.TH (makeSingletons)
import Control.Monad.State.Strict (State, execState, runState)
import Control.Monad.Trans.Class (lift)
import Control.Comonad
import qualified Data.Sequence as S
import qualified Data.Vector as V
import qualified Data.List.NonEmpty as N
import Data.Default

import Chimera.STG.Util
import Chimera.Load
import qualified Chimera.STG.UI as UI

class HasGetResource c where getResource :: c Resource
class HasStateInt c where stateInt :: Lens' c Int

data LookAt p q = LookAt {
  _local :: p,
  _global :: q
  }

makeLenses ''LookAt

data Line c p where
  GetResourceLine :: Line c Resource
  Appear :: c -> Line c ()
  Wait :: Int -> Line c ()
  Stop :: Line c ()

makeSingletons ''Line

type Line' c = ReifiedProgram (Line c)

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

data StateEffect = Active | Inactive deriving (Eq, Enum, Show)

data EffectObject = EffectObject {
  _objectEffect :: Object,
  _ress :: V.Vector Bitmap,
  _stateEffect :: StateEffect,
  _slowRate :: Int
  }

makeClassy ''EffectObject

type Effect = Autonomie (State EffectObject) EffectObject

instance HasObject EffectObject where object = objectEffect
instance HasStateInt EffectObject where
  stateInt = lens (fromEnum . _stateEffect) (\f a -> f & stateEffect .~ toEnum a)
instance Default EffectObject where def = EffectObject def V.empty Active 3

instance HasEffectObject Effect where effectObject = auto
instance HasObject Effect where object = auto . objectEffect
instance HasStateInt Effect where stateInt = auto . stateInt

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

instance HasObject Chara where object = objectChara
instance Default Chara where def = Chara def Alive 0 S.empty

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

data BulletObject = BulletObject {
  _objectBullet :: Object
  } deriving (Eq, Show)

makeClassy ''BulletObject

instance HasObject BulletObject where object = objectBullet
instance Default BulletObject where def = BulletObject (size .~ V2 3 3 $ def)

data EnemyObject = EnemyObject {
  _charaEnemy :: Chara,
  _effectEnemy :: S.Seq Effect
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
    _effectEnemy = S.empty
    }
