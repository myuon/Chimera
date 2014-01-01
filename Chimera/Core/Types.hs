{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies  #-}
{-# LANGUAGE FlexibleInstances, GADTs, FlexibleContexts, RankNTypes #-}
module Chimera.Core.Types where

import Graphics.UI.FreeGame
import Control.Lens
import Control.Monad.Operational.Mini (Program, interpret)
import Control.Monad.Operational.TH (makeSingletons)
import Control.Monad.State.Strict (State, StateT, execState, runState, get, put)
import qualified Data.Sequence as S
import qualified Data.Vector as V
import qualified Data.Foldable as F
import Data.Default

import Chimera.Core.Util

data Autonomie m a = Autonomie {
  _auto :: a,
  _runAuto :: m ()
  }

makeLensesFor [("_auto", "__auto"),
               ("_runAuto", "__runAuto")] ''Autonomie

instance (Monad m, Default a) => Default (Autonomie m a) where
  def = Autonomie {
    _auto = def,
    _runAuto = return ()
    }

class Autonomic c m a | c -> a, c -> m where
  autonomie :: Lens' c (Autonomie m a)

instance Autonomic (Autonomie m a) m a where
  autonomie = id

auto :: (Autonomic c m a) => Lens' c a
auto = autonomie . __auto

runAuto :: (Autonomic c m a) => Lens' c (m ())
runAuto = autonomie . __runAuto

instance (Eq a) => Eq (Autonomie m a) where
  a == b = a^.auto == b^.auto

instance (Show a) => Show (Autonomie m a) where
  show a = show $ a^.auto
  
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

data StateChara = Alive | Attack | Damaged | Dead deriving (Eq, Enum, Show)

data Chara = Chara {
  _objectChara :: Object,
  _stateChara :: StateChara,
  _hp :: Int
  }

makeClassy ''Chara

instance HasObject Chara where object = objectChara
instance Default Chara where
  def = Chara { 
    _objectChara = def,
    _stateChara = Alive,
    _hp = 0
    }

data StateBullet = PlayerB | EnemyB | Outside deriving (Eq, Ord, Enum, Show)
data BKind = BallLarge | BallMedium | BallSmall | 
             Oval | Diamond | Needle | BallFrame | BallTiny
  deriving (Eq, Ord, Enum, Show)

data BColor = Red | Orange | Yellow | Green | Cyan | Blue | Purple | Magenta
  deriving (Eq, Ord, Enum, Show)

data EffectObject = EffectObject {
  _objectEffect :: Object,
  _stateEffect :: StateEffect,
  _slowRate :: Int,
  _img :: Resource -> Game (),
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

runLookAt :: Lens' f (S.Seq a) -> (f -> a -> (a, S.Seq (State f ()))) -> State f ()
runLookAt member go = do
  f <- get
  (s', fs) <- scanSeq (go f) `fmap` use member
  member .= s'
  f <- get
  put $ F.foldl (\g u -> u `execState` g) f fs
  
  where
    scanSeq :: (a -> (a, S.Seq b)) -> S.Seq a -> (S.Seq a, S.Seq b)
    scanSeq f es = let pairs = fmap f es in 
      (fmap fst pairs, (F.foldl (S.><) S.empty $ fmap snd pairs))

collide :: (HasObject c, HasObject b) => c -> b -> Bool
collide c b = case b^.speed > b^.size^._x || b^.speed > b^.size^._y of
  True -> let b' = (b & size +~ fromPolar (b^.speed, -b^.angle))
          in detect b' c || detect c b'
  False -> detect b c || detect c b
  where
    detect :: (HasObject c, HasObject c') => c -> c' -> Bool
    detect a b = 
      let V2 w' h' = a^.size
          r = rot2D (a^.angle) in
      or $ [(a^.pos) `isIn` b,
            (a^.pos + r !* (V2   w'    h' )) `isIn` b,
            (a^.pos + r !* (V2 (-w')   h' )) `isIn` b,
            (a^.pos + r !* (V2   w'  (-h'))) `isIn` b,
            (a^.pos + r !* (V2 (-w') (-h'))) `isIn` b]
    
    isIn :: (HasObject c) => Vec -> c -> Bool
    isIn p box = isInCentoredBox (p-box^.pos) where
      isInCentoredBox :: Vec -> Bool
      isInCentoredBox p' = 
        let V2 px' py' = rot2D (-box^.angle) !* p' in
        abs px' < (box^.size^._x)/2 && abs py' < (box^.size^._y)/2

