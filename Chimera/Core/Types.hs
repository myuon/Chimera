{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies  #-}
{-# LANGUAGE FlexibleInstances, GADTs, FlexibleContexts, RankNTypes #-}
module Chimera.Core.Types where

import FreeGame
import Control.Lens
import Control.Monad.Operational.Mini
import Control.Monad.Operational.TH (makeSingletons)
import Control.Monad.State.Strict
import qualified Data.Sequence as S
import qualified Data.Vector as V
import qualified Data.Map as M
import qualified Data.IntMap.Strict as IM
import Data.Default
import Data.Functor.Product

import Chimera.Core.Util

data Autonomie m a = Autonomie a (m ())

class Autonomic c m a | c -> a, c -> m where
  autonomie :: Lens' c (Autonomie m a)
  auto :: (Autonomic c m a) => Lens' c a
  runAuto :: (Autonomic c m a) => Lens' c (m ())
  
  auto = autonomie . (\f (Autonomie a r) -> (\a' -> Autonomie a' r) `fmap` f a)
  runAuto = autonomie . (\f (Autonomie a r) -> (\r' -> Autonomie a r') `fmap` f r)

data StateEffect = Active | Inactive deriving (Eq, Enum, Show)
data ZIndex = Background | OnObject | Foreground deriving (Eq, Show)
data StateChara = Alive | Attack | Damaged | Dead deriving (Eq, Enum, Show)
data StateBullet = PlayerB | EnemyB | Outside deriving (Eq, Ord, Enum, Show)
data BKind = BallLarge | BallMedium | BallSmall | BallFrame | BallTiny |
             Oval | Diamond | Needle deriving (Eq, Ord, Enum, Show)
data BColor = Red | Orange | Yellow | Green | Cyan | Blue | Purple | Magenta
  deriving (Eq, Ord, Enum, Show)

data Pattern p q x where
  Hook :: Either (State p ()) (State q ()) -> Pattern p q ()
  Self :: Pattern p q p
  Env :: Pattern p q q
  Yield :: Pattern p q ()

data Resource = Resource {
  _charaImg :: V.Vector Bitmap,
  _bulletImg :: V.Vector (V.Vector Bitmap),
  _effectImg :: V.Vector (V.Vector Bitmap),
  _board :: Bitmap,
  _font :: Font,
  _layerBoard :: Bitmap,
  _portraits :: V.Vector Bitmap,
  _numbers :: V.Vector (Game ()),
  _labels :: M.Map String (Game ())
  }

data Object = Object {
  _pos :: Vec2,
  _spXY :: Vec2,
  _speed :: Double,
  _angle :: Double,
  
  _counter :: Int,
  _size :: Vec2
  } deriving (Eq, Show)

data Chara = Chara {
  _objectChara :: Object,
  _stateChara :: StateChara,
  _hp :: Int
  }

data EffectObject = EffectObject {
  _objectEffect :: Object,
  _stateEffect :: StateEffect,
  _slowRate :: Int,
  _img :: Resource -> Game (),
  _zIndex :: ZIndex
  }

data BulletObject = BulletObject {
  _objectBullet :: Object,
  _stateBullet :: StateBullet,
  _kind :: BKind,
  _bcolor :: BColor
  } deriving (Eq, Show)

data EnemyObject = EnemyObject {
  _charaEnemy :: Chara,
  _effectIndexes :: S.Seq Int
  }

data Player = Player {
  _charaPlayer :: Chara,
  _keysPlayer :: M.Map Key Int
  }

data Field = Field {
  _player :: Player,
  _enemy :: S.Seq Enemy,
  _bullets :: S.Seq Bullet,
  _effects :: IM.IntMap Effect,
  
  _resource :: Resource,
  _counterF :: Int,
  _isDebug :: Bool,
  _danmakuTitle :: String
  }

type LookAt p q = ReifiedProgram (Pattern p q)
type Danmaku c = LookAt c Field
type Effect = Autonomie (State EffectObject) EffectObject
type Bullet = Autonomie (Danmaku BulletObject) BulletObject
type Enemy = Autonomie (Danmaku EnemyObject) EnemyObject

makeSingletons ''Pattern 
makeLenses ''Resource
makeClassy ''Object
makeClassy ''Chara
makeClassy ''EffectObject
makeClassy ''BulletObject
makeClassy ''EnemyObject
makeLenses ''Player
makeLenses ''Field

class GUIClass c where
  update :: State c ()
  paint :: Resource -> StateT c Game ()

class HasGetResource c where
  getResource :: c Resource

instance Autonomic (Autonomie m a) m a where autonomie = id
instance (Eq a) => Eq (Autonomie m a) where a == b = a^.auto == b^.auto
instance (Show a) => Show (Autonomie m a) where show a = show $ a^.auto

instance HasObject Chara where object = objectChara
instance HasObject EffectObject where object = objectEffect
instance HasEffectObject Effect where effectObject = auto
instance HasObject Effect where object = auto . objectEffect
instance HasObject BulletObject where object = objectBullet
instance HasChara EnemyObject where chara = charaEnemy
instance HasObject EnemyObject where object = chara . object
instance HasChara Player where chara = charaPlayer
instance HasObject Player where object = chara . object
instance HasObject Bullet where object = auto . object
instance HasBulletObject Bullet where bulletObject = auto
instance HasObject Enemy where object = auto . object
instance HasChara Enemy where chara = auto . chara
instance HasEnemyObject Enemy where enemyObject = auto

instance HasGetResource (Danmaku c) where
  getResource = (^.resource) `fmap` env

instance (Monad m, Default a) => Default (Autonomie m a) where
  def = Autonomie def (return ())

instance Default Object where
  def = Object {
    _pos = V2 0 0,
    _spXY = V2 0 0,
    _speed = 0,
    _angle = 0,
    _counter = 0,
    _size = V2 1 1
    }

instance Default Chara where
  def = Chara { 
    _objectChara = def,
    _stateChara = Alive,
    _hp = 0
    }

instance Default EffectObject where
  def = EffectObject { 
    _objectEffect = def, 
    _stateEffect = Active,
    _slowRate = 3,
    _img = error "_img is not defined",
    _zIndex = Background
    }

instance Default BulletObject where
  def = BulletObject { 
    _objectBullet = (size .~ V2 3 3 $ def),
    _stateBullet = EnemyB,
    _kind = BallMedium,
    _bcolor = Red
    }

instance Default EnemyObject where
  def = EnemyObject {
    _charaEnemy =
      spXY .~ V2 0 0 $
      size .~ V2 15 15 $
      def,
    _effectIndexes = S.empty
    }

instance Default Player where
  def = Player {
    _charaPlayer =
      pos .~ V2 320 420 $ 
      speed .~ 2.5 $
      size .~ V2 5 5 $
      hp .~ 10 $
      def,
    _keysPlayer = M.fromList $ zip keyList [0..]
    }

instance Default Field where
  def = Field {
    _player = def,
    _enemy = S.empty,
    _bullets = S.empty,
    _effects = IM.empty,
    
    _resource = error "_resource is not defined.",
    _counterF = 0,
    _isDebug = False,
    _danmakuTitle = ""
    }

keyList :: [Key]
keyList = [
  KeyUp, KeyDown, KeyRight, KeyLeft, KeyLeftShift, KeyRightShift,
  charToKey 'Z', charToKey 'X']

runLookAt :: p -> q -> LookAt p q () -> 
             State (Product (State p) (State q) ()) (LookAt p q ())
runLookAt p q = go where
  go (Hook (Left f) :>>= next) = modify (>> Pair f (return ())) >> go (next ())
  go (Hook (Right g) :>>= next) = modify (>> Pair (return ()) g) >> go (next ())
  go (Self :>>= next) = get >>= \(Pair f _) -> go (next $ f `execState` p)
  go (Env :>>= next) = get >>= \(Pair _ g) -> go (next $ g `execState` q)
  go (Yield :>>= next) = return (next ())
  go (Return next) = return (Return next)

runLookAtAll :: p -> q -> LookAt p q () -> Product (State p) (State q) ()
runLookAtAll p q m = go m `execState` return () where
  go (Hook (Left f) :>>= next) = modify (>> Pair f (return ())) >> go (next ())
  go (Hook (Right g) :>>= next) = modify (>> Pair (return ()) g) >> go (next ())
  go (Self :>>= next) = get >>= \(Pair f _) -> go (next $ f `execState` p)
  go (Env :>>= next) = get >>= \(Pair _ g) -> go (next $ g `execState` q)
  go (Yield :>>= next) = go (next ())
  go (Return _) = return ()

collide :: (HasObject c, HasObject b) => c -> b -> Bool
collide oc ob = let oc' = extend oc; ob' = extend ob; in
  detect ob' oc' || detect oc' ob'
  where
    extend :: (HasObject c) => c -> c
    extend x = x & size -~ V2 (x^.speed) 0 `rotate2` (-x^.angle) + (x^.spXY)
--    extend x = x & size +~ (x^.speed) * (V2 1 0) `rotate2` (-x^.angle)

    detect :: (HasObject c, HasObject c') => c -> c' -> Bool
    detect a b = 
      let V2 w' h' = a^.size
          r = \v -> rotate2 v $ a^.angle in
      or [(a^.pos) `isIn` b,
          (a^.pos + r (V2   w'    h' )) `isIn` b,
          (a^.pos + r (V2 (-w')   h' )) `isIn` b,
          (a^.pos + r (V2   w'  (-h'))) `isIn` b,
          (a^.pos + r (V2 (-w') (-h'))) `isIn` b]
    
    isIn :: (HasObject c) => Vec2 -> c -> Bool
    isIn p box = isInCentoredBox (p-box^.pos) where
      isInCentoredBox :: Vec2 -> Bool
      isInCentoredBox p' = let V2 px' py' = p' `rotate2` (-box^.angle) in
        abs px' < (box^.size^._x)/2 && abs py' < (box^.size^._y)/2

