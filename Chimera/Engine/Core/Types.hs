{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, GADTs #-}
{-# LANGUAGE FunctionalDependencies, FlexibleContexts, FlexibleInstances #-}
module Chimera.Engine.Core.Types where

import FreeGame
import Control.Lens
import Control.Monad.Operational.Mini
import Control.Monad.Operational.TH (makeSingletons)
import Control.Monad.State.Strict
import qualified Data.Vector as V
import qualified Data.Map as M
import Data.Default
import Data.Functor.Product
import Data.Reflection (Given, given)

data Autonomie m a = Autonomie a (m ())

class Autonomic c m a | c -> a, c -> m where
  autonomie :: Lens' c (Autonomie m a)
  auto :: (Autonomic c m a) => Lens' c a
  runAuto :: (Autonomic c m a) => Lens' c (m ())
  
  auto = autonomie . (\f (Autonomie a r) -> (\a' -> Autonomie a' r) `fmap` f a)
  runAuto = autonomie . (\f (Autonomie a r) -> (\r' -> Autonomie a r') `fmap` f r)

data Pattern p q x where
  Hook :: Either (State p ()) (State q ()) -> Pattern p q ()
  Self :: Pattern p q p
  Env :: Pattern p q q
  Yield :: Pattern p q ()

type LookAt p q = ReifiedProgram (Pattern p q)

data BKind = BallLarge | BallMedium | BallSmall | BallFrame | BallTiny |
             Oval | Diamond | Needle deriving (Eq, Ord, Enum, Show)
data BColor = Red | Orange | Yellow | Green | Cyan | Blue | Purple | Magenta
              deriving (Eq, Ord, Enum, Show)

data Resource = Resource {
  _charaImg :: V.Vector Bitmap,
  _bulletImg :: V.Vector (V.Vector Bitmap),
  _effectImg :: V.Vector (V.Vector Bitmap),
  _board :: Bitmap,
  _font :: Font,
  _layerBoard :: Bitmap,
  _portraits :: V.Vector Bitmap,
  _numbers :: V.Vector (Game ()),
  _labels :: M.Map String (Game ()),
  _areaBullet :: BKind -> Vec2
  }

data Memory = Memory {
  _cities :: [String]
}

data Config = Config {
  _windowMode :: WindowMode,
  _windowSize :: BoundingBox2,
  _gameArea :: BoundingBox2,
  _validArea :: BoundingBox2,
  _debugMode :: Bool,
  _titleName :: String
  }

data Object = Object {
  _pos :: Vec2,
  _spXY :: Vec2,
  _speed :: Double,
  _ang :: Double,
  
  _counter :: Int,
  _size :: Vec2
  } deriving (Eq, Show)

class GUIClass c where
  update :: (Given Resource, Given Config) => State c ()
  paint :: (Given Resource) => StateT c Game ()

makeSingletons ''Pattern 
makeLenses ''Resource
makeLenses ''Memory
makeLenses ''Config
makeClassy ''Object

instance Autonomic (Autonomie m a) m a where autonomie = id
instance (Eq a) => Eq (Autonomie m a) where a == b = a^.auto == b^.auto
instance (Show a) => Show (Autonomie m a) where show a = show $ a^.auto

instance (Monad m, Default a) => Default (Autonomie m a) where
  def = Autonomie def (return ())

instance Default Object where
  def = Object {
    _pos = V2 0 0,
    _spXY = V2 0 0,
    _speed = 0,
    _ang = 0,
    _counter = 0,
    _size = V2 1 1
    }

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

