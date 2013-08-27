{-# LANGUAGE TemplateHaskell, RankNTypes #-}
module Object where

import Control.Lens
import Control.Monad.State

import Global

-- ################################# 
-- # Object
-- #################################
data Object = Object {
  _pos :: Pos,
  _speed :: Double
  } deriving (Eq, Show)

makeClassy ''Object


-- ################################# 
-- # Bullet
-- #################################
data Bullet = Bullet {
  _objectBullet :: Object,
  _angle :: Double
  } deriving (Eq, Show)

makeLenses ''Bullet

instance HasObject Bullet where
  object = objectBullet

initBullet :: Pos -> Double -> Double -> Bullet
initBullet p s ang = Bullet (Object p s) ang


-- ################################# 
-- # Chara
-- #################################
data Chara = Chara {
  _objectChara :: Object,
  _counter :: Int,
  _hp :: Int
  } deriving (Show)

makeClassy ''Chara

instance HasObject Chara where
  object = objectChara

initChara :: PoInt -> Double -> Int -> Chara
initChara p s h = Chara (Object (toNum p) s) 0 h


-- ################################# 
-- # Player
-- #################################
data Player = Player {
  _charaPlayer :: Chara
  }

makeLenses ''Player

instance HasChara Player where
  chara = charaPlayer

instance HasObject Player where
  object = chara . object

initPlayer :: Player
initPlayer = Player (initChara (320, 180) 2 10)


-- ################################# 
-- # Enemy
-- #################################
data EnemyKind = Oneway | Spiral deriving (Eq, Show)
data Motion = Mono Int Int deriving (Eq, Show)
data MotionState = Go | Stay | Back | Dead deriving (Eq, Show)

data Enemy = Enemy {
  _charaEnemy :: Chara,
  _kind :: EnemyKind,
  _shotQ :: [Bullet],
  _motion :: Motion,
  _mstate :: MotionState
  }

makeLenses ''Enemy

instance HasChara Enemy where
  chara = charaEnemy

instance HasObject Enemy where
  object = chara . object

initEnemy :: PoInt -> Double -> Int -> EnemyKind -> Motion -> Enemy
initEnemy p s h k m = Enemy (initChara p s h) k [] m Go

