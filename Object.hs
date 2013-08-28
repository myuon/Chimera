{-# LANGUAGE TemplateHaskell, RankNTypes #-}
module Object where

import qualified Graphics.UI.SDL as SDL
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
data BulletKind = BallLarge | BallMedium | BallSmall | Oval | Diamond | Needle | BallFrame
  deriving (Eq, Show)
  
data BulletColor = Red | Orange | Yellow | Green | Cyan | Blue | Purple | Magenta
  deriving (Eq, Show, Enum)

data Bullet = Bullet {
  _objectBullet :: Object,
  _angle :: Double,
  _kindBullet :: BulletKind,
  _color :: BulletColor
  } deriving (Eq, Show)

makeLenses ''Bullet

instance HasObject Bullet where
  object = objectBullet

initBullet :: Pos -> Double -> Double -> BulletKind -> BulletColor -> Bullet
initBullet p s ang k c = Bullet (Object p s) ang k c

bulletImgRect :: BulletKind -> BulletColor -> SDL.Rect
bulletImgRect BallLarge c = SDL.Rect (60 * fromEnum c) 0 60 60
bulletImgRect BallMedium c = SDL.Rect (30 * fromEnum c) 60 30 30
bulletImgRect BallSmall c = SDL.Rect (20 * fromEnum c) 90 20 20
bulletImgRect Oval c = SDL.Rect (160 + 10 * fromEnum c) 90 10 20
bulletImgRect Diamond c = SDL.Rect (240 + 10 * fromEnum c) 90 10 20
bulletImgRect BallFrame c = SDL.Rect (20 * fromEnum c) 110 20 20
bulletImgRect Needle c = SDL.Rect (5 * fromEnum c) 130 5 100


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
  _kindEnemy :: EnemyKind,
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

