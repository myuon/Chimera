{-# LANGUAGE TemplateHaskell, RankNTypes #-}
module Chimera.Object where

import qualified Graphics.UI.FreeGame as Game
import qualified Graphics.UI.FreeGame.GUI.GLFW as GL
import qualified Data.Array as Array
import Control.Lens
import Control.Monad.State

import Chimera.Global

data Object = Object {
  _pos :: Pos,
  _speed :: Double',
  _counter :: Int
  } deriving (Eq, Show)

makeClassy ''Object


data BulletKind = BallLarge | BallMedium | BallSmall | 
  Oval | Diamond | Needle | BallFrame | BallTiny
  deriving (Eq, Ord, Enum, Array.Ix, Show)
  
data BulletColor = Red | Orange | Yellow | Green | Cyan | Blue | Purple | Magenta
  deriving (Eq, Ord, Enum, Array.Ix, Show)

data BulletMotion = Normal | Rotate Double' deriving (Eq, Show)

data BarrangeIndex = BPlayer | BZako Int | BBoss Int | BDebug
  deriving (Eq, Show)

type BulletImg = Array.Array BulletKind
                (Array.Array BulletColor Game.Bitmap)

data Bullet = Bullet {
  _objectBullet :: Object,
  _angle :: Double',
  _kindBullet :: BulletKind,
  _color :: BulletColor,
  _barrage :: BarrangeIndex,
  _param :: Int
  } deriving (Eq, Show)

makeLenses ''Bullet

instance HasObject Bullet where
  object = objectBullet

initBullet :: Pos -> Double' -> Double' -> BulletKind -> BulletColor -> BarrangeIndex -> Bullet
initBullet p s ang k c i = Bullet (Object p s 0) ang k c i 0

initBullet' :: Pos -> Double' -> Double' -> BulletKind -> BulletColor -> BarrangeIndex -> Int -> Bullet
initBullet' p s ang k c i param = Bullet (Object p s 0) ang k c i param

data Chara = Chara {
  _objectChara :: Object,
  _hp :: Int
  } deriving (Show)

makeClassy ''Chara

instance HasObject Chara where
  object = objectChara

initChara :: Pos -> Double' -> Int -> Chara
initChara p s h = Chara (Object p s 0) h


data Player = Player {
  _charaPlayer :: Chara
  } deriving (Show)

makeLenses ''Player

instance HasChara Player where
  chara = charaPlayer

instance HasObject Player where
  object = chara . object

initPlayer :: Player
initPlayer = Player (initChara (toNum $ fromPair (320, 420)) 2 10)


data EnemyKind = Oneway | Spiral | Boss Int deriving (Eq, Show)
data Motion = Mono Int Int | WaitMono Int deriving (Eq, Show)
data MotionState = Go | Stay | Back | Dead deriving (Eq, Show)

data Enemy = Enemy {
  _charaEnemy :: Chara,
  _kindEnemy :: BarrangeIndex,
  _shotQ :: [Bullet],
  _motion :: Motion,
  _mstate :: MotionState
  }

makeLenses ''Enemy

instance HasChara Enemy where
  chara = charaEnemy

instance HasObject Enemy where
  object = chara . object

initEnemy :: Pos -> Double' -> Int -> BarrangeIndex -> Motion -> Enemy
initEnemy p s h i m = Enemy (initChara p s h) i [] m Go

