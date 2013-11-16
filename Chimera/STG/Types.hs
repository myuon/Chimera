{-# LANGUAGE TemplateHaskell, GADTs #-}
module Chimera.STG.Types (
  pos, spXY, speed, angle, counter
  , object, chara, hp
  , Bullet, initBullet
  , Enemy, initEnemy
  , Player, keys, initPlayer
  , img

  , Pattern'(..)
  , Danmaku
  ) where

import Graphics.UI.FreeGame
import Control.Lens
import Control.Monad.Operational.Mini (Program)

import Chimera.STG.Util
import qualified Chimera.STG.UI as UI

data Object = Object {
  _pos :: Vec,
  _spXY :: Vec,
  _speed :: Double',
  _angle :: Double',
  
  _counter :: Int
  } deriving (Eq, Show)

makeClassy ''Object

{-
data BulletKind = BallLarge | BallMedium | BallSmall | 
  Oval | Diamond | Needle | BallFrame | BallTiny
  deriving (Eq, Ord, Enum, Show)
  
data BulletColor = Red | Orange | Yellow | Green | Cyan | Blue | Purple | Magenta
  deriving (Eq, Ord, Enum, Array.Ix, Show)

data BulletMotion = Normal | Rotate Double' deriving (Eq, Show)

data BarrangeIndex = BPlayer | BZako Int | BBoss Int | BDebug
  deriving (Eq, Show)

type BulletImg = Array.Array BulletKind
                (Array.Array BulletColor Game.Bitmap)
-}

data Bullet = Bullet {
  _objectBullet :: Object
--  _kindBullet :: BulletKind,
--  _color :: BulletColor,
--  _barrage :: BarrangeIndex,
--  _param :: Int
  } deriving (Eq, Show)

makeLenses ''Bullet

instance HasObject Bullet where
  object = objectBullet

{-
initBullet :: Pos -> Double' -> Double' -> BulletKind -> BulletColor -> BarrangeIndex -> Bullet
initBullet p s ang k c i = Bullet (Object p s 0) ang k c i 0

initBullet' :: Pos -> Double' -> Double' -> BulletKind -> BulletColor -> BarrangeIndex -> Int -> Bullet
initBullet' p s = Bullet (Object p s 0)
-}

initBullet :: Vec -> Double' -> Double' -> Bullet
initBullet p sp ang = Bullet Object { _pos = p, _spXY = undefined, _speed = sp, _angle = ang, _counter = 0}

data Chara = Chara {
  _objectChara :: Object,
  _hp :: Int
  } deriving (Show)

makeClassy ''Chara

instance HasObject Chara where
  object = objectChara

initChara :: Vec -> Vec -> Int -> Chara
initChara p sp = Chara Object { _pos = p, _spXY = sp, _speed = undefined, _angle = undefined, _counter = 0 }


data Player = Player {
  _charaPlayer :: Chara,
  _keys :: UI.Keys,
  _img :: Bitmap
  }

makeLenses ''Player

instance HasChara Player where
  chara = charaPlayer

instance HasObject Player where
  object = chara . object

initPlayer :: Bitmap -> Player
initPlayer = Player 
  (Chara (Object (V2 320 420) undefined 2 undefined 0) 10)
  (UI.initKeys)

{-
data EnemyKind = Oneway | Spiral | Boss Int deriving (Eq, Show)
data Motion = Mono Int Int | WaitMono Int deriving (Eq, Show)
data MotionState = Go | Stay | Back | Dead deriving (Eq, Show)
-}


data Enemy = Enemy {
  _charaEnemy :: Chara,
--  _kindEnemy :: BarrangeIndex,
  _shotQ :: [Bullet]
--  _motion :: Motion,
--  _mstate :: MotionState
  }

makeLenses ''Enemy

instance HasChara Enemy where
  chara = charaEnemy

instance HasObject Enemy where
  object = chara . object

{-
initEnemy :: Vec -> Double' -> Int -> BarrangeIndex -> Motion -> Enemy
initEnemy p s h i m = Enemy (initChara p s h) i [] m Go
-}

initEnemy :: Vec -> Vec -> Int -> Enemy
initEnemy p v hp = Enemy (initChara p v hp) []

data Pattern' p where
  Shots :: [Bullet] -> Pattern' ()
  GetPlayer :: Pattern' Player
  Get :: Pattern' Enemy
  Put :: Enemy -> Pattern' ()

type Danmaku = Program Pattern'
