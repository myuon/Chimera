{-# LANGUAGE TemplateHaskell, GADTs, FlexibleContexts, TypeSynonymInstances, FlexibleInstances #-}
module Chimera.STG.Types (
  pos, spXY, speed, angle, counter
  , object, chara, hp
  , Bullet, initBullet
  , HasChara, HasObject
  , dindex
  , Enemy, initEnemy
  , Player, keys, initPlayer
  , img

  , getResource
  , Pattern(..)
  , Danmaku, DIndex(..)
  , Line(..), Stage, appear
  ) where

import Graphics.UI.FreeGame
import Control.Lens
import Control.Monad.Operational.Mini (Program, ReifiedProgram, singleton)
import Control.Monad.Operational.TH (makeSingletons)

import Chimera.STG.Util
import Chimera.Load
import qualified Chimera.STG.UI as UI

data Object = Object {
  _pos :: Vec,
  _spXY :: Vec,
  _speed :: Double',
  _angle :: Double',
  
  _counter :: Int
  } deriving (Eq, Show)

makeClassy ''Object

class HasImg c where
  img :: Simple Lens c Bitmap

data Bullet = Bullet {
  _objectBullet :: Object,
--  _kindBullet :: BulletKind,
--  _color :: BulletColor,
--  _barrage :: BarrangeIndex,
--  _param :: Int
  _imgBullet :: Bitmap
  } deriving (Eq, Show)

makeLenses ''Bullet

instance HasObject Bullet where
  object = objectBullet

instance HasImg Bullet where
  img = imgBullet

initBullet :: Vec -> Double' -> Double' -> Bitmap -> Bullet
initBullet p sp ang = Bullet (Object p undefined sp ang 0)

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
  _imgPlayer :: Bitmap
  }

makeLenses ''Player

instance HasChara Player where
  chara = charaPlayer

instance HasObject Player where
  object = chara . object

instance HasImg Player where
  img = imgPlayer

initPlayer :: Bitmap -> Player
initPlayer = Player 
  (Chara (Object (V2 320 420) undefined 2 undefined 0) 10)
  UI.initKeys

newtype DIndex = DIndex Int

data Enemy = Enemy {
  _charaEnemy :: Chara,
--  _kindEnemy :: BarrangeIndex,
  _shotQ :: [Bullet],
  _dindex :: DIndex,
  _imgEnemy :: Bitmap
--  _motion :: Motion,
--  _mstate :: MotionState
  }

makeLenses ''Enemy

instance HasChara Enemy where
  chara = charaEnemy

instance HasObject Enemy where
  object = chara . object

instance HasImg Enemy where
  img = imgEnemy

initEnemy :: Vec -> Int -> DIndex -> Bitmap -> Enemy
initEnemy p hp = Enemy (initChara p 0 hp) []

class HasGetResource c where
  getResource :: c Resource

data Pattern p where
  Shots :: [Bullet] -> Pattern ()
  GetPlayer :: Pattern Player
  Get :: Pattern Enemy
  Put :: Enemy -> Pattern ()
  GetResourcePattern :: Pattern Resource

type Danmaku = Program Pattern

getResourcePattern :: Danmaku Resource
getResourcePattern = singleton GetResourcePattern

instance HasGetResource Danmaku where
  getResource = getResourcePattern


data Line p where
  GetResourceLine :: Line Resource
  Appear :: Int -> Enemy -> Line ()
 
makeSingletons ''Line

type Stage = ReifiedProgram Line

instance HasGetResource Stage where
  getResource = getResourceLine

