{-# LANGUAGE TemplateHaskell, GADTs #-}
module Chimera.STG.Types (
  pos, spXY, speed, angle, counter, size
  , object, chara, hp
  , Bullet, initBullet, initBullet', kindBullet, KindBullet(..), param
  , HasChara, HasObject
  , Enemy, initEnemy
  , state, State(..), kind, Kind(..)
  , Player, keys
  , img
  ) where

import Graphics.UI.FreeGame
import Control.Lens
import Control.Monad.Operational.Mini (Program, ReifiedProgram, singleton)
import Data.Default

import Chimera.STG.Util
import Chimera.Load
import qualified Chimera.STG.UI as UI

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
    _size = V2 0 0
    }

class HasImg c where
  img :: Simple Lens c Bitmap

data KindBullet = KindBullet Int deriving (Eq, Show)

data Bullet = Bullet {
  _objectBullet :: Object,
--  _kindBullet :: BulletKind,
--  _color :: BulletColor,
  _imgBullet :: Bitmap,
  _kindBullet :: KindBullet,
  _param :: Int
  } deriving (Eq, Show)

makeLenses ''Bullet

instance HasObject Bullet where
  object = objectBullet

instance HasImg Bullet where
  img = imgBullet

instance Default Bullet where
  def = Bullet {
    _objectBullet = size .~ V2 3 3 $ def,
    _imgBullet = undefined,
    _kindBullet = KindBullet 0,
    _param = 0
    }

initBullet :: Vec -> Double' -> Double' -> Bitmap -> KindBullet -> Int -> Bullet
initBullet p sp ang = Bullet (Object p undefined sp ang 0 (V2 3 3))

initBullet' :: Vec -> Double' -> Double' -> BKind -> BColor -> Resource -> KindBullet -> Int -> Bullet
initBullet' p sp ang bk bc res k = initBullet p sp ang (bulletBitmap bk bc (snd $ res^.bulletImg)) k

data Chara = Chara {
  _objectChara :: Object,
  _hp :: Int
  } deriving (Show)

makeClassy ''Chara

instance HasObject Chara where
  object = objectChara

instance Default Chara where
  def = Chara {
    _objectChara = def,
    _hp = 0
    }

initChara :: Vec -> Vec -> Int -> Vec -> Chara
initChara p sp h s = Chara (Object p sp undefined undefined 0 s) h

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

instance Default Player where
  def = Player {
    _charaPlayer =
      pos .~ V2 320 420 $ 
      speed .~ 2.5 $
      size .~ V2 5 5 $
      hp .~ 10 $
      def,
    _keys = def,
    _imgPlayer = undefined
    }

data State = Dead | Alive deriving (Eq, Show)

data Kind = Zako Int | Boss Int | Debug deriving (Eq, Show)

data Enemy = Enemy {
  _charaEnemy :: Chara,
  _shotQ :: [Bullet],
  _imgEnemy :: Bitmap,
  _state :: State,
  _kind :: Kind
  }

makeLenses ''Enemy

instance HasChara Enemy where
  chara = charaEnemy

instance HasObject Enemy where
  object = chara . object

instance HasImg Enemy where
  img = imgEnemy

instance Default Enemy where
  def = Enemy {
    _charaEnemy =
      spXY .~ V2 0 0 $
      size .~ V2 15 15 $
      def,
    _shotQ = [],
    _imgEnemy = undefined,
    _state = Alive,
    _kind = undefined
    }

initEnemy :: Vec -> Int -> Resource -> Kind -> Enemy
initEnemy p h res k =
  pos .~ p $
  hp .~ h $
  img .~ (snd $ res^.charaImg) $
  kind .~ k $
  def
