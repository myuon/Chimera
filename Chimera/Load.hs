{-# LANGUAGE TemplateHaskell, TypeSynonymInstances, FlexibleInstances #-}
module Chimera.Load (
  Resource(..)
  , charaImg, bulletImg, effectImg, board
  , execLoad
  , BKind(..), BColor(..)
  , bulletBitmap
  ) where

import Graphics.UI.FreeGame
import Control.Lens
import Data.Default
import qualified Data.Vector as V
import System.IO.Unsafe (unsafePerformIO)

import Chimera.STG.Util

data Resource = Resource {
  _charaImg :: (Bitmap, Bitmap),
  _bulletImg :: (Bitmap, Bitmap),
  _effectImg :: V.Vector (V.Vector Bitmap),
  _board :: Bitmap
  }

makeLenses ''Resource

-- *unsafe*
instance Default Resource where
  def = unsafePerformIO $ do
    r1 <- loadBitmapFromFile "data/img/player_reimu.png"
    r2 <- loadBitmapFromFile "data/img/dot_yousei.png"
    r3 <- loadBitmapFromFile "data/img/shot.png"
    b <- loadBitmapFromFile "data/img/board.png"
    e1 <- loadBitmapFromFile "data/img/lightring.png"
    e2 <- loadBitmapFromFile "data/img/lightbomb.png"
    e3 <- loadBitmapFromFile "data/img/eff1.png"
    p1_0 <- loadBitmapFromFile "data/img/pat1_0.png"
    p1_1 <- loadBitmapFromFile "data/img/pat1_1.png"
    p1_2 <- loadBitmapFromFile "data/img/pat1_2.png"
    
    return $ Resource {
      _charaImg = (cropBitmap r1 (50,50) (0,0), cropBitmap r2 (32,32) (0,0)),
      _bulletImg = (r3, r3),
      _effectImg = V.fromList $ [
        V.fromList $ cutIntoN 10 e1,
        V.fromList $ cutIntoN 10 e2,
        V.fromList $ cutIntoN 12 e3,
        V.fromList [p1_0, p1_1, p1_2]],
      _board = b
    }

execLoad :: Font -> Resource -> Game ()
execLoad font res = do
  translate (V2 0 0) $ fromBitmap $ (res^.effectImg) V.! 1 V.! 0
  translate (V2 0 0) $ fromBitmap $ (res^.effectImg) V.! 2 V.! 0
  translate (V2 0 0) $ fromBitmap $ (res^.effectImg) V.! 3 V.! 0
  translate (V2 0 0) $ fromBitmap $ (res^.effectImg) V.! 3 V.! 1
  translate (V2 0 0) $ fromBitmap $ (res^.effectImg) V.! 3 V.! 2

data BKind = BallLarge | BallMedium | BallSmall | 
  Oval | Diamond | Needle | BallFrame | BallTiny
  deriving (Eq, Ord, Enum, Show)

data BColor = Red | Orange | Yellow | Green | Cyan | Blue | Purple | Magenta
  deriving (Eq, Ord, Enum, Show)

bulletBitmap :: BKind -> BColor -> Bitmap -> Bitmap
bulletBitmap b c
  | b == BallLarge  = clip (60 * color c) 0 60 60
  | b == BallMedium = clip (30 * color c) 60 30 30
  | b == BallSmall  = clip (20 * color c) 90 20 20
  | b == Oval       = clip (160 + 10 * color c) 90 10 20
  | b == Diamond    = clip (240 + 10 * color c) 90 10 20
  | b == BallFrame  = clip (20 * color c) 110 20 20
  | b == Needle     = clip (5 * color c) 130 5 100
  | b == BallTiny   = clip (40 + 10 * color c) 130 10 10
  where
    color :: BColor -> Int
    color = fromEnum

    clip :: Int -> Int -> Int -> Int -> Bitmap -> Bitmap
    clip a b c d img = cropBitmap img (c,d) (a,b)

