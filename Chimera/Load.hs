{-# LANGUAGE TemplateHaskell, TypeSynonymInstances, FlexibleInstances #-}
module Chimera.Load (
  Resource(..)
  , charaImg, bulletImg, effectImg, board
  , BKind(..), BColor(..)
  , bulletBitmap
  ) where

import Graphics.UI.FreeGame
import Control.Lens
import Data.Default
import qualified Data.Vector as V
import System.IO.Unsafe (unsafePerformIO)

data Resource = Resource {
  _charaImg :: (Bitmap, Bitmap),
  _bulletImg :: (Bitmap, Bitmap),
  _effectImg :: V.Vector Bitmap,
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
    
    return Resource {
      _charaImg = (cropBitmap r1 (50,50) (0,0), cropBitmap r2 (32,32) (0,0)),
      _bulletImg = (r3, r3),
      _effectImg = V.fromList [e1],
      _board = b
    }

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

