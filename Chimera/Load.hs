{-# LANGUAGE TemplateHaskell #-}
module Chimera.Load (
  Resource
  , font, charaImg, bulletImg, board
  , load
  , bulletImgRect
  ) where

import Graphics.UI.FreeGame
import Control.Lens

data Resource = Resource {
  _font :: Font,
  _charaImg :: (Bitmap, Bitmap),
  _bulletImg :: (Bitmap, Bitmap),
  _board :: Bitmap
  }

makeLenses ''Resource

load :: Game Resource
load = do 
  font <- embedIO $ loadFont "data/font/VL-PGothic-Regular.ttf"
  r1 <- embedIO $ loadBitmapFromFile "data/img/player_reimu.png"
  r2 <- embedIO $ loadBitmapFromFile "data/img/dot_yousei.png"
  r3 <- embedIO $ loadBitmapFromFile "data/img/shot.png"
  b <- embedIO $ loadBitmapFromFile "data/img/board.png"
  
  return $ Resource {
    _font = font,
    _charaImg = (cropBitmap r1 (50,50) (0,0), cropBitmap r2 (32,32) (0,0)),
    _bulletImg = (r3, r3),
    _board = b
  }

data BKind = BallLarge | BallMedium | BallSmall | 
  Oval | Diamond | Needle | BallFrame | BallTiny
  deriving (Eq, Ord, Enum, Show)

data BColor = Red | Orange | Yellow | Green | Cyan | Blue | Purple | Magenta
  deriving (Eq, Ord, Enum, Show)

bulletImgRect :: BKind -> BColor -> Bitmap -> Bitmap
bulletImgRect b c
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

