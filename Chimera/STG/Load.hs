{-# LANGUAGE TemplateHaskell, TypeSynonymInstances, FlexibleInstances #-}
module Chimera.STG.Load (
  Resource(..)
  , GetPicture, picture
  , charaImg, bulletImg, effectImg, board
  , execLoad
  , BKind(..), BColor(..)
  ) where

import Graphics.UI.FreeGame
import Control.Lens
import Data.Default
import qualified Data.Vector as V
import System.IO.Unsafe (unsafePerformIO)

import Chimera.STG.Util
import Chimera.STG.Types
import Chimera.STG.World

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
    e4 <- loadBitmapFromFile "data/img/eff2.png"
    p1_0 <- loadBitmapFromFile "data/img/pat1_0.png"
    p1_1 <- loadBitmapFromFile "data/img/pat1_1.png"
    p1_2 <- loadBitmapFromFile "data/img/pat1_2.png"
    
    return $ Resource {
      _charaImg = V.fromList [cropBitmap r1 (50,50) (0,0),
                              cropBitmap r2 (32,32) (0,0)],
      _bulletImg = splitBulletBitmaps r3,
      _effectImg = V.fromList $ [
        V.fromList $ cutIntoN 10 e1,
        V.fromList $ cutIntoN 10 e2,
        V.fromList $ cutIntoN 12 e3,
        V.fromList [p1_0, p1_1, p1_2],
        V.fromList $ cutIntoN 14 e4],
      _board = b
    }

class GetPicture c where
  picture :: Resource -> c -> Bitmap

execLoad :: Font -> Resource -> Game ()
execLoad font res = do
  tick
  translate (V2 30 30) . colored white . text (font) 20 $ "読み込み中…"
  tick
  translate (V2 0 0) $ fromBitmap $ (res^.charaImg) V.! 0
  translate (V2 0 0) $ fromBitmap $ (res^.charaImg) V.! 1
  translate (V2 0 0) $ fromBitmap $ (res^.bulletImg) V.! 0 V.! 0
  translate (V2 0 0) $ fromBitmap $ (res^.effectImg) V.! 1 V.! 0
  translate (V2 0 0) $ fromBitmap $ (res^.effectImg) V.! 2 V.! 0
  translate (V2 0 0) $ fromBitmap $ (res^.effectImg) V.! 3 V.! 0
  translate (V2 0 0) $ fromBitmap $ (res^.effectImg) V.! 3 V.! 1
  translate (V2 0 0) $ fromBitmap $ (res^.effectImg) V.! 3 V.! 2
  translate (V2 0 0) $ fromBitmap $ (res^.effectImg) V.! 4 V.! 0

splitBulletBitmaps :: Bitmap -> V.Vector (V.Vector Bitmap)
splitBulletBitmaps img = 
  V.fromList [V.fromList [clipBulletBitmap (toEnum k) (toEnum c) img
                         | c <- [fromEnum Red .. fromEnum Magenta]] 
             | k <- [fromEnum BallLarge .. fromEnum BallTiny]]

getBulletBitmap :: V.Vector (V.Vector Bitmap) -> BKind -> BColor -> Bitmap
getBulletBitmap imgs bk bc = imgs V.! (fromEnum bk) V.! (fromEnum bc)

clipBulletBitmap :: BKind -> BColor -> Bitmap -> Bitmap
clipBulletBitmap b c
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

instance GetPicture Player where
  picture res _ = (res^.charaImg) V.! 0

instance GetPicture Enemy where
  picture res _ = (res^.charaImg) V.! 1

instance GetPicture Bullet where
  picture res b = getBulletBitmap (res^.bulletImg) (b^.kind) (b^.color)

