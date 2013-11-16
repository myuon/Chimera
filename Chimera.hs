{-# LANGUAGE TemplateHaskell #-}
module Chimera where

import Graphics.UI.FreeGame
import Control.Lens

import qualified Chimera.STG as STG
import Data.Time (UTCTime, getCurrentTime, diffUTCTime)

makeLenses ''GUIParam

data Load = Load {
  _font :: Font,
  _charaImg :: (Bitmap, Bitmap),
  _bulletImg :: (Bitmap, Bitmap),
  _board :: Bitmap
  }

makeLenses ''Load

data BKind = BallLarge | BallMedium | BallSmall | 
  Oval | Diamond | Needle | BallFrame | BallTiny
  deriving (Eq, Ord, Enum, Show)

data BColor = Red | Orange | Yellow | Green | Cyan | Blue | Purple | Magenta
  deriving (Eq, Ord, Enum, Show)


initLoad :: Game Load
initLoad = do 
  font <- embedIO $ loadFont "data/font/VL-PGothic-Regular.ttf"
  r1 <- embedIO $ loadBitmapFromFile "data/img/player_reimu.png"
  r2 <- embedIO $ loadBitmapFromFile "data/img/dot_yousei.png"
  r3 <- embedIO $ loadBitmapFromFile "data/img/shot.png"
  b <- embedIO $ loadBitmapFromFile "data/img/board.png"
  
  return $ Load {
    _font = font,
    _charaImg = (cropBitmap r1 (50,50) (0,0), cropBitmap r2 (32,32) (0,0)),
    _bulletImg = (r3, r3),
    _board = b
  }

{-  
  where
    makeBulletImg :: Bitmap -> Obj.BulletImg
    makeBulletImg img = Array.listArray (Obj.BallLarge, Obj.BallTiny)
             [Array.listArray (Obj.Red, Obj.Magenta)
             [bulletImgRect kind color img
              | color <- [Obj.Red .. Obj.Magenta]]
              | kind  <- [Obj.BallLarge .. Obj.BallTiny]]
  
    bulletImgRect :: Obj.BulletKind -> Obj.BulletColor -> Bitmap -> Bitmap
    bulletImgRect b c
      | b == Obj.BallLarge  = clip (60 * color c) 0 60 60
      | b == Obj.BallMedium = clip (30 * color c) 60 30 30
      | b == Obj.BallSmall  = clip (20 * color c) 90 20 20
      | b == Obj.Oval       = clip (160 + 10 * color c) 90 10 20
      | b == Obj.Diamond    = clip (240 + 10 * color c) 90 10 20
      | b == Obj.BallFrame  = clip (20 * color c) 110 20 20
      | b == Obj.Needle     = clip (5 * color c) 130 5 100
      | b == Obj.BallTiny   = clip (40 + 10 * color c) 130 10 10
      where
        color :: Obj.BulletColor -> Int
        color = fromEnum

        clip :: Int -> Int -> Int -> Int -> Bitmap -> Bitmap
        clip a b c d img = cropBitmap img (c,d) (a,b)
-}

data GameFrame = GameFrame {
  _screenMode :: Int,
  _field :: STG.Field,
  _load :: Load,
  _prevTime :: UTCTime
  }

makeLenses ''GameFrame

initGameFrame :: GameFrame
initGameFrame = GameFrame {
  _screenMode = 0,
  _field = STG.initField,
  _load = undefined,
  _prevTime = undefined
  }

start :: GUIParam
start =
  windowTitle .~ "Chimera" $
  clearColor .~ Color 0 0 0.2 1.0 $
  def

step :: Game Bool
step = do
  tick
  keySpecial KeyEsc

mainloop :: GameFrame -> Game GameFrame
{-
mainloop gf = do
  time' <- embedIO $ getCurrentTime
  let fps' = getFPS $ diffUTCTime time' (gf ^. prevTime)
  
  draw (gf ^. load ^. charaImg) (gf ^. load ^. bulletImg) (gf ^. field)
  translate (V2 320 240) $
    fromBitmap (gf ^. load ^. board)

  writeFPS $ "fps:" ++ (show $ fps')
  translate (V2 0 50) .
   colored white .
   text (gf ^. load ^. font) 20 $ show $ length $ gf ^. field ^. Field.bulletE
  
  return $
    key .~ key' $
    field %~ update key' $
    prevTime .~ time' $
    gf

  where
    writeFPS :: String -> Game ()
    writeFPS = translate (V2 0 20) .
               colored white .
               text (gf ^. load ^. font) 20

    getFPS :: (RealFrac a, Fractional a) => a -> Int
    getFPS diff = floor $ (1 / diff)
-}
mainloop = undefined


main :: IO (Maybe a)
main = runGame start $ do
  load' <- initLoad
  time' <- embedIO $ getCurrentTime
  
  run $
    load .~ load' $
    prevTime .~ time' $
    initGameFrame
  quit
  
  where
    run :: GameFrame -> Game ()
    run gf = do
      gf' <- mainloop gf
      step >>= flip unless (run gf')
  
