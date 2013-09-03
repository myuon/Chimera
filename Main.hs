{-# LANGUAGE TemplateHaskell, FlexibleContexts, ImpredicativeTypes #-}
module Main where

import qualified Graphics.UI.FreeGame as Game
import qualified Graphics.UI.FreeGame.GUI.GLFW as GL
import qualified Graphics.UI.FreeGame.Internal.Finalizer as Fin
import Control.Lens
import Control.Monad
import Control.Applicative

import Global
import qualified Data.Array as Array
import qualified Data.Time as Time
import qualified Object as Obj
import qualified Key
import qualified Field

makeLenses ''Game.GUIParam

data Load = Load {
  _font :: Game.Font,
  _charaImg :: (Game.Bitmap, Game.Bitmap),
  _bulletImg :: (Obj.BulletImg, Obj.BulletImg)
  }

makeLenses ''Load

initLoad :: Game.Game Load
initLoad = do 
  font <- Game.embedIO $ Game.loadFont "data/font/VL-PGothic-Regular.ttf"
  r1 <- Game.embedIO $ Game.loadBitmapFromFile "data/img/player_reimu.png"
  r2 <- Game.embedIO $ Game.loadBitmapFromFile "data/img/dot_yousei.png"
  r3 <- Game.embedIO $ Game.loadBitmapFromFile "data/img/shot.png"
  
  return $ Load {
    _font = font,
    _charaImg = (Game.cropBitmap r1 (50,50) (0,0), Game.cropBitmap r2 (32,32) (0,0)),
    _bulletImg = (\x -> (x,x)) $! makeBulletImg r3
  }
  
  where
    makeBulletImg :: Game.Bitmap -> Obj.BulletImg
    makeBulletImg img = Array.listArray (Obj.BallLarge, Obj.BallTiny)
             [Array.listArray (Obj.Red, Obj.Magenta)
             [bulletImgRect kind color img
              | color <- [Obj.Red .. Obj.Magenta]]
              | kind  <- [Obj.BallLarge .. Obj.BallTiny]]
  
    bulletImgRect :: Obj.BulletKind -> Obj.BulletColor -> Game.Bitmap -> Game.Bitmap
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

        clip :: Int -> Int -> Int -> Int -> Game.Bitmap -> Game.Bitmap
        clip a b c d img = Game.cropBitmap img (c,d) (a,b)

data GameFrame = GameFrame {
  _screenMode :: Int,
  _key :: Key.Keys,
  _field :: Field.Field,
  _load :: Load,
  _prevTime :: Time.UTCTime
  }

makeLenses ''GameFrame

initGameFrame :: GameFrame
initGameFrame = GameFrame {
  _screenMode = 0,
  _key = Key.initKeys,
  _field = Field.initField,
  _load = undefined,
  _prevTime = undefined
  }

start :: Game.GUIParam
start =
  windowTitle .~ "Chimera" $
  clearColor .~ Game.Color 0 0 0.2 1.0 $
  Game.def

step :: Game.Game Bool
step = do
  Game.tick
  Game.keySpecial Game.KeyEsc

mainloop :: GameFrame -> Game.Game GameFrame
mainloop gf = do
  key' <- Key.update (gf ^. key)
  time' <- Game.embedIO $ Time.getCurrentTime
  let fps' = getFPS $ Time.diffUTCTime time' (gf ^. prevTime)
  
  Field.draw (gf ^. load ^. charaImg) (gf ^. load ^. bulletImg) (gf ^. field)
  writeFPS $ "fps:" ++ (show $ fps')
  Game.translate (Game.V2 0 50) .
   Game.colored Game.white .
   Game.text (gf ^. load ^. font) 20 $ show $ length $ gf ^. field ^. Field.bulletE
  
  return $
    key .~ key' $
    field %~ Field.update key' $
    prevTime .~ time' $
    gf

  where
    writeFPS :: String -> Game.Game ()
    writeFPS = Game.translate (Game.V2 0 20) .
               Game.colored Game.white .
               Game.text (gf ^. load ^. font) 20

    getFPS :: (RealFrac a, Fractional a) => a -> Int
    getFPS diff = floor $ (1 / diff)

main :: IO (Maybe a)
main = Game.runGame start $ do
  load' <- initLoad
  time' <- Game.embedIO $ Time.getCurrentTime

  run $
    load .~ load' $
    prevTime .~ time' $
    initGameFrame
  Game.quit
  
  where
    run :: GameFrame -> Game.Game ()
    run gf = do
      gf' <- mainloop gf
      step >>= flip unless (run gf')
  
