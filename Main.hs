{-# LANGUAGE TemplateHaskell, FlexibleContexts, ImplicitParams #-}
module Main where

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as SDLI
import qualified Graphics.UI.SDL.Framerate as SDLF
import Control.Lens
import Control.Monad
import Control.Monad.State
import Control.Applicative
import Control.Bool

import qualified Data.Time as Time
import Data.Word (Word32)
import Global
import qualified Player
import qualified Key
import qualified Field
import Debug.Trace

data GameFrame = GameFrame {
  _screenMode :: Int,
  _screen :: IO SDL.Surface,
  _key :: Key.Keys,
  _field :: Field.Field,
  _pic :: Pic,
  _fps :: SDLF.FPSManager
  }

makeLenses ''GameFrame

initGameFrame :: GameFrame
initGameFrame = GameFrame {
  _screenMode = 0,
  _screen = SDL.getVideoSurface,
  _key = Key.initKeys,
  _field = Field.initField,
  _pic = undefined,
  _fps = undefined
  }

start :: IO ()
start = do
  SDL.init [SDL.InitEverything]
  SDL.setVideoMode 640 480 32 []
  SDL.setCaption "Chimera" "chimera"
  SDL.enableKeyRepeat 1 10
  return ()
  
run :: IO GameFrame -> IO ()
run gfIO = do
  gf <- gfIO
  let gf' = mainloop gf
  (gf ^. screen) >>= step >>= flip unless (run gf')

step :: SDL.Surface -> IO Bool
step screen =
  fmap and . sequence $ [
    SDL.tryFlip screen, 
    clearDisplay, 
    SDL.pollEvent >>= (\ev -> return $ isQuit ev)
    ]
  
  where
    isQuit :: SDL.Event -> Bool
    isQuit (SDL.KeyDown (SDL.Keysym SDL.SDLK_ESCAPE _ _)) = True
    isQuit SDL.Quit = True
    isQuit _ = False
    
    clearDisplay :: IO Bool
    clearDisplay = do
      color <- SDL.mapRGB (SDL.surfaceGetPixelFormat screen) 0 0 60
      SDL.fillRect screen (Just (SDL.Rect 0 0 640 480)) color

mainloop :: GameFrame -> IO GameFrame
mainloop gf = do
  time <- Time.getCurrentTime
  SDL.delay 1
  
  screen <- gf ^. screen
  key' <- Key.update $ gf ^. key
  
  Field.draw screen (gf ^. pic ^. charaImg) (gf ^. pic ^. shotImg) (gf ^. field)
  
  SDLF.delay (gf ^. fps)
  time' <- Time.getCurrentTime
  SDL.setCaption ("Chimera "++(show $ (getFPS $ Time.diffUTCTime time' time))++":" ++ (show $ length (gf ^. field ^. Field.bulletE))) "chimera"
  
  return $
    key .~ key' $
    field %~ Field.update key' $
    gf
  
  where
    getFPS :: (RealFrac a, Fractional a) => a -> Int
    getFPS diff = floor $ (1 / diff)

end :: IO ()
end = SDL.quit

main :: IO ()
main = do
  start
  pic' <- load
  fps' <- SDLF.new
  SDLF.init fps'
  SDLF.set fps' 60
  
  run (return $
       pic .~ pic' $
       fps .~ fps' $
       initGameFrame)
  end
  
  where
    load :: IO Pic
    load = do
      p <- initPic
      let [r1,r2,r3] = p ^. raw
      
      return $
        charaImg .~ (r1, r3) $
        shotImg .~ (r2, r2) $
        p
