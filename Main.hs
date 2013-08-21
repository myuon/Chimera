{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}
module Main where

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as SDLI
import Control.Lens
import Control.Monad
import Control.Monad.State
import Control.Applicative

import qualified Player
import qualified Key

data GameFrame = GameFrame {
  _screenMode :: Int,
  _player :: Player.Player,
  _screen :: IO SDL.Surface,
  _key :: Key.Keys
  }

makeLenses ''GameFrame

initGameFrame :: GameFrame
initGameFrame = GameFrame {
  _screenMode = 0,
  _player = Player.initPlayer,
  _screen = SDL.getVideoSurface,
  _key = Key.initKeys
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
  SDL.delay 1
  
  screen <- gf ^. screen
  key' <- Key.update $ gf ^. key
  
  Player.draw screen (gf ^. player)
  
  return $ 
    key .~ key' $
    player %~ Player.update key' $ 
    gf

end :: IO ()
end = SDL.quit

main = start >> run (return initGameFrame) >> end
