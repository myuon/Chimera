{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}
module Main where

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as SDLI
import Control.Lens
import Control.Monad
import Control.Monad.State
import Control.Applicative

import qualified Player

data GameFrame = GameFrame {
  _screenMode :: Int,
  _player :: Player.Player,
  _screen :: IO SDL.Surface
  }

makeLenses ''GameFrame

initGameFrame :: GameFrame
initGameFrame = GameFrame {
  _screenMode = 0,
  _player = Player.initPlayer,
  _screen = SDL.getVideoSurface
  }

start :: IO ()
start = do
  SDL.init [SDL.InitEverything]
  SDL.setVideoMode 640 480 32 []
  SDL.setCaption "Chimera" "chimera"

run :: IO GameFrame -> IO ()
run gfIO = do
  gf <- gfIO
  let gf' = mainloop gf
  (gf ^. screen) >>= step >>= flip unless (run gf')

step :: SDL.Surface -> IO Bool
step screen = fmap and . sequence $
  [SDL.tryFlip screen,
  SDL.pollEvent >>= (\ev -> return $ isQuit ev)]
  
  where
    isQuit :: SDL.Event -> Bool
    isQuit (SDL.KeyDown keysym) = SDL.symKey keysym == SDL.SDLK_ESCAPE
    isQuit SDL.Quit = True
    isQuit _ = False

mainloop :: GameFrame -> IO GameFrame
mainloop gf = do
  screen <- gf ^. screen
  Player.draw screen (gf ^. player)
  return $ player %~ Player.update $ gf

end :: IO ()
end = SDL.quit

main = start >> run (return initGameFrame) >> end
