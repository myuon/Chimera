{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}
module Main where

import qualified Graphics.UI.SDL as SDL
import Control.Lens

import Control.Monad
import Control.Monad.State
import Control.Applicative
import Control.Monad.IO.Class
import Data.IORef

import qualified Player

data GameFrame = GameFrame {
  _screenMode :: Int,
  _player :: Player.Player
  } deriving (Eq, Show)

makeLenses ''GameFrame

initGameFrame :: GameFrame
initGameFrame = GameFrame { 
  _screenMode = 0,
  _player = Player.initPlayer
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
  step >>= flip unless (run gf')
  
step :: IO Bool
step = SDL.pollEvent >>= (\ev -> return $ isQuit ev)
  where
    isQuit :: SDL.Event -> Bool
    isQuit (SDL.KeyDown keysym) = SDL.symKey keysym == SDL.SDLK_ESCAPE
    isQuit SDL.Quit = True
    isQuit _ = False

mainloop :: GameFrame -> IO GameFrame
mainloop gf = do
  Player.draw $ gf ^. player
  return $ player %~ Player.update $ gf

end :: IO ()
end = SDL.quit

main = start >> run (return initGameFrame) >> end
