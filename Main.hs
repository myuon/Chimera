{-# LANGUAGE TemplateHaskell, FlexibleContexts, ImplicitParams #-}
module Main where

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as SDLI
import Control.Lens
import Control.Monad
import Control.Monad.State
import Control.Applicative

import Global
import qualified Player
import qualified Key
import qualified Bullet
import Debug.Trace

data GameFrame = GameFrame {
  _screenMode :: Int,
  _player :: Player.Player,
  _screen :: IO SDL.Surface,
  _key :: Key.Keys,
  _bullet :: Bullet.Bullet,
  _pic :: Pic
  }

makeLenses ''GameFrame

initGameFrame :: GameFrame
initGameFrame = GameFrame {
  _screenMode = 0,
  _player = Player.initPlayer,
  _screen = SDL.getVideoSurface,
  _key = Key.initKeys,
  _bullet = Bullet.initBullet,
  _pic = undefined
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
  
  Player.draw screen (gf ^. pic ^. playerImg) (gf ^. player)
  Bullet.draw screen (gf ^. pic ^. shotImg) (gf ^. bullet)
  
  return $
    key .~ key' $
    player %~ Player.update key' $ 
    bullet %~ Bullet.update key' $
    gf

end :: IO ()
end = SDL.quit

main :: IO ()
main = do
  start
  pic' <- load
  run (return $
       pic .~ pic' $
       initGameFrame)
  end
  
  where
    load :: IO Pic
    load = do
      p <- initPic
      let [r1,r2] = p ^. raw
      SDL.setClipRect r1 (Just $ SDL.Rect 0 0 50 50)
      SDL.setClipRect r2 (Just $ SDL.Rect 0 0 16 72)
  
      return $
        playerImg .~ r1 $
        shotImg .~ r2 $
        p
