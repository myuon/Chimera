{-# LANGUAGE TemplateHaskell, FlexibleContexts, ImplicitParams #-}
module Main where

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as SDLI
import Control.Lens
import Control.Monad
import Control.Monad.State
import Control.Applicative
import Control.Bool

import Data.Word (Word32)
import Global
import qualified Player
import qualified Key
import qualified Field
import Debug.Trace

data GameFrame = GameFrame {
  _screenMode :: Int,
  _player :: Player.Player,
  _screen :: IO SDL.Surface,
  _key :: Key.Keys,
  _field :: Field.Field,
  _pic :: Pic,
  _fps :: Word32
  }

makeLenses ''GameFrame

initGameFrame :: GameFrame
initGameFrame = GameFrame {
  _screenMode = 0,
  _player = Player.initPlayer,
  _screen = SDL.getVideoSurface,
  _key = Key.initKeys,
  _field = Field.initField,
  _pic = undefined,
  _fps = 0
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
--  SDL.delay 1
  
  fps' <- SDL.getTicks
--  delayFPS 60 (fps' - (gf ^. fps))
  
  screen <- gf ^. screen
  key' <- Key.update $ gf ^. key
  
  Player.draw screen (fst $ gf ^. pic ^. charaImg) (gf ^. player)
  Field.draw screen (snd $ gf ^. pic ^. charaImg) (gf ^. pic ^. shotImg) (gf ^. field)
  
  SDL.setCaption ("Chimera "++(show $ getFPS fps' (gf ^. fps))++":" ++ (show $ length (gf ^. field ^. Field.bulletE))) "chimera"
--  SDL.setCaption ("Chimera:"++(show $ length (gf ^. field ^. Field.bulletE))) "chimera"

  return $
    fps .~ fps' $
    key .~ key' $
    field %~ Field.update key' (gf ^. player) $
    player %~ Player.update key' $ 
    gf
  
  where
    getFPS :: Word32 -> Word32 -> Int
    getFPS f f' = floor $ (1000 / fromIntegral (f - f'))
    
    delayFPS :: Int -> Word32 -> IO ()
    delayFPS fps f = do
      let s = floor $ 1000 / fromIntegral fps
      SDL.delay $ bool 0 (s - f) (s > f)
    
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
      let [r1,r2,r3] = p ^. raw
      
--      c1 <- makeImgSurface (50,50) r1 (SDL.Rect 0 0 50 50)
--      c2 <- makeImgSurface (32,32) r3 (SDL.Rect 0 0 32 32)
--      s1 <- makeImgSurface (20,20) r2 (SDL.Rect 0 0 20 20)
--      s2 <- makeImgSurface (20,20) r2 (SDL.Rect 0 100 20 20)
      
      return $
        charaImg .~ (r1, r3) $
        shotImg .~ ([r2], [r2]) $
        p
        
    makeImgSurface :: (Int, Int) -> SDL.Surface -> SDL.Rect -> IO SDL.Surface
    makeImgSurface (w,h) img r = do
      s <- SDL.createRGBSurfaceEndian [SDL.SrcAlpha] w h 32
      SDL.blitSurface
        img (Just r)
        s (Just $ SDL.Rect 0 0 w h)
      s' <- SDL.displayFormatAlpha s
      return $ s'
