{-# LANGUAGE TemplateHaskell #-}
module Global where

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as SDLI
import Control.Lens
import Control.Monad.State

type Point a = (a,a)

apply :: (a -> a -> a) -> Point a -> Point a -> Point a
apply f (a,b) (c,d) = (f a c, f b d)

mapp :: (a -> b) -> Point a -> Point b
mapp f (a,b) = (a,b) & both %~ f

($+) :: (Num a) => Point a -> Point a -> Point a
($+) = apply (+)

($*) :: (Num a) => a -> Point a -> Point a
($*) k = mapp (*k)

type Pos = Point Int

toNum :: (Num a) => Pos -> Point a
toNum = mapp fromIntegral

toPos :: (RealFrac a) => Point a -> Pos
toPos = mapp truncate

fromPolar :: (Floating a) => (a,a) -> Point a
fromPolar (r,t) = r $* (cos t, -sin t)

center :: SDL.Rect -> Pos
center r = (SDL.rectX r, SDL.rectY r) $+ 
           (mapp floor $ (-1/2) $* toNum (SDL.rectW r, SDL.rectH r))

data Pic = Pic {
  _raw :: [SDL.Surface],
  _charaImg :: (SDL.Surface, SDL.Surface),
  _shotImg :: ([SDL.Surface], [SDL.Surface])
  }

makeLenses ''Pic

initPic :: IO Pic
initPic = do 
  r1 <- SDL.displayFormatAlpha =<< SDLI.load "data/img/player_reimu.png"
  r2 <- SDL.displayFormatAlpha =<< SDLI.load "data/img/_shot3.png"
  r3 <- SDL.displayFormatAlpha =<< SDLI.load "data/img/dot_yousei.png"
  
  return $ Pic {
    _raw = [r1, r2, r3],
    _charaImg = undefined,
    _shotImg = undefined
  }
