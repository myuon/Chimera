{-# LANGUAGE TemplateHaskell #-}
module Global where

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as SDLI
import Control.Lens
import Control.Arrow
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

abss :: (Num a) => Point a -> a
abss (a,b) = a * a + b * b

type Pos = Point Double
type PoInt = Point Int

toNum :: (Num a, Integral b) => Point b -> Point a
toNum = mapp fromIntegral

toInt :: (RealFrac a) => Point a -> Point Int
toInt = mapp truncate

fromPolar :: (Floating a) => (a,a) -> Point a
fromPolar (r,t) = r $* (cos t, -sin t)

center :: SDL.Rect -> PoInt
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
