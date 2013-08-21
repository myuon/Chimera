{-# LANGUAGE TemplateHaskell, ImplicitParams #-}
module Bullet where

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as SDLI
import Control.Lens
import Control.Monad
import Control.Monad.State

import Global
import qualified Key as Key

data Shot = Shot {
  _pos :: Pos,
  _sp :: Int,
  _angle :: Double
  }

makeLenses ''Shot

initShot :: Shot
initShot = Shot (200,240) 0 0.0

data Bullet = Bullet {
  _bullet :: [Shot]
  }

makeLenses ''Bullet

initBullet :: Bullet
initBullet = Bullet {
  _bullet = []
  }

update :: Key.Keys -> Bullet -> Bullet
update key = execState $ do
  addShot key
  updateBullet
  
updateBullet :: State Bullet ()
updateBullet = do
  b <- use bullet
  bullet .= map (execState updateShot) b

addShot :: Key.Keys -> State Bullet ()
addShot key =
  if key ^. Key.z > 0 then
    do
      b <- use bullet
      bullet .= initShot : b
  else
    do
      b <- use bullet
      bullet .= b

updateShot :: State Shot ()
updateShot = do
  (x,y) <- use pos
  pos .= (x,y) $+ (1,0)
  
draw :: SDL.Surface -> SDL.Surface -> Bullet -> IO ()
draw screen img b = mapM_ (drawShot screen img) (b ^. bullet)

drawShot :: SDL.Surface -> SDL.Surface -> Shot -> IO ()
drawShot screen img s = do
  let (px,py) = s ^. pos
  let (x,y) = center $ SDL.Rect px py 16 72
  
  SDL.blitSurface
    img (Just $ SDL.Rect 0 0 16 72)
    screen (Just $ SDL.Rect x y 16 72)
  return ()
