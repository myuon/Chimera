{-# LANGUAGE TemplateHaskell, ImplicitParams #-}
module Field where

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as SDLI
import Control.Lens
import Control.Monad
import Control.Monad.State

import Global
import qualified Key as Key
import Debug.Trace

data Bullet = Bullet {
  _pos :: Pos,
  _sp :: Int,
  _angle :: Double
  }

makeLenses ''Bullet

initBullet :: Bullet
initBullet = Bullet (200,240) 0 0.0

data Field = Field {
  _bullet :: [Bullet]
  }

makeLenses ''Field

initField :: Field
initField = Field {
  _bullet = []
  }

update :: Key.Keys -> Field -> Field
update key = execState $ do
  addBullet key
  updateField
  
updateField :: State Field ()
updateField = do
  b <- use bullet
  bullet .= map (execState updateBullet) b

addBullet :: Key.Keys -> State Field ()
addBullet key =
  if key ^. Key.z > 0 then
    do
      b <- use bullet
      bullet .= initBullet : b
  else
    do
      b <- use bullet
      bullet .= b

updateBullet :: State Bullet ()
updateBullet = do
  (x,y) <- use pos
  pos .= (x,y) $+ (1,0)
  
draw :: SDL.Surface -> SDL.Surface -> Field -> IO ()
draw screen img b = mapM_ (drawBullet screen img) (b ^. bullet)

drawBullet :: SDL.Surface -> SDL.Surface -> Bullet -> IO ()
drawBullet screen img s = do
  let (px,py) = s ^. pos
  let (x,y) = center $ SDL.Rect px py 16 72
  
  SDL.blitSurface
    img (Just $ SDL.Rect 0 0 16 72)
    screen (Just $ SDL.Rect x y 16 72)
  return ()
