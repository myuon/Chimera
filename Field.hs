{-# LANGUAGE TemplateHaskell, ImplicitParams #-}
module Field where

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as SDLI
import Control.Lens
import Control.Arrow
import Control.Monad
import Control.Monad.State

import Global
import qualified Key
import qualified Player
import Debug.Trace

data Bullet = Bullet {
  _pos :: Pos,
  _speed :: Int,
  _angle :: Float
  }

makeLenses ''Bullet

initBullet :: Pos -> Bullet
initBullet p = Bullet p 5 1.5

data Field = Field {
  _bullet :: [Bullet]
  }

makeLenses ''Field

initField :: Field
initField = Field {
  _bullet = []
  }

update :: Key.Keys -> Player.Player -> Field -> Field
update key p = execState $ do
  addBullet key p
  updateField
  
updateField :: State Field ()
updateField = do
  bullet %= filter (\b -> isInside (b ^. pos)) . map (execState updateBullet)

addBullet :: Key.Keys -> Player.Player -> State Field ()
addBullet key p =
  if key ^. Key.z > 0 && p ^. Player.counter `mod` 10 == 0
    then do
      b <- use bullet
      bullet .= initBullet (p ^. Player.pos) : b
      
    else return ()

updateBullet :: State Bullet ()
updateBullet = do
  (x,y) <- use pos
  r <- use speed
  t <- use angle
  pos .= (x,y) $+ (mapp floor $ fromPolar (fromIntegral r,t))

isInside :: Pos -> Bool
isInside = uncurry (&&) .
           ((\p -> p >= 0 && p <= 640) *** (\p -> p >= 0 && p <= 480))

draw :: SDL.Surface -> SDL.Surface -> Field -> IO ()
draw screen img b = mapM_ (drawBullet screen img) (b ^. bullet)

drawBullet :: SDL.Surface -> SDL.Surface -> Bullet -> IO ()
drawBullet screen img s = do
  let (px,py) = s ^. pos
  let (x,y) = center $ SDL.Rect px py 20 20
  
  SDL.blitSurface
    img (Just $ SDL.Rect 0 0 20 20)
    screen (Just $ SDL.Rect x y 20 20)
  return ()
