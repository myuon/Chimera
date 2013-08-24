{-# LANGUAGE TemplateHaskell, ImplicitParams,
FlexibleInstances, UndecidableInstances #-}
module Field where

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as SDLI
import Control.Bool
import Control.Lens
import Control.Arrow
import Control.Monad
import Control.Monad.State

import Global
import qualified Key
import qualified Player
import Debug.Trace

data Bullet = Bullet {
  _pos :: Point Double,
  _speed :: Double,
  _angle :: Double
  }

makeLenses ''Bullet

initBullet :: Pos -> Bullet
initBullet p = Bullet (toNum p) 5 (pi/2)

updateBullet :: State Bullet ()
updateBullet = do
  (x,y) <- use pos
  r <- use speed
  t <- use angle
  pos .= (x,y) $+ fromPolar (r,t)

drawBullet :: SDL.Surface -> [SDL.Surface] -> Bullet -> IO ()
drawBullet screen [imgB1] s = do
  let (px,py) = toPos $ s ^. pos
  let (x,y) = center $ SDL.Rect px py 10 10
  
  SDL.blitSurface
    (imgB1) (Just $ SDL.Rect 85 40 10 10)
    screen (Just $ SDL.Rect x y 10 10)
  return ()

data EnemyKind = Oneway

data Enemy = Enemy {
  _chara :: Player.Chara,
  _kind :: EnemyKind,
  _shotQ :: [Bullet]
  }

makeLenses ''Enemy

initEnemy :: Enemy
initEnemy = Enemy {
  _chara = Player.Chara (320, 50) 1 0,
  _kind = Oneway,
  _shotQ = []
  }

updateEnemy :: Player.Player -> State Enemy ()
updateEnemy p = do
  enemy <- get
  shotQ .= addShot enemy p

  (chara . Player.counter) %= (+1)

addShot :: Enemy -> Player.Player -> [Bullet]
addShot e p = bool id (spiral:) (e^.(chara . Player.counter) `mod` 1 == 0) $ []
  where
    posE = e ^. (chara . Player.pos)
    posP = p ^. (Player.chara . Player.pos)
  
    toPlayer :: Bullet
    toPlayer = Bullet (toNum posE) 2 (atan2 (fromIntegral $ posE^._2 - posP^._2)
                                            (fromIntegral $ posP^._1 - posE^._1))

    spiral :: Bullet
    spiral = Bullet (toNum posE) 0.15 $ (fromIntegral $ e^.(chara . Player.counter)) / 10

drawEnemy :: SDL.Surface -> SDL.Surface -> Enemy -> IO ()
drawEnemy screen img e = do
  let (px,py) = e ^. (chara . Player.pos)
  let (x,y) = center $ SDL.Rect px py 32 32
  
  SDL.blitSurface
    img (Just $ SDL.Rect 0 0 32 32)
    screen (Just $ SDL.Rect x y 32 32)
  return ()

clearQ :: State Enemy ()
clearQ = shotQ .= []

data Field = Field {
  _enemy :: [Enemy],
  _bulletP :: [Bullet],
  _bulletE :: [Bullet]
  }

makeLenses ''Field

initField :: Field
initField = Field {
  _bulletP = [],
  _bulletE = [],
  _enemy = [initEnemy]
  }

update :: Key.Keys -> Player.Player -> Field -> Field
update key p = execState $ do
  addPlayerBullet key p
  addEnemyBullet
  updateField p

updateField :: Player.Player -> State Field ()
updateField p = do
  bulletE %= filter (\b -> isInside (toPos $ b ^. pos)) . map (execState updateBullet)
  bulletP %= filter (\b -> isInside (toPos $ b ^. pos)) . map (execState updateBullet)
  enemy %= map (execState $ updateEnemy p)

addEnemyBullet :: State Field ()
addEnemyBullet = do
  es <- use enemy
  bs <- use bulletE
  bulletE %= (++) (concatMap (\e -> e ^. shotQ) es)
  enemy .= map (execState clearQ) es

addPlayerBullet :: Key.Keys -> Player.Player -> State Field ()
addPlayerBullet key p =
  if key ^. Key.z > 0 && p ^. (Player.chara . Player.counter) `mod` 10 == 0
    then do
      b <- use bulletP
      bulletP .= initBullet (p ^. (Player.chara . Player.pos)) : b
    else return ()

isInside :: Pos -> Bool
isInside = uncurry (&&) .
           ((\p -> p >= 0 && p <= 640) *** (\p -> p >= 0 && p <= 480))

draw :: SDL.Surface -> SDL.Surface -> ([SDL.Surface], [SDL.Surface]) -> Field -> IO ()
draw screen imgE (imgBP, imgBE) b = do
  mapM_ (drawBullet screen imgBP) (b ^. bulletP)
  mapM_ (drawBullet screen imgBE) (b ^. bulletE)
  mapM_ (drawEnemy screen imgE) (b ^. enemy)

