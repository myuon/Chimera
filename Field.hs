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

import Data.List
import Global
import Object
import qualified Key
import qualified Player
import Debug.Trace

updateBullet :: State Bullet ()
updateBullet = do
  (x,y) <- use pos
  r <- use speed
  t <- use angle
  pos .= (x,y) $+ fromPolar (r,t)

drawBullet :: SDL.Surface -> [SDL.Surface] -> Bullet -> IO ()
drawBullet screen [imgB1] s = do
  let (px,py) = toInt $ s ^. pos
  let (x,y) = (px,py) $+ (-5,-5)
  
  SDL.blitSurface
    (imgB1) (Just $ SDL.Rect 85 40 10 10)
    screen (Just $ SDL.Rect x y 10 10)
  return ()

updateEnemy :: Player -> State Enemy ()
updateEnemy p = do
  enemy <- get
  shotQ .= addShot enemy p

  counter %= (+1)

addShot :: Enemy -> Player -> [Bullet]
addShot e p = case (e ^. kind) of 
  Spiral -> bool id (spiral:) (e ^. counter `mod` 1 == 0) $ []
  Oneway -> bool id (toPlayer:) (e ^. counter `mod` 50 == 0) $ []
  where
    posE = e ^. pos
    posP = p ^. pos
    
    spiral :: Bullet
    spiral = initBullet posE 0.15 $ (fromIntegral $ e^. (chara . counter)) / 10

    toPlayer :: Bullet
    toPlayer = 
      initBullet posE 2 (atan2 (posE ^. _2 - posP ^. _2)
                               (posP ^. _1 - posE ^. _1))

drawEnemy :: SDL.Surface -> SDL.Surface -> Enemy -> IO ()
drawEnemy screen img e = do
  let (px,py) = toInt $ e ^. pos
  let (x,y) = center $ SDL.Rect px py 32 32
  
  SDL.blitSurface
    img (Just $ SDL.Rect 0 0 32 32)
    screen (Just $ SDL.Rect x y 32 32)
  return ()

clearQ :: State Enemy ()
clearQ = shotQ .= []

data Field = Field {
  _player :: Player,
  _enemy :: [Enemy],
  _bulletP :: [Bullet],
  _bulletE :: [Bullet]
  }

makeLenses ''Field

initField :: Field
initField = Field {
  _player = initPlayer,
  _enemy = [initEnemy (320, 50) 1 20 Oneway,
            initEnemy (120, 90) 1 20 Spiral],
  _bulletP = [],
  _bulletE = []
  }

update :: Key.Keys -> Field -> Field
update key = execState $ do
  addPlayerBullet key
  addEnemyBullet
  updateField key
  collideP
  collideE

collideP :: State Field ()
collideP = do
  es <- use enemy
  bs <- use bulletP
  
  let pair = map (\e -> collideChara e bs) es
  let bullet' = map snd pair
  enemy .= map fst pair
  bulletP %= bool (const . foldl1 intersect $ bullet') id (bullet' == [])
  
collideE :: State Field ()
collideE = do
  p <- use player
  bs <- use bulletE
  
  let pair = collideChara p bs
  player .= fst pair
  bulletE .= snd pair

collideChara :: (HasObject c, HasChara c) => c -> [Bullet] -> (c, [Bullet])
collideChara c bullet = (,)
  (hp %~ (subtract (length bullet')) $ c)
  (filter (\b -> not $ b `elem` bullet') bullet)
  where
    bullet' :: [Bullet]
    bullet' = inDist (c ^. pos) bullet

    inDist :: Pos -> [Bullet] -> [Bullet]
    inDist p = filter ((< 10.0^2) . dist p . (\b -> b ^. pos))
      where
        dist :: Pos -> Pos -> Double
        dist a = abss . apply (-) a

updateField :: Key.Keys -> State Field ()
updateField key = do
  p <- use player
  bulletE %= filter (\b -> isInside $ b ^. pos) . map (execState updateBullet)
  bulletP %= filter (\b -> isInside $ b ^. pos) . map (execState updateBullet)
  enemy %= filter (\e -> e ^. hp > 0) . map (execState $ updateEnemy p)
  player %= Player.update key

addEnemyBullet :: State Field ()
addEnemyBullet = do
  es <- use enemy
  bulletE %= (++) (concatMap (\e -> e ^. shotQ) es)
  enemy .= map (execState clearQ) es

addPlayerBullet :: Key.Keys -> State Field ()
addPlayerBullet key = do
  p <- use player
  if key ^. Key.z > 0 && p ^. counter `mod` 10 == 0
    then do
      b <- use bulletP
      bulletP .= lineBullet (p ^. pos) : b
    else return ()
    
  where
    lineBullet :: Pos -> Bullet
    lineBullet p = initBullet p 5 (pi/2)

isInside :: Pos -> Bool
isInside = uncurry (&&) .
           ((\p -> p >= 0 && p <= 640) *** (\p -> p >= 0 && p <= 480))

draw :: SDL.Surface -> (SDL.Surface, SDL.Surface) -> ([SDL.Surface], [SDL.Surface]) -> Field -> IO ()
draw screen (imgP, imgE) (imgBP, imgBE) b = do
  mapM_ (drawBullet screen imgBP) (b ^. bulletP)
  mapM_ (drawBullet screen imgBE) (b ^. bulletE)
  mapM_ (drawEnemy screen imgE) (b ^. enemy)
  Player.draw screen imgP (b ^. player)
