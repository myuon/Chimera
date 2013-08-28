{-# LANGUAGE TemplateHaskell, ImplicitParams,
FlexibleInstances, UndecidableInstances #-}
module Field where

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as SDLI
import qualified Graphics.UI.SDL.Rotozoomer as SDLR
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

drawBullet :: SDL.Surface -> SDL.Surface -> Bullet -> IO ()
drawBullet screen imgB1 s = do
  let rect = bulletImgRect (s ^. kindBullet) (s ^. color)
  let (w,h) = sizeRect rect
  let (x,y) = (toInt $ s ^. pos) $+ (toInt $ (-1/2) $* toNum (w,h))

--  img <- SDLR.rotozoom imgB1 (s ^. angle) 1.0 False
  
  SDL.blitSurface
    imgB1 (Just $ rect)
    screen (Just $ SDL.Rect x y w h)
  return ()

updateEnemy :: Player -> State Enemy ()
updateEnemy p = do
  enemy <- get
  pos %= move (enemy ^. motion) ((enemy ^. speed) $* (0,1)) (enemy ^. counter)
  shotQ .= if (enemy ^. mstate == Stay) then addShot enemy p else []
  counter %= (+1)
  mstate %= checkState (enemy ^. motion) enemy
  where
    move :: Motion -> Pos -> Int -> Pos -> Pos
    move (Mono go stay) v cnt
      | cnt < go = ($+) v
      | go <= cnt && cnt < go + stay = id
      | go + stay <= cnt = \p -> p $- v

    checkState :: Motion -> Enemy -> MotionState -> MotionState
    checkState (Mono go stay) e
      | e ^. counter == go = const Stay
      | e ^. counter == go + stay = const Back
      | go + stay < e ^. counter && not (isInside (e ^. pos)) = const Dead 
      | otherwise = id

    addShot :: Enemy -> Player -> [Bullet]
    addShot e p = case (e ^. kindEnemy) of 
      Spiral -> bool id (spiral:) (e ^. counter `mod` 3 == 0) $ []
      Oneway -> bool id (toPlayer:) (e ^. counter `mod` 50 == 0) $ []
      where
        posE = e ^. pos
        posP = p ^. pos
        
        spiral :: Bullet
        spiral = let ang = (fromIntegral $ e ^. counter) / 10 in
          initBullet posE 2 ang Needle Red

        toPlayer :: Bullet
        toPlayer = let ang = atan2 (posE ^. _2 - posP ^. _2) (posP ^. _1 - posE ^. _1) in
          initBullet posE 2 ang BallLarge Green

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
  _bulletE :: [Bullet],
  _enemyQ :: [(Int, Enemy)]
  }

makeLenses ''Field

initField :: Field
initField = Field {
  _player = initPlayer,
  _enemy = [],
  _bulletP = [],
  _bulletE = [],
  _enemyQ = [
    (10, initEnemy (320, -50) 1 20 Oneway (Mono 260 300)),
    (200, initEnemy (120, -10) 1 20 Spiral (Mono 160 300))]
  }

update :: Key.Keys -> Field -> Field
update key = execState $ do
  addPlayerBullet key
  addEnemyBullet
  updateField key
  collideP
  collideE
  addEnemy

addEnemy :: State Field ()
addEnemy = do
  p <- use player
  eQ <- use enemyQ
  case maybeHead (p ^. counter) eQ of
    Just e ->
      enemy %= (e:) >>
      enemyQ %= tail
    Nothing -> return ()
  
  where
    maybeHead :: Int -> [(Int, Enemy)] -> Maybe Enemy
    maybeHead _ [] = Nothing
    maybeHead cnt ((time, e):_) = if cnt == time
      then Just $ e
      else Nothing

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
  enemy %= filter (\e -> e ^. hp > 0 && e ^. mstate /= Dead) . map (execState $ updateEnemy p)
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
    lineBullet p = initBullet p 5 (pi/2) Diamond Red

draw :: SDL.Surface -> (SDL.Surface, SDL.Surface) -> (SDL.Surface, SDL.Surface) -> Field -> IO ()
draw screen (imgP, imgE) (imgBP, imgBE) b = do
  mapM_ (drawBullet screen imgBP) (b ^. bulletP)
  mapM_ (drawBullet screen imgBE) (b ^. bulletE)
  mapM_ (drawEnemy screen imgE) (b ^. enemy)
  Player.draw screen imgP (b ^. player)
