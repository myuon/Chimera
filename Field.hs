{-# LANGUAGE TemplateHaskell, FlexibleInstances, UndecidableInstances #-}
module Field where

import qualified Graphics.UI.FreeGame as Game
import qualified Graphics.UI.FreeGame.GUI.GLFW as GL
import Control.Lens
import Control.Arrow
import Control.Monad
import Control.Monad.State

import qualified Data.Array as Array
import qualified Linear.V2 as V2
import Data.List
import Global
import Object
import qualified Key
import qualified Player
import qualified Barrage
import Debug.Trace

drawBullet :: BulletImg -> Bullet -> Game.Game ()
drawBullet imgB s = do
  let img = imgB Array.! (s ^. kindBullet) Array.! (s ^. color)
  
  Game.translate (s ^. pos) $
    Game.rotateR (s ^. angle + pi/2) $
    Game.fromBitmap img

drawEnemy :: Game.Bitmap -> Enemy -> Game.Game ()
drawEnemy img e = Game.translate (e ^. pos) $ Game.fromBitmap img

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
--    (10, initEnemy (fromPair (320, 200)) 1 20 (BDebug) (WaitMono 0)),
    (10, initEnemy (fromPair (320, 200)) 1 20 (BZako 0) (Mono 0 300)),
    (400, initEnemy (fromPair (320, -20)) 1 20 (BBoss 0) (WaitMono 100))
    ]
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
    Just e -> do
      enemy %= (e:)
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
        dist :: Pos -> Pos -> Double'
        dist p q = absV $ (p - q)

updateField :: Key.Keys -> State Field ()
updateField key = do
  p <- use player
  player %= Player.update key
  bulletE %= filter (\b -> isInside $ b ^. pos) . map (\b -> (execState $ Barrage.barrage (b ^. barrage) ^. Barrage.bullet) b)
  bulletP %= filter (\b -> isInside $ b ^. pos) . map (\b -> (execState $ Barrage.barrage (b ^. barrage) ^. Barrage.bullet) b)  
  enemy %= filter (\e -> e ^. hp > 0 && e ^. mstate /= Dead) . map (\e -> (execState $ (Barrage.barrage (e ^. kindEnemy) ^. Barrage.enemy) p) e)

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
    lineBullet p = initBullet p 5 (pi/2) Diamond Red BPlayer

draw :: (Game.Bitmap, Game.Bitmap) -> (BulletImg, BulletImg) -> Field -> Game.Game ()
draw (imgP, imgE) (imgBP, imgBE) b = do
  mapM_ (drawBullet imgBP) (b ^. bulletP)
  mapM_ (drawBullet imgBE) (b ^. bulletE)
  mapM_ (drawEnemy imgE) (b ^. enemy)
  Player.draw imgP (b ^. player)
