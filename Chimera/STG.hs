{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts, Rank2Types #-}
module Chimera.STG (
  update, draw, loadStage

  , module M
  ) where

import Graphics.UI.FreeGame
import Control.Lens
import Control.Arrow ((***))
import Control.Monad.State (get, put, execStateT, runStateT, State, StateT)

import Chimera.STG.Types as M
import Chimera.STG.World as M
import Chimera.STG.Util
import Chimera.Load
import Chimera.STG.UI as M
import Chimera.Barrage

stage1 :: Stage ()
stage1 = do
  res <- getResource
  appear 100 $ initEnemy (V2 320 200) 20 (DIndex 0) (snd $ res ^. charaImg)

loadStage :: StateT Field Game ()
loadStage = do
  stage .= stage1
  return ()

-- access to methods in superclass
(./) :: s -> StateT s Game () -> StateT c Game s
s ./ m = bracket $ m `execStateT` s

(.#) :: s -> StateT s Game a -> StateT s Game a
s .# m = do
  (s', f') <- bracket $ m `runStateT` s
  put f'
  return s'

-- update the value
a <.- s = s >>= (\x -> a .= x)

class GUIClass c where
  update :: StateT c Game ()
  draw :: StateT c Game ()

instance GUIClass Player where
  update = do
    s <- use speed
    k <- use keys
    counter %= (+1)
    pos %= clamp . (+ (s $* dir k))
    
    where
      dir :: Keys -> Vec
      dir key = let addTup b p q = bool q (fromPair p+q) b in
        addTup (key ^. up    > 0) (0,-1) $
        addTup (key ^. down  > 0) (0,1) $
        addTup (key ^. right > 0) (1,0) $
        addTup (key ^. left  > 0) (-1,0) $
        fromPair (0,0)

  draw = do
    p <- get
    translate (p ^. pos) $ fromBitmap (p ^. img)

clamp :: Vec -> Vec
clamp = fromPair . (edgeX *** edgeY) . toPair
  where
    edgeX = (\p -> bool p areaLeft (p < areaLeft)) .
            (\p -> bool p areaRight (p > areaRight))
    
    edgeY = (\p -> bool p areaLeft (p < areaLeft)) .
            (\p -> bool p areaBottom (p > areaBottom))

instance GUIClass Enemy where
  update = do
    sp <- use spXY
    pos %= clamp . (+sp)
    spXY .= 0
    counter %= (+1)
    
  draw = do
    p <- get
    translate (p ^. pos) $ fromBitmap (p ^. img)

instance GUIClass Bullet where
  update = do
    r <- use speed
    t <- use angle
    pos %= (+ fromPolar (r,t))
    counter %= (+1)

  draw = do
    b <- get
    translate (b ^. pos) $
      rotateR (b ^. angle + pi/2) $
      fromBitmap (b ^. img)

instance GUIClass Field where
  update = do
    s' <- do
      s <- use stage
      f <- get
      f .# (runStage s)
    stage .= s'
    
    p <- use player
    es <- use enemy
    player <.- (p ./ update)
    
    bsP <- use bulletP
    bsE <- use bulletE
    bulletP <.- (mapM (\b -> b ./ update) . filter (\b -> isInside $ b ^. pos) $ bsP)
    bulletE <.- (mapM (\b -> b ./ update) . filter (\b -> isInside $ b ^. pos) $ bsE)
    
    addBulletP

    f <- get
--    enemy <.- mapM (\e -> e ./ update) es
    enemy <.- (mapM updateLookAt $ zipWith LookAt es $ repeat f)

  draw = do
    res <- use resource
    p <- use player
    es <- use enemy
    bsP <- use bulletP
    bsE <- use bulletE
    
    mapM_ (\b -> b ./ draw) bsE
    mapM_ (\b -> b ./ draw) bsP
    mapM_ (\e -> e ./ draw) es
    p ./ draw

    bracket $ translate (V2 320 240) $ fromBitmap (res ^. board)

updateLookAt :: AtEnemy -> StateT Field Game Enemy
updateLookAt a = do
  LookAt e f <- a ./ update'
  put f
  return e
  
  where
    update' :: StateT AtEnemy Game ()
    update' = get >>= \f -> f ./ runDanmaku zako >>= put

addBulletP :: StateT Field Game ()
addBulletP = do
  p <- use player

  when (p ^. keys ^. zKey > 0 && p ^. counter `mod` 10 == 0) $ do
    res <- use resource
    bulletP %= (:) (lineBullet (p ^. pos) (fst $ res ^. bulletImg))

lineBullet :: Vec -> Bitmap -> Bullet
lineBullet p r = initBullet p 5 (pi/2) (bulletBitmap Diamond Red r)
