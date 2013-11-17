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
  appear 30 $ initEnemy (V2 320 (-40)) 2 (snd $ res ^. charaImg) (Zako 10)
  appear 50 $ initEnemy (V2 300 (-40)) 2 (snd $ res ^. charaImg) (Zako 10)
  appear 70 $ initEnemy (V2 280 (-40)) 2 (snd $ res ^. charaImg) (Zako 10)
  appear 90 $ initEnemy (V2 260 (-40)) 2 (snd $ res ^. charaImg) (Zako 10)
  appear 110 $ initEnemy (V2 240 (-40)) 2 (snd $ res ^. charaImg) (Zako 10)

  appear 150 $ initEnemy (V2 220 (-40)) 2 (snd $ res ^. charaImg) (Zako 11)
  appear 170 $ initEnemy (V2 200 (-40)) 2 (snd $ res ^. charaImg) (Zako 11)
  appear 190 $ initEnemy (V2 180 (-40)) 2 (snd $ res ^. charaImg) (Zako 11)
  appear 210 $ initEnemy (V2 160 (-40)) 2 (snd $ res ^. charaImg) (Zako 11)
  appear 230 $ initEnemy (V2 140 (-40)) 2 (snd $ res ^. charaImg) (Zako 11)

  appear 400 $ initEnemy (V2 300 (-40)) 2 (snd $ res ^. charaImg) (Zako 20)
  appear 420 $ initEnemy (V2 100 (-40)) 2 (snd $ res ^. charaImg) (Zako 21)
  appear 440 $ initEnemy (V2 280 (-40)) 2 (snd $ res ^. charaImg) (Zako 20)
  appear 460 $ initEnemy (V2 120 (-40)) 2 (snd $ res ^. charaImg) (Zako 21)
  appear 480 $ initEnemy (V2 260 (-40)) 2 (snd $ res ^. charaImg) (Zako 20)
  appear 500 $ initEnemy (V2 140 (-40)) 2 (snd $ res ^. charaImg) (Zako 21)

  appear 570 $ initEnemy (V2 260 (-40)) 10 (snd $ res ^. charaImg) (Zako 30)

  appear 870 $ initEnemy (V2 260 (-40)) 10 (snd $ res ^. charaImg) (Zako 40)

  appear 1170 $ initEnemy (V2 260 (-40)) 10 (snd $ res ^. charaImg) (Zako 50)

  appear 1500 $ initEnemy (V2 260 (-40)) 10 (snd $ res ^. charaImg) (Zako 60)

stage2 :: Stage ()
stage2 = do
  res <- getResource
  appear 30 $ initEnemy (V2 260 240) 2 (snd $ res ^. charaImg) (Debug)

stage3 :: Stage ()
stage3 = do
  res <- getResource
  appear 30 $ initEnemy (V2 260 (-40)) 10 (snd $ res ^. charaImg) (Zako 70)

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
    pos %= (+sp)
    counter %= (+1)
    h <- use hp
    when (h <= 0) $ state .= Dead
    
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
    player <.- (p ./ update)
    
    bsP <- use bulletP
    bsE <- use bulletE
    bulletP <.- (mapM (\b -> b ./ update) . filter (\b -> isInside $ b ^. pos) $ bsP)
    bulletE <.- (mapM (\b -> b ./ update) . filter (\b -> isInside $ b ^. pos) $ bsE)
    
    addBulletP
    collideE
    collideP

    f <- get
    es <- use enemy
    es' <- run es f
    enemy <.- (mapM (\e -> e ./ update) . filter (\e -> e ^. state /= Dead) $ es')
    
    where
      run :: [Enemy] -> Field -> StateT Field Game [Enemy]
      run [] _ = return $ []
      run (e:es) f = do
        (e', f') <- bracket $ updateLookAt (LookAt e f) `runStateT` f
        put f'
        fmap (e':) (run es f')

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
    update' = get >>= \f -> f ./ runDanmaku (barrage (f ^. local ^. kind)) >>= put

addBulletP :: StateT Field Game ()
addBulletP = do
  p <- use player
  when (p ^. keys ^. zKey > 0 && p ^. counter `mod` 10 == 0) $ do
    res <- use resource
    bulletP %= (:) (lineBullet (p ^. pos) (fst $ res ^. bulletImg))

  where
    lineBullet :: Vec -> Bitmap -> Bullet
    lineBullet p r = initBullet p 5 (pi/2) (bulletBitmap Diamond Red r)

collideE :: StateT Field Game ()
collideE = do
  es <- use enemy
  bs <- use bulletP
  
  let (es', bs') = run es bs
  enemy .= es'
  bulletP .= bs'
  
  where
    run :: [Enemy] -> [Bullet] -> ([Enemy], [Bullet])
    run [] bs = ([], bs)
    run (e:es) bs = let
      (e', bs') = collide e bs
      (es', bs'') = run es bs' in
      (e':es', bs'')

collideP :: StateT Field Game ()
collideP = do
  p <- use player
  bs <- use bulletE
  
  let (p', bs') = collide p bs
  player .= p'
  bulletE .= bs'
  
collide :: (HasChara c, HasObject c) => c -> [Bullet] -> (c, [Bullet])
collide c bs = (,)
  (hp %~ (\x -> x - (length bs - length bs')) $ c)
  bs'
  
  where
    bs' :: [Bullet]
    bs' = filter (\b -> not $ 15.0^2 > (absV $ (b^.pos) - (c^.pos))) bs

