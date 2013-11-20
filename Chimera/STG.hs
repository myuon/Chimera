{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts, Rank2Types #-}
module Chimera.STG (
  update, draw, loadStage

  , module M
  ) where

import Graphics.UI.FreeGame
import Control.Lens
import Control.Arrow ((***))
import Control.Monad.State.Strict (get, put, execStateT, evalStateT, runStateT, StateT)
import qualified Data.Vector as V
import Control.Monad.Trans.Class (lift)

import Chimera.STG.Types as M
import Chimera.STG.World as M
import Chimera.STG.Util
import Chimera.Load
import Chimera.STG.UI as M
import Chimera.Barrage

import Data.Time

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

  appear 2000 $ initEnemy (V2 260 (-40)) 10 (snd $ res ^. charaImg) (Boss 0)

  appear 2500 $ initEnemy (V2 260 (-40)) 10 (snd $ res ^. charaImg) (Boss 1)

stage2 :: Stage ()
stage2 = do
  res <- getResource
  appear 30 $ initEnemy (V2 260 240) 2 (snd $ res ^. charaImg) (Debug)

stage3 :: Stage ()
stage3 = do
  res <- getResource
  appear 30 $ initEnemy (V2 260 (-40)) 10 (snd $ res ^. charaImg) (Zako 60)

loadStage :: StateT Field Game ()
loadStage = do
  stage .= stage1
  return ()

-- access to methods in superclass
(./) :: s -> StateT s Game () -> StateT c Game s
s ./ m = lift $ m `execStateT` s

class GUIClass c where
  update :: c -> Game c
  draw :: c -> Game ()

instance GUIClass Player where
  update p = do
    let s = p ^. speed
    let k = p ^. keys
    return $
      counter %~ (+1) $
      pos %~ clamp . (+ (s $* dir k)) $
      p
    
    where
      dir :: Keys -> Vec
      dir key = let addTup b p q = bool q (fromPair p+q) b in
        addTup (key ^. up    > 0) (0,-1) $
        addTup (key ^. down  > 0) (0,1) $
        addTup (key ^. right > 0) (1,0) $
        addTup (key ^. left  > 0) (-1,0) $
        fromPair (0,0)

  draw p = do
    translate (p ^. pos) $ fromBitmap (p ^. img)

clamp :: Vec -> Vec
clamp = fromPair . (edgeX *** edgeY) . toPair
  where
    edgeX = (\p -> bool p areaLeft (p < areaLeft)) .
            (\p -> bool p areaRight (p > areaRight))
    
    edgeY = (\p -> bool p areaLeft (p < areaLeft)) .
            (\p -> bool p areaBottom (p > areaBottom))

instance GUIClass Enemy where
  update e = do
    let s = if (e^.hp <= 0) then const Dead else id
    
    return $
      pos %~ (+ e^.spXY) $
      counter %~ (+1) $
      state %~ s $
      e
  
  draw e = do
    translate (e ^. pos) $ fromBitmap (e ^. img)

instance GUIClass Bullet where
  update b = runBullet (b^.kindBullet) `execStateT` b

  draw b = do
    translate (b ^. pos) $ rotateR (b ^. angle + pi/2) $
      fromBitmap (b ^. img)

instance GUIClass Field where
  update f = do
    (s', LookAt c' _ me) <- runStage (f^.stage) `runStateT` (LookAt (f^.counterF) f Nothing)

    let es = getEnemy me $ f^.enemy
    pairs <- mapM (\e -> updateLookAt e f `evalStateT` f) es
    es' <- (mapM update . filter (\e -> e ^. state /= Dead) $ map fst pairs)
    let bsE = (V.++ (f^.bulletE)) (V.concat . map V.concat $ map snd pairs)

    p' <- update (f^.player)
    b <- addBulletP (f^.resource) `evalStateT` p'
    let bsP = b V.++ (f^.bulletP)

    bsP' <- (V.mapM update . V.filter (\b -> isInside $ b ^. pos) $ bsP)
    bsE' <- (V.mapM update . V.filter (\b -> isInside $ b ^. pos) $ bsE)

    (es'', bsP'') <- collideE (es', bsP')
    (p'', bsE'') <- collideP (p', bsE')

    return $
      counterF .~ c' $
      stage .~ s' $
      player .~ p' $
      enemy .~ es'' $
      bulletP .~ bsP'' $
      bulletE .~ bsE'' $
      f
      
    where
      getEnemy :: Maybe Enemy -> [Enemy] -> [Enemy]
      getEnemy (Just e) = (e:)
      getEnemy (Nothing) = id
      
  draw f = do
    V.mapM_ (\b -> draw b) (f ^. bulletP)
    draw (f ^. player)
    V.mapM_ (\b -> draw b) (f ^. bulletE)
    mapM_ (\e -> draw e) (f ^. enemy)
    
    translate (V2 320 240) $ fromBitmap (f ^. resource ^. board)

updateLookAt :: Enemy -> Field -> StateT Field Game (Enemy, [V.Vector Bullet])
updateLookAt e f = do
  LookAt e' _ r' <- lift $ runDanmaku (barrage (e^.kind)) `execStateT` (LookAt e f [])
  return (e', r')

addBulletP :: Resource -> StateT Player Game (V.Vector Bullet)
addBulletP res = do
  p <- get
  if (p ^. keys ^. zKey > 0 && p ^. counter `mod` 10 == 0) then
    return $ V.singleton $ lineBullet (p ^. pos) (fst $ res ^. bulletImg)
  else
    return $ V.empty

  where
    lineBullet :: Vec -> Bitmap -> Bullet
    lineBullet p r = initBullet p 5 (pi/2) (bulletBitmap Diamond Red r) (KindBullet 0) 0

collideE :: ([Enemy], V.Vector Bullet) -> Game ([Enemy], V.Vector Bullet)
collideE (es, bs) = do
  return $ run es bs
  
  where
    run :: [Enemy] -> V.Vector Bullet -> ([Enemy], V.Vector Bullet)
    run [] bs = ([], bs)
    run (e:es) bs = let
        (e', bs') = collide e bs
        (es', bs'') = run es bs' in
        (e':es', bs'')

collideP :: (Player, V.Vector Bullet) -> Game (Player, V.Vector Bullet)
collideP (p,bs) = do
  let (p', bs') = collide p bs
  return $ (p', bs')
  
collide :: (HasChara c, HasObject c) => c -> V.Vector Bullet -> (c, V.Vector Bullet)
collide c bs = (,)
  (hp %~ (\x -> x - (V.length bs - V.length bs')) $! c)
  bs'
  
  where
    bs' :: V.Vector Bullet
    bs' = V.filter (\b -> not $ 15.0^2 > (absV $ (b^.pos) - (c^.pos))) bs

