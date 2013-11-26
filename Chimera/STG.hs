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

import Chimera.Scripts.Stage1

stage2 :: Stage ()
stage2 = do
  res <- getResource
  keeper $ initEnemy (V2 260 (-40)) 2 res (Zako 60)

loadStage :: Field -> Field
loadStage f =
  loadField $
  resource .~ load1 $
  stage .~ stage1 $
  f

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
      pos %~ clamp . (+ ((bool id (0.5*) (k^.shift > 0) $ s) *^ dir k)) $
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
    (s', LookAt () _ me) <- runStage (f^.stage) `runStateT` (LookAt () f Nothing)

    bsP' <- (V.mapM update . V.filter (\b -> isInside $ b ^. pos) $ (f^.bulletP))
    bsE' <- (V.mapM update . V.filter (\b -> isInside $ b ^. pos) $ (f^.bulletE))

    p' <- update (f^.player)
    let b = addBulletP (f^.resource) p'

    pairs <- mapM (\e -> updateLookAt e f `evalStateT` f) (f^.enemy)
    es' <- (mapM update . filter (\e -> e ^. state /= Dead) $ map fst pairs)

    (es'', bsP'') <- collideE (es', bsP')
    (p'', bsE'') <- collideP (p', (V.concat . map V.concat $ map snd pairs) V.++ bsE')

    return $
      counterF %~ (+1) $
      stage .~ s' $
      player .~ p' $
      enemy .~ maybe id (:) me es'' $
      bulletP .~ b V.++ bsP'' $
      bulletE .~ bsE'' $
      f
      
  draw f = do
    V.mapM_ (\b -> draw b) (f ^. bulletP)
    draw (f ^. player)
    V.mapM_ (\b -> draw b) (f ^. bulletE)
    mapM_ (\e -> draw e) (f ^. enemy)

    when (f^.isDebug) $ do
      V.mapM_ (\b -> colored blue . polygon $ boxVertex (b^.pos) (b^.size)) (f ^. bulletP)
      (\p -> colored yellow . polygon $ boxVertex (p^.pos) (p^.size)) $ f^.player
      V.mapM_ (\b -> colored red . polygon $ boxVertex (b^.pos) (b^.size)) (f ^. bulletE)
      mapM_ (\e -> colored green . polygon $ boxVertex (e^.pos) (e^.size)) (f ^. enemy)
    
    translate (V2 320 240) $ fromBitmap (f ^. resource ^. board)

updateLookAt :: Enemy -> Field -> StateT Field Game (Enemy, [V.Vector Bullet])
updateLookAt e f = do
  LookAt e' _ r' <- lift $ runDanmaku (barrage (e^.kind)) `execStateT` (LookAt e f [])
  return (e', r')

addBulletP :: Resource -> Player -> (V.Vector Bullet)
addBulletP res p = do
  if (p ^. keys ^. zKey > 0 && p ^. counter `mod` 10 == 0) then
    V.singleton $ lineBullet (p ^. pos) (fst $ res ^. bulletImg)
  else
    V.empty

  where
    lineBullet :: Vec -> Bitmap -> Bullet
    lineBullet p r = initBullet p 5 (pi/2) (bulletBitmap Diamond Red r) (KindBullet 0) 0

collideE :: ([Enemy], V.Vector Bullet) -> Game ([Enemy], V.Vector Bullet)
collideE (es, bs) = return $ run es bs
  
  where
    run :: [Enemy] -> V.Vector Bullet -> ([Enemy], V.Vector Bullet)
    run [] bs = ([], bs)
    run (e:es) bs = let
        (e', bs') = collide e bs
        (es', bs'') = run es bs' in
        (e':es', bs'')

collideP :: (Player, V.Vector Bullet) -> Game (Player, V.Vector Bullet)
collideP (p,bs) = return $ collide p bs
  
collide :: (HasChara c, HasObject c) => c -> V.Vector Bullet -> (c, V.Vector Bullet)
collide c bs = (,)
  (hp %~ (\x -> x - (V.length bs - V.length bs')) $ c)
  bs'
  
  where
    bs' :: V.Vector Bullet
    bs' = V.filter (\b -> not $ detect (b^.pos, b^.size) (c^.pos, c^.size)) bs

    detect :: (Vec, Vec) -> (Vec, Vec) -> Bool
    detect (pos1, size1) (pos2, size2) =
      let V2 dx dy = fmap abs $ pos1 - pos2; V2 sx sy = size1 + size2 in
      dx < sx/2 && dy < sy/2
