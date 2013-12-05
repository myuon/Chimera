{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts, Rank2Types #-}
module Chimera.STG (
  update, draw, loadStage

  , module M
  ) where

import Graphics.UI.FreeGame
import Control.Lens
import Control.Arrow ((***))
import Control.Monad.State.Strict
import qualified Data.Sequence as S
import qualified Data.Foldable as F
import Control.Monad.Trans.Class (lift)

import Chimera.STG.World as M
import Chimera.STG.Util
import Chimera.Load
import Chimera.STG.UI as M

import Chimera.Scripts
import Chimera.Scripts.Stage1

stage2 :: Stage ()
stage2 = do
  res <- getResource
  keeper $ initEnemy (V2 260 40) 2 res (Debug)

loadStage :: Field -> Field
loadStage f =
  loadField $
  stage .~ stage1 $
  resource .~ load1 $
  f

class GUIClass c where
  update :: State c ()
  draw :: StateT c Game ()

instance GUIClass Player where
  update = do
    s <- use speed
    k <- use keys
    counter %= (+1)
    pos %= clamp . (+ (bool id (0.5*^) (k^.shift>0) $ s *^ dir k))

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
    when (h <= 0) $ stateEnemy .= Dead
  
  draw = do
    e <- get
    translate (e ^. pos) $ fromBitmap (e ^. img)

instance GUIClass Bullet where
  update = do
    b <- get
    bulletObject %= execState (b^.runAuto)

  draw = do
    b <- get
    translate (b ^. pos) $ rotateR (b ^. angle + pi/2) $ fromBitmap (b ^. img)

instance GUIClass Field where
  update = do
    -- collision
    collideE
    collideP
    
    -- append
    f <- get
    let (s', LookAt me _) = runStage (f^.stage) `runState` (LookAt Nothing f)
    stage .= s'
    counterF %= (+1)
    enemy %= maybe id (:) me
    addBulletP

    es <- use enemy
    bulletE %= (S.>< (S.fromList $ concat $ concat $ fmap (^.shotQ) es))
    enemy %= fmap (\e -> shotQ .~ [[]] $ e)

    -- run
    f <- get
    enemy %= fmap (\e -> (^.local) $ runDanmaku (e^.runAuto) `execState` LookAt e f)
    
    -- move
    bulletP %= S.filter (\b -> isInside $ b^.pos) . fmap (execState update)
    bulletE %= S.filter (\b -> isInside $ b^.pos) . fmap (execState update)
    enemy %= fmap (execState update) . filter (\e -> e^.stateEnemy /= Dead)
    player %= execState update

  draw = do
    f <- get
    mapM_' (\p -> lift $ draw `execStateT` p) (f^.bulletP)
    lift $ draw `execStateT` (f ^. player)
    mapM_' (\b -> lift $ draw `execStateT` b) (f^.bulletE)
    mapM_ (\e -> lift $ draw `execStateT` e) (f^.enemy)

    when (f^.isDebug) $ do
      mapM_' (\b -> colored blue . polygon $ boxVertex (b^.pos) (b^.size)) (f ^. bulletP)
      (\p -> colored yellow . polygon $ boxVertex (p^.pos) (p^.size)) $ f^.player
      mapM_' (\b -> colored red . polygon $ boxVertex (b^.pos) (b^.size)) (f ^. bulletE)
      mapM_ (\e -> colored green . polygon $ boxVertex (e^.pos) (e^.size)) (f ^. enemy)
    
    translate (V2 320 240) $ fromBitmap (f^.resource^.board)
    
    where
      sequence_' :: Monad m => S.Seq (m a) -> m () 
      sequence_' ms = F.foldr (>>) (return ()) ms
      
      mapM_' :: Monad m => (a -> m b) -> S.Seq a -> m ()
      mapM_' f as = sequence_' (fmap f as)

addBulletP :: State Field ()
addBulletP = do
  p <- use player
  when (p ^. keys ^. zKey > 0 && p ^. counter `mod` 10 == 0) $ do
    res <- use resource
    bulletP %= (<|) (
      pos .~ (p^.pos) $ 
      speed .~ 5 $ 
      angle .~ pi/2 $ 
      img .~ (bulletBitmap Diamond Red (snd $ res^.bulletImg)) 
      $ def)

collideE :: State Field ()
collideE = do
  es <- use enemy
  bs <- use bulletP
  
  let (es', bs') = run es bs
  enemy .= es'
  bulletP .= bs'

  where
    run :: [Enemy] -> S.Seq Bullet -> ([Enemy], S.Seq Bullet)
    run [] bs = ([], bs)
    run (e:es) bs = let
        (e', bs') = collide e bs
        (es', bs'') = run es bs' in
        (e':es', bs'')

collideP :: State Field ()
collideP = do
  p <- use player
  bs <- use bulletE
  
  let (p', bs') = collide p bs
  player .= p'
  bulletE .= bs'
  
collide :: (HasChara c, HasObject c) => c -> S.Seq Bullet -> (c, S.Seq Bullet)
collide c bs = (,)
  (hp %~ (\x -> x - S.length bs') $ c)
  (S.filter (\b -> (Nothing ==) $ b `S.elemIndexL` bs') bs)
  
  where
    bs' :: S.Seq Bullet
    bs' = S.filter (\b -> detect (b^.pos, b^.size) (c^.pos, c^.size)) bs

    detect :: (Vec, Vec) -> (Vec, Vec) -> Bool
    detect (pos1, size1) (pos2, size2) =
      let V2 dx dy = fmap abs $ pos1 - pos2; V2 sx sy = size1 + size2 in
      dx < sx/2 && dy < sy/2
