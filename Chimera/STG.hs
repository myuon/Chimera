{-# LANGUAGE GADTs, TypeSynonymInstances, FlexibleInstances, FlexibleContexts, Rank2Types #-}
module Chimera.STG (
  update, draw
  
  , module Chimera.STG.World
  , module Chimera.STG.UI
  , module Chimera.STG.Types
  ) where

import Graphics.UI.FreeGame
import Control.Lens
import Control.Arrow ((***))
import Control.Monad.Operational.Mini (interpret)
import Control.Monad.State (get, put, execState, execStateT, evalStateT, runStateT, State, StateT)

import Chimera.STG.Types
import Chimera.STG.World
import Chimera.STG.Util
import Chimera.Load
import Chimera.STG.UI

import Control.Monad.Free (MonadFree)
import Control.Monad.Free.Church (F)

runDanmaku :: Danmaku () -> State AtEnemy ()
runDanmaku = interpret exec

exec :: Pattern' x -> State AtEnemy x
exec Get = use local
exec (Put e) = local .= e

-- access to methods in superclass
(./) :: s -> StateT s Game () -> StateT c Game s
s ./ m = bracket $ m `execStateT` s

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

instance GUIClass Bullet where
  update = do
    embedIO $ putStrLn "updating a bullet"
  draw = do
    embedIO $ putStrLn "drawing a bullet"

instance GUIClass Field where
  update = do
    p <- use player
    player <.- (p ./ update)
    
    where
      updateEnemies :: [Enemy] -> StateT Field Game ()
      updateEnemies _ = do
        enemy %= filter (\e -> e^.hp > 0)

--    enemy %= filter (\e -> e ^. hp > 0 && e ^. mstate /= Dead) . map (\e -> (\(LookAt e _) -> e) $ (execState $ runDanmaku Barrage.zako1) (LookAt e f))

  --  bulletE %= filter (\b -> isInside $ b ^. pos) . map (\b -> (execState $ Barrage.barrage (b ^. barrage) ^. Barrage.bullet) b)
  --  bulletP %= filter (\b -> isInside $ b ^. pos) . map (\b -> (execState $ Barrage.barrage (b ^. barrage) ^. Barrage.bullet) b)  
  --  enemy %= filter (\e -> e ^. hp > 0 && e ^. mstate /= Dead) . map (\e -> (execState $ (Barrage.barrage (e ^. kindEnemy) ^. Barrage.enemy) p) e)
    
  draw = do
    p <- use player
    res <- use resource
    bracket $ do
      draw `evalStateT` p
      translate (V2 320 240) $ fromBitmap (res ^. board)

