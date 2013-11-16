{-# LANGUAGE GADTs, TypeSynonymInstances, FlexibleInstances #-}
module Chimera.STG (
  update, draw
  
  , module Chimera.STG.World
  ) where

import Graphics.UI.FreeGame
import Control.Lens
import Control.Arrow
import Control.Monad.Operational.Mini (interpret)
import Control.Monad.State (get, execState, evalStateT, State, StateT)

import Chimera.STG.Types
import Chimera.STG.World
import Chimera.STG.Util
import qualified Chimera.STG.UI as UI

runDanmaku :: Danmaku () -> State AtEnemy ()
runDanmaku = interpret exec

exec :: Pattern' x -> State AtEnemy x
exec Get = use local
exec (Put e) = local .= e


class GUIClass c where
  update :: StateT c Game ()
  draw :: StateT c Game ()

instance GUIClass Player where
  update = do
    p <- get
    (chara.counter) %= (+1)
    bracket $ do
      key <- p ^. keyState
      updatePlayer key `evalStateT` p
    
    where
      updatePlayer :: UI.Keys -> StateT Player Game ()
      updatePlayer key = do
        s <- use (chara.speed)
        (chara.pos) %= (+ (s $* dir key))
        (chara.pos) %= clamp
      
      dir :: UI.Keys -> Vec
      dir key = let addTup b p q = bool q (fromPair p+q) b in
        addTup (key ^. UI.up    > 0) (0,-1) $
        addTup (key ^. UI.down  > 0) (0,1) $
        addTup (key ^. UI.right > 0) (1,0) $
        addTup (key ^. UI.left  > 0) (-1,0) $
        fromPair (0,0)
  draw =
--    Game.translate (fmap realToFrac $ p ^. pos) $ Game.fromBitmap img
    embedIO $ putStrLn "drawing player"

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
    f <- get
    bracket $ do
      update `evalStateT` (f ^. player)
      updateEnemies (f ^. enemy) `evalStateT` f

    embedIO $ putStrLn "updating field"
    
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
    bracket $ do
      draw `evalStateT` p
    embedIO $ putStrLn "drawing field"
--    liftUI $ (evalStateT draw) (f ^. player)

