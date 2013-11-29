module Chimera.Scripts.Stage1 (
  load1, stage1, zako
  , doBullet
  )
  where

import Graphics.UI.FreeGame
import Control.Lens
import Control.Monad
import Control.Monad.Operational.Mini
import Control.Monad.State.Strict (get, put, execStateT, State, StateT)

import Chimera.STG.Util
import Chimera.STG.Types
import Chimera.STG.World
import Chimera.Load
import Chimera.Scripts

load1 :: Resource
load1 = def

stage1 :: Stage ()
stage1 = do
  res <- getResource

  appearAt 30 $ initEnemy (V2 320 (-40)) 2 res (Zako 1 10)

zako :: Int -> Danmaku ()
zako n
  | n >= 20 = zakoCommon 0 (motionCommon 100 (Curve (acc $ n `mod` 10))) 50 Needle Purple
  | n >= 10 = zakoCommon 0 (motionCommon 100 (Straight)) 50 BallMedium (toEnum $ n `mod` 10)
  | otherwise = return ()
  where
    acc :: Int -> Vec
    acc 0 = V2 (-0.05) 0.005
    acc 1 = V2 0.05 0.005

doBullet :: Int -> State Bullet ()
doBullet 0 = doBulletCommon 0
