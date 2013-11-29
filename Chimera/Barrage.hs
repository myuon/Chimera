{-# LANGUAGE RankNTypes, GADTs, FlexibleContexts #-}
module Chimera.Barrage (
  barrage, runBullet
  ) where

import Graphics.UI.FreeGame
import Control.Monad.Operational.Mini (singleton)
import Control.Monad
import Control.Monad.State.Strict (get, put, execStateT, State, StateT)
import Control.Lens

import Chimera.Load
import Chimera.STG.Types
import Chimera.STG.World
import Chimera.STG.Util

import qualified Chimera.Scripts as Scripts
import qualified Chimera.Scripts.Stage1 as Stage1

barrage :: KindEnemy -> Danmaku ()
barrage (Zako s n)
  | s == 1 = Stage1.zako n
  | otherwise = return ()
--barrage (Boss n) = boss n
barrage (Debug) = Scripts.debug

runBullet :: KindBullet -> State Bullet ()
runBullet (KindBullet s n)
  | s == 0 = Scripts.doBulletCommon n
  | s == 1 = Stage1.doBullet n

