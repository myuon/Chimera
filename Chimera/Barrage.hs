{-# LANGUAGE RankNTypes, GADTs, FlexibleContexts #-}
module Chimera.Barrage (
  barrage
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
barrage (Boss _ n) = Stage1.boss n
barrage (Debug) = Scripts.debug

