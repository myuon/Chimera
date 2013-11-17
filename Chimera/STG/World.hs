{-# LANGUAGE TemplateHaskell, GADTs, RankNTypes #-}
module Chimera.STG.World (
  LookAt(..), local, global
  , AtEnemy
  , Field
  , player, enemy, bulletP, bulletE, stage, resource
  , initField
  , runDanmaku, runStage
  ) where

import Graphics.UI.FreeGame
import Control.Arrow
import Control.Lens
import Control.Monad.State (get, put, execStateT, State, StateT)
import Control.Monad.Operational.Mini

import Chimera.STG.Types
import Chimera.STG.Util
import Chimera.Load
import qualified Chimera.STG.UI as UI

data Field = Field {
  _player :: Player,
  _enemy :: [Enemy],
  _bulletP :: [Bullet],
  _bulletE :: [Bullet],

  _stage :: Stage (),
  _resource :: Resource,
  _counter :: Int
  }

makeLenses ''Field

initField :: Resource -> Field
initField res = Field {
  _player = initPlayer (fst $ res ^. charaImg),
  _enemy = [],
  _bulletP = [],
  _bulletE = [],

  _stage = return (),
  _resource = res,
  _counter = 0
  }

data LookAt p q = LookAt {
  _local :: p,
  _global :: q
  }

makeLenses ''LookAt

type AtEnemy = LookAt Enemy Field

runDanmaku :: Danmaku () -> StateT AtEnemy Game ()
runDanmaku = interpret step
  where
    step :: Pattern a -> StateT AtEnemy Game a
    step GetResourcePattern = use global >>= \f -> return $ f ^. resource
    step Get = use local
    step (Put e) = local .= e
    step (Shots bs) = (global.bulletE) %= (bs++)
    step GetPlayer = use global >>= \f -> return $ f ^. player
    step _ = undefined

runStage :: Stage () -> StateT Field Game (Stage ())
runStage (GetResourceLine :>>= next) = count >> next `fmap` use resource
runStage line@(Appear n e :>>= next) = count >> use counter >>= (\c ->
  case c > n of
    True -> enemy %= (e:) >> return (next ())
    False -> return line)
runStage line@(Return _) = return line

count :: StateT Field Game ()
count = counter %= (+1)
