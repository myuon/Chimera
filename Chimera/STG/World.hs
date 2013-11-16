{-# LANGUAGE TemplateHaskell, GADTs #-}
module Chimera.STG.World (
  LookAt, local, global
  , AtEnemy
  , Field
  , player, enemy, bulletP, bulletE, enemyQ, resource
  , initField
  ) where

import Graphics.UI.FreeGame
import Control.Arrow
import Control.Lens

import Chimera.STG.Types
import Chimera.STG.Util
import Chimera.Load
import qualified Chimera.STG.UI as UI

data LookAt p q = LookAt {
  _local :: p,
  _global :: q
  }

makeLenses ''LookAt

type AtEnemy = LookAt Enemy Field

data Field = Field {
  _player :: Player,
  _enemy :: [Enemy],
  _bulletP :: [Bullet],
  _bulletE :: [Bullet],
  _enemyQ :: [(Int, Enemy)],

  _resource :: Resource
  }

makeLenses ''Field

initField :: Resource -> Field
initField res = Field {
  _player = initPlayer (fst $ res ^. charaImg),
  _enemy = [],
  _bulletP = [],
  _bulletE = [],
  _enemyQ = [],

  _resource = res
  }
