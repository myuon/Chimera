{-# LANGUAGE TemplateHaskell, GADTs #-}
module Chimera.STG.World (
  LookAt, local, global
  , AtEnemy
  , Field
  , player, enemy, bulletP, bulletE, enemyQ
  , initField
  ) where

import Graphics.UI.FreeGame
import Control.Arrow
import Control.Lens

import Chimera.STG.Types
import Chimera.STG.Util
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
  _enemyQ :: [(Int, Enemy)]
  }

makeLenses ''Field

initField :: Field
initField = Field {
  _player = initPlayer,
  _enemy = [],
  _bulletP = [],
  _bulletE = [],
  _enemyQ = [
    ]
  }
