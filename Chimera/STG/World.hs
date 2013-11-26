{-# LANGUAGE TemplateHaskell, GADTs, RankNTypes, FlexibleContexts, TypeSynonymInstances, FlexibleInstances #-}
module Chimera.STG.World (
  LookAt(..), local, global
  , AtEnemy
  , Field(..)
  , player, enemy, bulletP, bulletE, stage, resource, counterF, isDebug
  , loadField
  , runDanmaku, runStage

  , getResource
  , Pattern(..)
  , Danmaku
  , Line(..), Stage, appear, wait, appearAt, keeper
  ) where

import Graphics.UI.FreeGame
import Control.Arrow
import Control.Lens
import Control.Monad.State.Strict (get, put, modify, execStateT, State, StateT)
import Control.Monad.Operational.Mini
import Control.Monad.Operational.TH (makeSingletons)
import qualified Data.Vector as V
import Data.Default

import Chimera.STG.Types
import Chimera.STG.Util
import Chimera.Load
import qualified Chimera.STG.UI as UI

class HasGetResource c where
  getResource :: c Resource

data Pattern p where
  Shots :: [Bullet] -> Pattern ()
  GetPlayer :: Pattern Player
  Get :: Pattern Enemy
  Put :: Enemy -> Pattern ()
  GetResourcePattern :: Pattern Resource

type Danmaku = Program Pattern

getResourcePattern :: Danmaku Resource
getResourcePattern = singleton GetResourcePattern

instance HasGetResource Danmaku where
  getResource = getResourcePattern


data Line p where
  GetResourceLine :: Line Resource
  Appear :: Enemy -> Line ()
  Wait :: Int -> Line ()
  Stop :: Line ()

makeSingletons ''Line

type Stage = ReifiedProgram Line

instance HasGetResource Stage where
  getResource = getResourceLine

appearAt :: Int -> Enemy -> Stage ()
appearAt n e = wait n >> appear e

keeper :: Enemy -> Stage ()
keeper e = appear e >> stop

data Field = Field {
  _player :: Player,
  _enemy :: [Enemy],
  _bulletP :: V.Vector Bullet,
  _bulletE :: V.Vector Bullet,

  _stage :: Stage (),
  _resource :: Resource,
  _counterF :: Int,
  _isDebug :: Bool
  }

makeLenses ''Field

instance Default Field where
  def = Field {
    _player = undefined,
    _enemy = [],
    _bulletP = V.empty,
    _bulletE = V.empty,

    _stage = return (),
    _resource = undefined,
    _counterF = 0,
    _isDebug = False
    }

loadField :: Field -> Field
loadField f =
  player .~ initPlayer (fst $ (f^.resource)^.charaImg) $
  f

data LookAt p q r = LookAt {
  _local :: p,
  _global :: q,
  _result :: r
  }

makeLenses ''LookAt

type AtEnemy = LookAt Enemy Field [V.Vector Bullet]

runDanmaku :: Danmaku () -> StateT AtEnemy Game ()
runDanmaku = interpret step
  where
    step :: Pattern a -> StateT AtEnemy Game a
    step GetResourcePattern = use global >>= \f -> return $ f ^. resource
    step Get = use local
    step (Put e) = local .= e
    step (Shots bs) = result %= ((:) (V.fromList bs))
    step GetPlayer = use global >>= \f -> return $ f ^. player

runStage :: Stage () -> StateT (LookAt () Field (Maybe Enemy)) Game (Stage ())
runStage (GetResourceLine :>>= next) = next `fmap` use (global.resource)
runStage (Appear e :>>= next) = result .= (Just e) >> return (next ())
runStage line@(Wait n :>>= next) = case n == 0 of
    True -> return (next ())
    False -> return (Wait (n-1) :>>= next)
runStage line@(Stop :>>= next) = use (global.enemy) >>= (\es -> case length es == 0 of
    True -> return (next ())
    False -> return line)
runStage line@(Return _) = return line
