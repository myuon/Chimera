{-# LANGUAGE TemplateHaskell, GADTs, RankNTypes, FlexibleContexts, TypeSynonymInstances, FlexibleInstances #-}
module Chimera.STG.World (
  Enemy
  , Effect, effectObject, stateEffect, StateEffect(..), effEnemyDead
  , AtEnemy
  , Field(..)
  , player, enemy, bulletP, bulletE, stage, resource, counterF, isDebug, effects
  , loadField
  , runDanmaku, runStage

  , getResource
  , Pattern(..)
  , Danmaku
  , Line(..), Stage, appear, wait, appearAt, keeper
  , module Chimera.STG.Types
  ) where

import Graphics.UI.FreeGame
import Control.Arrow
import Control.Lens
import Control.Monad.State.Strict (get, put, modify, execStateT, State, StateT)
import Control.Monad.Operational.Mini
import Control.Monad.Operational.TH (makeSingletons)
import qualified Data.Sequence as S
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

type Enemy = Autonomie Danmaku EnemyObject

instance HasObject Enemy where object = auto . object
instance HasChara Enemy where chara = auto . chara
instance HasEnemyObject Enemy where enemyObject = auto

instance Default Enemy where
  def = Autonomie {
    _auto = def,
    _runAuto = return ()
  }

data Line p where
  GetResourceLine :: Line Resource
  Appear :: Enemy -> Line ()
  Wait :: Int -> Line ()
  Stop :: Line ()

makeSingletons ''Line

type Stage = ReifiedProgram Line

instance HasGetResource Stage where
  getResource = getResourceLine

data StateEffect = Active | Inactive deriving (Eq, Show)

data EffectObject = EffectObject {
  _objectEffect :: Object,
  _ress :: V.Vector Bitmap,
  _stateEffect :: StateEffect,
  _slowRate :: Int
  }

makeClassy ''EffectObject

instance HasObject EffectObject where object = objectEffect

instance Default EffectObject where
  def = EffectObject {
    _objectEffect = def,
    _ress = V.empty,
    _stateEffect = Active,
    _slowRate = 5
  }

type Effect = Autonomie (State EffectObject) EffectObject

instance HasObject Effect where object = auto . objectEffect
instance HasEffectObject Effect where effectObject = auto

instance Default Effect where
  def = Autonomie {
    _auto = def,
    _runAuto = return ()
  }

effEnemyDead :: Resource -> Vec -> Effect
effEnemyDead res p =
  pos .~ p $
  ress .~ (V.fromList $ cutIntoN 10 $ (res^.effectImg) V.! 0) $
  runAuto .~ run $
  (def :: Effect)

  where
    cutIntoN :: Int -> Bitmap -> [Bitmap]
    cutIntoN n img = let (w,h) = bitmapSize img; w1 = w `div` n in
      [cropBitmap img (w1,h) (w1*i,0) | i <- [0..n-1]]

    run :: State EffectObject ()
    run = do
      f <- get
      res <- use ress
      let i = (f^.counter) `div` (f^.slowRate)
      img .= res V.! i
      counter %= (+1)
      when (i == V.length res) $ stateEffect .= Inactive

data Field = Field {
  _player :: Player,
  _enemy :: S.Seq Enemy,
  _bulletP :: S.Seq Bullet,
  _bulletE :: S.Seq Bullet,
  _effects :: S.Seq Effect,

  _stage :: Stage (),
  _resource :: Resource,
  _counterF :: Int,
  _isDebug :: Bool
  }

makeLenses ''Field

instance Default Field where
  def = Field {
    _player = undefined,
    _enemy = S.empty,
    _bulletP = S.empty,
    _bulletE = S.empty,
    _effects = S.empty,

    _stage = return (),
    _resource = undefined,
    _counterF = 0,
    _isDebug = False
    }

loadField :: Field -> Field
loadField f =
  player .~ ((img .~ (fst $ (f^.resource)^.charaImg)) $ def) $
  f

type AtEnemy = LookAt Enemy Field

runDanmaku :: Danmaku () -> State AtEnemy ()
runDanmaku = interpret step
  where
    step :: Pattern a -> State AtEnemy a
    step GetResourcePattern = use global >>= \f -> return $ f ^. resource
    step Get = use local
    step (Put e) = local .= e
    step (Shots bs) = (local.shotQ) %= (S.><) (S.fromList bs)
    step GetPlayer = use global >>= \f -> return $ f ^. player

appearAt :: Int -> Enemy -> Stage ()
appearAt n e = wait n >> appear e

keeper :: Enemy -> Stage ()
keeper e = appear e >> stop

runStage :: Stage () -> State (LookAt (Maybe Enemy) Field) (Stage ())
runStage (GetResourceLine :>>= next) = next `fmap` use (global.resource)
runStage (Appear e :>>= next) = local .= (Just e) >> return (next ())
runStage line@(Wait n :>>= next) = case n == 0 of
    True -> return (next ())
    False -> return (Wait (n-1) :>>= next)
runStage line@(Stop :>>= next) = use (global.enemy) >>= (\es -> case S.length es == 0 of
    True -> return (next ())
    False -> return line)
runStage line@(Return _) = return line

