{-# LANGUAGE TemplateHaskell, TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts, RankNTypes #-}
module Chimera.Core.World (
  Player, keysPlayer, actPlayer
  , Bullet, Enemy, Field, StateField(..), Danmaku
  , player, enemy, bullets, effects, stateField
  , makeBullet
  , resource, counterF, isDebug
  , liftLocal, liftGlobal, readGlobal
  , runDanmaku, runAutonomie
  , module M
  ) where

import FreeGame
import Control.Lens
import Control.Monad.State.Strict (State, StateT, get, execState, execStateT, lift)
import qualified Data.Vector as V
import qualified Data.Sequence as S
import qualified Data.Map as M
import Data.Default

import Chimera.Core.Types as M
import Chimera.Core.Util as M
import Chimera.Core.Load (picture, GetPicture, areaBullet, getBulletBitmap)

keyList :: [Key]
keyList = [charToKey 'Z', charToKey 'X', KeyUp, KeyDown, KeyRight, KeyLeft, KeyLeftShift, KeyRightShift]

data Player = Player {
  _charaPlayer :: Chara,
  _keysPlayer :: M.Map Key Int
  }

makeLenses ''Player

actPlayer :: StateT Player Game ()
actPlayer = do
  pairs <- lift $ mapM (\k -> (,) k `fmap` fromEnum `fmap` keyPress k) keyList
  keysPlayer %= M.unionWith go (M.fromList pairs)
  where
    go a b
      | a == 0 = 0
      | otherwise = a + b

instance HasChara Player where chara = charaPlayer
instance HasObject Player where object = chara . object

instance Default Player where
  def = Player {
    _charaPlayer =
      pos .~ V2 320 420 $ 
      speed .~ 2.5 $
      size .~ V2 5 5 $
      hp .~ 10 $
      def,
    _keysPlayer = M.fromList $ zip keyList [0..]
    }

instance GetPicture Player where
  picture res _ = (res^.charaImg) V.! 0

instance GUIClass Player where
  update = do
    s <- use speed
    k <- use keysPlayer
    counter %= (+1)
    pos += case k M.! KeyLeftShift > 0 || k M.! KeyRightShift > 0 of
      True -> ((0.5 * s) *^ dir k)
      False -> s *^ dir k
    pos %= clamp

    where
      dir :: M.Map Key Int -> Vec2
      dir k = let addTup b p q = bool q (fromPair p+q) b in
        addTup (k M.! KeyUp    > 0) (0,-1) $
        addTup (k M.! KeyDown  > 0) (0,1) $
        addTup (k M.! KeyRight > 0) (1,0) $
        addTup (k M.! KeyLeft  > 0) (-1,0) $
        fromPair (0,0)

  paint res = do
    p <- get
    draw $ translate (p^.pos) $ bitmap (picture res p)

instance GUIClass Effect where
  update = do
    run <- use runAuto
    effectObject %= execState run
  
  paint res = do
    b <- get
    lift $ translate (b^.pos) $ rotateR (b^.angle) $ scale (b^.size) $ b^.img $ res

type Danmaku c = Runner c (Field, S.Seq (State Field ()))

type Bullet = Autonomie (Danmaku BulletObject) BulletObject
instance HasObject Bullet where object = auto . object
instance HasBulletObject Bullet where bulletObject = auto

instance GetPicture Bullet where
  picture res b = getBulletBitmap (res^.bulletImg) (b^.kind) (b^.bcolor)

instance GUIClass Bullet where
  update = do
    r <- use speed
    t <- use angle
    pos %= (+ fromPolar (r,t))
    p <- use pos
    unless (isInside p) $ stateBullet .= Outside

  paint res = do
    b <- get
    case b^.size^._x /= b^.size^._y of
      True -> draw $ translate (b^.pos) $ 
              rotateR (b^.angle + pi/2) $ bitmap (picture res b)
      False -> draw $ translate (b^.pos) $ bitmap (picture res b)

makeBullet :: (HasObject c, HasBulletObject c) => c -> c
makeBullet b = b & size .~ areaBullet (b^.kind)

type Enemy = Autonomie (Danmaku EnemyObject) EnemyObject
instance HasObject Enemy where object = auto . object
instance HasChara Enemy where chara = auto . chara
instance HasEnemyObject Enemy where enemyObject = auto

instance GetPicture Enemy where
  picture res _ = (res^.charaImg) V.! 1

instance GUIClass Enemy where
  update = do
    sp <- use spXY
    pos %= (+sp)
    counter %= (+1)
    h <- use hp
    when (h <= 0) $ stateChara .= Dead
    effectEnemy %= 
      S.filter (\e -> e^.stateEffect /= Inactive) . fmap (\e -> update `execState` e)
  
  paint res = do
    c <- get
    mapM_' (\e -> lift $ paint res `execStateT` e) (c^.effectEnemy)
    draw $ translate (c^.pos) $ bitmap (picture res c)

data StateField = Shooting | Talking deriving (Eq, Show)

data Field = Field {
  _player :: Player,
  _enemy :: S.Seq Enemy,
  _bullets :: S.Seq Bullet,
  _effects :: S.Seq Effect,
  
  _resource :: Resource,
  _counterF :: Int,
  _isDebug :: Bool,
  _stateField :: StateField
  }

makeLenses ''Field

instance Default Field where
  def = Field {
    _player = def,
    _enemy = S.empty,
    _bullets = S.empty,
    _effects = S.empty,
    
    _resource = error "_resource is not defined.",
    _counterF = 0,
    _isDebug = False,
    _stateField = Shooting
    }


instance HasGetResource (Danmaku c) where
  getResource = ((^.resource) . fst) `fmap` getGlobal

runDanmaku :: Danmaku c () -> State (LookAt c (Field, S.Seq (State Field ()))) ()
runDanmaku = runPattern

liftGlobal :: State Field () -> Danmaku c ()
liftGlobal u = do
  f <- getGlobal
  putGlobal (fst f, snd f S.|> u)
  
readGlobal :: Danmaku c Field
readGlobal = fst `fmap` getGlobal

liftLocal :: State c a -> Danmaku c a
liftLocal = liftState getLocal putLocal

runAutonomie :: (Autonomic c (Danmaku a) a) => Lens' Field (S.Seq c) -> State Field ()
runAutonomie member = runLookAt member go where
  go :: (Autonomic c (Danmaku a) a) => Field -> c -> (c, S.Seq (State Field ()))
  go f e = let k = LookAt (e^.auto) (f, S.empty)
               LookAt eo' (_, fs) = (runDanmaku $ e^.runAuto) `execState` k in
           (e & auto .~ eo', fs)

