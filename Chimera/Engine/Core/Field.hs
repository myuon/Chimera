{-# LANGUAGE TemplateHaskell, Rank2Types, FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Chimera.Engine.Core.Field where

import FreeGame
import Control.Lens
import Control.Monad.State.Strict
import qualified Data.Sequence as S
import qualified Data.Vector as V
import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import Data.Reflection
import Data.Default
import Data.Char (digitToInt)
import Data.Functor.Product

import Chimera.Engine.Core.Util
import Chimera.Engine.Core.Types

data StateEffect = Active | Inactive deriving (Eq, Enum, Show)
data ZIndex = Background | OnObject | Foreground deriving (Eq, Show)
data StateChara = Alive | Attack | Damaged | Dead deriving (Eq, Enum, Show)
data StateBullet = PlayerB | EnemyB | Outside deriving (Eq, Ord, Enum, Show)
data BKind = BallLarge | BallMedium | BallSmall | BallFrame | BallTiny |
             Oval | Diamond | Needle deriving (Eq, Ord, Enum, Show)
data BColor = Red | Orange | Yellow | Green | Cyan | Blue | Purple | Magenta
  deriving (Eq, Ord, Enum, Show)

data Chara = Chara {
  _objectChara :: Object,
  _stateChara :: StateChara,
  _hp :: Int,
  _effectIndexes :: S.Seq Int
  }

data EffectObject = EffectObject {
  _objectEffect :: Object,
  _stateEffect :: StateEffect,
  _slowRate :: Int,
  _img :: Resource -> Game (),
  _zIndex :: ZIndex
  }

data BulletObject = BulletObject {
  _objectBullet :: Object,
  _stateBullet :: StateBullet,
  _kind :: BKind,
  _bcolor :: BColor
  } deriving (Eq, Show)

data EnemyObject = EnemyObject {
  _charaEnemy :: Chara
  }

data Player = Player {
  _charaPlayer :: Chara,
  _keysPlayer :: M.Map Key Int,
  _shotZ :: (Given Resource) => State Chara (S.Seq Bullet),
  _shotX :: (Given Resource) => State Chara (S.Seq Bullet)
  }

data Field = Field {
  _player :: Player,
  _enemy :: S.Seq Enemy,
  _bullets :: S.Seq Bullet,
  _effects :: IM.IntMap Effect,
  _counterF :: Int,
  _isDebug :: Bool,
  _danmakuTitle :: String
  }

type Effect = Autonomie (State EffectObject) EffectObject
type Bullet = Autonomie (Danmaku BulletObject) BulletObject
type Enemy = Autonomie (Danmaku EnemyObject) EnemyObject
type Danmaku c = LookAt c Field

makeClassy ''Chara
makeClassy ''EffectObject
makeClassy ''BulletObject
makeClassy ''EnemyObject
makeLenses ''Player
makeLenses ''Field

instance HasObject Chara where object = objectChara
instance HasObject EffectObject where object = objectEffect
instance HasEffectObject Effect where effectObject = auto
instance HasObject Effect where object = auto . objectEffect
instance HasObject BulletObject where object = objectBullet
instance HasChara EnemyObject where chara = charaEnemy
instance HasObject EnemyObject where object = chara . object
instance HasChara Player where chara = charaPlayer
instance HasObject Player where object = chara . object
instance HasObject Bullet where object = auto . object
instance HasBulletObject Bullet where bulletObject = auto
instance HasObject Enemy where object = auto . object
instance HasChara Enemy where chara = auto . chara
instance HasEnemyObject Enemy where enemyObject = auto

instance Default Chara where
  def = Chara { 
    _objectChara = def,
    _stateChara = Alive,
    _hp = 0,
    _effectIndexes = S.empty
    }

instance Default EffectObject where
  def = EffectObject { 
    _objectEffect = def, 
    _stateEffect = Active,
    _slowRate = 3,
    _img = error "_img is not defined",
    _zIndex = Background
    }

instance Default BulletObject where
  def = BulletObject { 
    _objectBullet = (size .~ V2 3 3 $ def),
    _stateBullet = EnemyB,
    _kind = BallMedium,
    _bcolor = Red
    }

instance Default EnemyObject where
  def = EnemyObject {
    _charaEnemy =
      spXY .~ V2 0 0 $
      size .~ V2 15 15 $
      def
    }

instance Default Player where
  def = Player {
    _charaPlayer =
      pos .~ V2 320 420 $ 
      speed .~ 2.5 $
      size .~ V2 5 5 $
      hp .~ 10 $
      def,
    _keysPlayer = M.fromList $ zip keyList [0..],
    _shotZ = error "uninitialized shotZ",
    _shotX = error "uninitialized shotX"
    }

instance Default Field where
  def = Field {
    _player = def,
    _enemy = S.empty,
    _bullets = S.empty,
    _effects = IM.empty,
    _counterF = 0,
    _isDebug = False,
    _danmakuTitle = ""
    }

collide :: (HasObject c, HasObject b) => c -> b -> Bool
collide oc ob = let oc' = extend oc; ob' = extend ob; in
  detect ob' oc' || detect oc' ob'
  where
    extend :: (HasObject c) => c -> c
    extend x = x & size -~ V2 (x^.speed) 0 `rotate2` (-x^.ang) + (x^.spXY)
--    extend x = x & size +~ (x^.speed) * (V2 1 0) `rotate2` (-x^.angle)

    detect :: (HasObject c, HasObject c') => c -> c' -> Bool
    detect a b = 
      let V2 w' h' = a^.size
          r = \v -> rotate2 v $ a^.ang in
      or [(a^.pos) `isIn` b,
          (a^.pos + r (V2   w'    h' )) `isIn` b,
          (a^.pos + r (V2 (-w')   h' )) `isIn` b,
          (a^.pos + r (V2   w'  (-h'))) `isIn` b,
          (a^.pos + r (V2 (-w') (-h'))) `isIn` b]
    
    isIn :: (HasObject c) => Vec2 -> c -> Bool
    isIn p box = isInCentoredBox (p-box^.pos) where
      isInCentoredBox :: Vec2 -> Bool
      isInCentoredBox p' = let V2 px' py' = p' `rotate2` (-box^.ang) in
        abs px' < (box^.size^._x)/2 && abs py' < (box^.size^._y)/2

areaBullet :: BKind -> Vec2
areaBullet BallLarge = V2 15 15
areaBullet BallMedium = V2 7 7
areaBullet BallSmall = V2 4 4
areaBullet Oval = V2 7 3
areaBullet Diamond = V2 5 3
areaBullet BallFrame = V2 5 5
areaBullet Needle = V2 30 1
areaBullet BallTiny = V2 2 2

makeBullet :: (HasObject c, HasBulletObject c) => c -> c
makeBullet b = b & size .~ areaBullet (b^.kind)

instance GUIClass Player where
  update = do
    counter %= (+1)
    use spXY >>= \sp -> pos += sp
    spXY .= 0
    pos %= clamp

    spXY <~ liftM2 (\s k -> case k M.! KeyLeftShift > 0 || k M.! KeyRightShift > 0 of
        True -> ((0.5 * s) *^ dir k)
        False -> s *^ dir k) (use speed) (use keysPlayer)

    where
      dir :: M.Map Key Int -> Vec2
      dir k = let addTup b p q = bool q (uncurry V2 p+q) b in
        addTup (k M.! KeyUp    > 0) (0,-1) $
        addTup (k M.! KeyDown  > 0) (0,1) $
        addTup (k M.! KeyRight > 0) (1,0) $
        addTup (k M.! KeyLeft  > 0) (-1,0) $
        0

  paint = do
    p <- get
    let resource = given :: Resource
    draw $ translate (p^.pos) $ bitmap $ (resource^.charaImg) V.! 0

instance GUIClass Effect where
  update = do
    run <- use runAuto
    effectObject %= execState run
  
  paint = do
    b <- get
    let res = given :: Resource
    lift $ translate (b^.pos) $ rotateR (b^.ang) $ scale (b^.size) $ b^.img $ res

instance GUIClass Bullet where
  update = do
    r <- use speed
    t <- use ang
    pos += rotate2 (V2 r 0) t
    p <- use pos
    let config = given :: Config
    unless (p `isInside` (config^.validArea)) $ stateBullet .= Outside

  paint = do
    let res = given :: Resource
    V2 x y <- use size
    b <- get
    case x /= y of
      True -> draw $ translate (b^.pos) $ 
              rotateR (b^.ang + pi/2) $ bitmap $ getBulletBitmap (res^.bulletImg) (b^.kind) (b^.bcolor)
      False -> draw $ translate (b^.pos) $ bitmap $ getBulletBitmap (res^.bulletImg) (b^.kind) (b^.bcolor)

instance GUIClass Enemy where
  update = do
    sp <- use spXY
    pos %= (+sp)
    counter %= (+1)
    h <- use hp
    when (h <= 0) $ stateChara .= Dead
  
  paint = do
    let res = given :: Resource
    c <- get
    draw $ translate (c^.pos) $ bitmap $ (res^.charaImg) V.! 1

instance GUIClass Field where
  update = do
    counterF %= (+1)
    danmakuTitle .= ""
    
    collideObj
    deadEnemyEffects
    
    scanAutonomies enemy
    scanAutonomies bullets
    
    bullets %= S.filter (\b -> b^.stateBullet /= Outside) . fmap (execState update)
    enemy %= fmap (execState update) . S.filter (\e -> e^.stateChara /= Dead)
    player %= execState update
    effects %= IM.filter (\e -> e^.stateEffect /= Inactive) . fmap (execState update)
    
    where
      deadEnemies = S.filter ((== Dead) . (^.stateChara))
      
      deadEnemyEffects = do
        ds <- deadEnemies `fmap` use enemy
        F.forM_ ds $ \e -> do
          effects %= insertIM (effEnemyDead $ e^.pos)
          F.forM_ (e^.effectIndexes) $ \i ->
            effects %= IM.adjust (execState $ stateEffect .= Inactive) i
      
  paint = do
    drawEffs Background
    drawObj
    drawEffs OnObject
    use isDebug >>= \m -> when m $ debugging
    translate (V2 320 240) . bitmap $ resource ^. board
    drawMessages
    drawEffs Foreground
    use danmakuTitle >>= \t -> when (t /= "") drawTitle
    
    where
      resource = given :: Resource

      drawObj = do
        _ <- lift . execStateT paint =<< use player
        F.mapM_ (lift . execStateT paint) =<< use bullets
        F.mapM_ (lift . execStateT paint) =<< use enemy
      
      drawEffs z = do
        F.mapM_ (lift . execStateT paint) . IM.filter (\r -> r^.zIndex == z)
          =<< use effects
      
      debugging = do
        F.mapM_ (\b -> color blue . polygon $ 
                       boxVertexRotated (b^.pos) (b^.size) (b^.ang)) =<< use bullets
        _ <- (\p -> color yellow . polygon $ 
                    boxVertex (p^.pos) (p^.size)) =<< use player
        F.mapM_ (\e -> color green . polygon $ 
                       boxVertex (e^.pos) (e^.size)) =<< use enemy
      
      drawMessages = do
        let ls = resource ^. labels
        
        lift $ translate (V2 430 30) $ ls M.! "fps"
        drawScore 30 =<< getFPS

        lift $ translate (V2 430 50) $ ls M.! "score"
        drawScore 50 =<< use counterF

        lift $ translate (V2 430 70) $ ls M.! "hiscore"
        drawScore 70 (0 :: Int)

        lift $ translate (V2 430 90) $ ls M.! "hp"
        drawScore 90 =<< use (player.hp)

        lift $ translate (V2 430 170) $ ls M.! "bullets"
        drawScore 170 . S.length =<< use bullets

        lift $ translate (V2 430 190) $ ls M.! "enemies"
        drawScore 190 . S.length =<< use enemy

        lift $ translate (V2 430 210) $ ls M.! "effects"
        drawScore 210 . IM.size =<< use effects

      drawScore y sc = do
        forM_ (zip (show $ maximum [sc, 0]) [1..]) $ \(n, i) -> 
          when (n /= '-') $
            lift $ translate (V2 (550 + i*13) y) $ (resource^.numbers) V.! digitToInt n
      
      drawTitle = do
        translate (V2 40 30) . text (resource^.font) 10 =<< use danmakuTitle

keyList :: [Key]
keyList = [
  KeyUp, KeyDown, KeyRight, KeyLeft, KeyLeftShift, KeyRightShift,
  charToKey 'Z', charToKey 'X']

clamp :: (Given Config) => Vec2 -> Vec2
clamp (V2 x y) = V2 (edgeX x) (edgeY y)
  where
    config = given :: Config

    Box (V2 areaLeft areaTop) (V2 areaRight areaBottom) = config ^. gameArea

    edgeX = (\p -> bool p areaLeft (p < areaLeft)) .
            (\p -> bool p areaRight (p > areaRight))
    
    edgeY = (\p -> bool p areaTop (p < areaTop)) .
            (\p -> bool p areaBottom (p > areaBottom))

actPlayer :: StateT Player Game ()
actPlayer = do
  pairs <- lift $ mapM (\k -> (,) k `fmap` fromEnum `fmap` keyPress k) keyList
  keysPlayer %= M.unionWith go (M.fromList pairs)
  where
    go a b
      | a == 0 = 0
      | otherwise = a + b

runDanmaku :: c -> Field -> Danmaku c () -> Product (State c) (State Field) ()
runDanmaku = runLookAtAll

scanAutonomies :: Lens' Field (S.Seq (Autonomie (Danmaku a) a)) -> State Field ()
scanAutonomies member = do
  f <- use id
  let pairs = fmap (\c -> runDanmaku (c^.auto) f (c^.runAuto)) $ f^.member
  member .= (fmap (\(b,s) -> b & auto %~ execState s) $ 
             S.zip (f^.member) $ fmap (\(Pair a _) -> a) pairs)
  modify $ execState $ T.mapM (\(Pair _ b) -> b) pairs

collideObj :: (Given Resource) => State Field ()
collideObj = do
  p <- use player
  let run' = run createEffect
  
  (n, bs') <- runPair PlayerB p `fmap` use bullets
  player %= (hp -~ n)
  when (n>0) $ effects %= (insertIM $ effPlayerDead (p^.pos))
  
  (es', bs'', _) <- (\es -> return $ run' EnemyB es bs') =<< use enemy
  enemy .= es'
  bullets .= bs''
  -- effects %= (effEnemyDamaged ...)

  where
    runPair :: (HasChara c, HasObject c) => 
               StateBullet -> c -> S.Seq Bullet -> (Int, S.Seq Bullet)
    runPair s c bs = 
      let bs' = S.filter (\b -> s /= b^.stateBullet && collide c b) bs in
      (S.length bs', S.filter (\b -> (Nothing ==) $ b `S.elemIndexL` bs') bs)
    
    run :: (HasChara c, HasObject c) => 
           (StateBullet -> c -> Effect) -> StateBullet -> S.Seq c -> S.Seq Bullet -> 
           (S.Seq c, S.Seq Bullet, S.Seq Effect)
    run eff s cseq bss = iter (S.viewl cseq) (S.empty, bss, S.empty) where
      iter S.EmptyL acc = acc
      iter (h S.:< rest) (cs, bs, es) = iter (S.viewl rest) (cs', bs', es') where
        (n, bs') = runPair s h bs
        cs' = (h & hp -~ n) S.<| cs
        es' = if n>0 then es else es S.|> eff s h
    
    createEffect :: (HasChara c, HasObject c, Given Resource) => StateBullet -> c -> Effect
    createEffect PlayerB e = effPlayerDead (e^.pos)
    createEffect EnemyB e = effPlayerDead (e^.pos)
    createEffect _ _ = error "otherwise case in createEffect"

addBullet :: (Given Resource) => State Field ()
addBullet = do
  keys <- use (player.keysPlayer)
  cnt <- use (player.counter)
  when (keys M.! charToKey 'Z' > 0 && cnt `mod` 10 == 0) $ do
    s <- use (player.shotZ)
    p <- use (player.charaPlayer)
    bullets ><= evalState s p
  
  when (keys M.! charToKey 'X' > 0 && cnt `mod` 20 == 0) $ do
    s <- use (player.shotX)
    p <- use (player.charaPlayer)
    bullets ><= evalState s p

effPlayerDead :: (Given Resource) => Vec2 -> Effect
effPlayerDead = go . effCommonAnimated 1 where
  go :: Effect -> Effect
  go e = e & size .~ V2 0.8 0.8 & slowRate .~ 5 & runAuto %~ (>> size *= 1.01)

effEnemyDead :: (Given Resource) => Vec2 -> Effect
effEnemyDead = effCommonAnimated 0

effCommonAnimated :: (Given Resource) => Int -> Vec2 -> Effect
effCommonAnimated k p = def & pos .~ p & zIndex .~ OnObject & runAuto .~ run where
  run = do
    f <- get
    let i = (f^.counter) `div` (f^.slowRate)
    img .= \r -> bitmap $ (r^.effectImg) V.! k V.! i
    counter %= (+1)
    let resource = given :: Resource
    when (i == V.length ((resource^.effectImg) V.! k)) $ stateEffect .= Inactive

getBulletBitmap :: V.Vector (V.Vector Bitmap) -> BKind -> BColor -> Bitmap
getBulletBitmap imgs bk bc = imgs V.! (fromEnum bk) V.! (fromEnum bc)
