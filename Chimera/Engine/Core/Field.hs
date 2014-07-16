{-# LANGUAGE TemplateHaskell, Rank2Types, FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Chimera.Engine.Core.Field where

import FreeGame
import Control.Lens
import Control.Arrow
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

data GroupFlag = GPlayer | GEnemy | None | All deriving (Eq, Show)
data ZIndex = Invisible | Background | OnObject | Foreground deriving (Eq, Show)
data StatePiece = Standby | Alive | Attack | Damaged | Dead deriving (Eq, Show)
data BKind = BallLarge | BallMedium | BallSmall | BallFrame | BallTiny |
             Oval | Diamond | Needle deriving (Eq, Ord, Enum, Show)
data BColor = Red | Orange | Yellow | Green | Cyan | Blue | Purple | Magenta
              deriving (Eq, Ord, Enum, Show)

data Piece = Piece {
  _objectPiece :: Object,
  _group :: GroupFlag,
  _drawing :: Game (),
  _statePiece :: StatePiece,
  _scaleRate :: Double
  }

data EffectPiece = EffectPiece {
  _pieceEffect :: Piece,
  _zIndex :: ZIndex,
  _slowRate :: Int
  }

data Chara = Chara {
  _pieceChara :: Piece,
  _hp :: Int,
  _effectIndexes :: S.Seq Int
  }

data Player = Player {
  _charaPlayer :: Chara,
  _keysPlayer :: M.Map Key Int,
  _shotZ :: (Given Resource) => State Chara (S.Seq Bullet),
  _shotX :: (Given Resource) => State Chara (S.Seq Bullet),
  _bombCount :: Int
  }

data Field = Field {
  _player :: Player,
  _enemies :: S.Seq Enemy,
  _bullets :: S.Seq Bullet,
  _effects :: IM.IntMap Effect,
  _counterF :: Int,
  _isDebug :: Bool,
  _danmakuTitle :: String
  }

type Danmaku c = LookAt c Field
type Component a = Autonomie (Danmaku a) a
type Bullet = Component Piece
type Enemy = Component Chara
type Effect = Component EffectPiece

makeClassy ''Chara
makeClassy ''Piece
makeClassy ''EffectPiece
makeLenses ''Player
makeLenses ''Field

instance HasObject Piece where object = objectPiece
instance HasPiece Chara where piece = pieceChara
instance HasObject Chara where object = piece . object
instance HasPiece EffectPiece where piece = pieceEffect
instance HasObject EffectPiece where object = piece . object
instance HasChara Player where chara = charaPlayer
instance HasPiece Player where piece = charaPlayer . piece
instance HasObject Player where object = charaPlayer . object
instance HasObject Bullet where object = auto . object
instance HasPiece Bullet where piece = auto
instance HasObject Enemy where object = auto . object
instance HasPiece Enemy where piece = auto . piece
instance HasChara Enemy where chara = auto
instance HasObject Effect where object = auto . object
instance HasPiece Effect where piece = auto . piece
instance HasEffectPiece Effect where effectPiece = auto

instance Default Piece where
  def = Piece def None (return ()) Alive 1.0

instance Default EffectPiece where
  def = EffectPiece def Background 3

instance Default Chara where
  def = Chara def 10 S.empty

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
    _shotX = error "uninitialized shotX",
    _bombCount = error "uninitialized bombCount"
    }

instance Default Field where
  def = Field def S.empty S.empty IM.empty 0 False ""

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
        addTup (k M.! KeyLeft  > 0) (-1,0) $ 0

  paint = do
    p <- get
    let resource = given :: Resource
    draw $ translate (p^.pos) $ bitmap $ (resource^.charaImg) V.! 0

instance (HasPiece (Component a), HasObject (Component a)) =>
         GUIClass (Component a) where
  update = do
    r <- use speed
    t <- use ang
    pos += rotate2 (V2 r 0) t

    sp <- use spXY
    pos += sp

    counter %= (+1)

    let config = given :: Config
    use pos >>= \p -> unless (p `isInside` (config^.validArea)) $ do
      use statePiece >>= \s -> when (s /= Standby) $ do
        statePiece .= Dead

  paint = do
    b <- get
    V2 x y <- use size
    case x /= y of
      True -> lift $ translate (b^.pos) $
        scale (V2 (b^.scaleRate) (b^.scaleRate)) $ rotateR (b^.ang + pi/2) $ b^.drawing
      False -> lift $ translate (b^.pos) $
        scale (V2 (b^.scaleRate) (b^.scaleRate)) $ b^.drawing

instance GUIClass Field where
  update = do
    counterF %= (+1)
    danmakuTitle .= ""

    collideObj
    deadEnemyEffects
    damagedEffects
    
    scanAutonomies enemies
    scanAutonomies bullets
    scanAutonomies effects

    enemies %= S.filter (\p -> p^.statePiece /= Dead) . fmap (execState update)
    bullets %= S.filter (\p -> p^.statePiece /= Dead) . fmap (execState update)
    effects %= IM.filter (\p -> p^.statePiece /= Dead) . fmap (execState update)
    player %= execState update
    
    where
      deadEnemyEffects = do
        ds <- S.filter (\p -> p^.statePiece == Dead) `fmap` use enemies
        F.forM_ ds $ \e -> do
          effects %= insertIM (effEnemyDead $ e^.pos)
          F.forM_ (e^.effectIndexes) $ \i ->
            effects %= IM.adjust (execState $ statePiece .= Dead) i

      damagedEffects = do
        {-
        ds <- S.filter (\p -> p^.statePiece == Damaged) `fmap` use enemies
        F.forM_ ds $ \e -> do
          effects %= insertIM (effPlayerDead $ e^.pos)
        enemies %= fmap (statePiece .~ Attack)
        -}

        p <- use player
        when (p^.statePiece == Damaged) $
          effects %= insertIM (effPlayerDead $ p^.pos)
        player %= (statePiece .~ Attack)

  paint = do
    drawEffs Background
    drawObj
    drawEffs OnObject
    translate (V2 320 240) . bitmap $ resource ^. board
    drawMessages
    drawEffs Foreground
    use danmakuTitle >>= \t -> when (t /= "") drawTitle
    use isDebug >>= \m -> when m $ debugging
    
    where
      resource = given :: Resource

      drawObj = do
        _ <- lift . execStateT paint =<< use player
        F.mapM_ (lift . execStateT paint) =<< use bullets
        F.mapM_ (lift . execStateT paint) =<< use enemies
      
      drawEffs z = do
        F.mapM_ (lift . execStateT paint) . IM.filter (\r -> r^.zIndex == z)
          =<< use effects
      
      debugging = do
        F.mapM_ (\b -> color blue . polygon $ 
                       boxVertexRotated (b^.pos) (b^.size) (b^.ang)) =<< use bullets
        _ <- (\p -> color yellow . polygon $ 
                    boxVertex (p^.pos) (p^.size)) =<< use player
        F.mapM_ (\e -> color green . polygon $ 
                       boxVertex (e^.pos) (e^.size)) =<< use enemies
      
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
        drawScore 190 . S.length =<< use enemies

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

scanAutonomies :: (Traversable f) => Lens' Field (f (Autonomie (Danmaku a) a)) -> State Field ()
scanAutonomies member = do
  f <- use id
  let pairs = fmap (runEach f) $ f^.member
  member .= (fmap (\(a, Pair s _) -> a & auto %~ execState s) pairs)
  modify $ execState $ T.mapM (\(_, Pair _ t) -> t) pairs

  where
    runEach :: Field -> Autonomie (Danmaku a) a -> (Autonomie (Danmaku a) a, Product (State a) (State Field) ())
    runEach f c = (,) c $ runDanmaku (c^.auto) f (c^.runAuto)

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

makeBullet :: (Given Resource, HasPiece c, HasObject c) => BKind -> BColor -> c -> c
makeBullet bk bc b = let resource = given :: Resource in b
  & size .~ areaBullet bk & group .~ GEnemy
  & drawing .~ (bitmap $ (resource^.bulletImg) V.! (fromEnum bk) V.! (fromEnum bc))

collideObj :: (Given Resource) => State Field ()
collideObj = do
  es <- use enemies
  bs <- use bullets
  let (es',bs') = collideSeq GPlayer es bs
  enemies .= es'

  p <- use player
  let (p',bs'') = collideSeq GEnemy (S.singleton p) bs'
  player .= S.index p' 0
  bullets .= bs''

  where
    collideTo :: (HasPiece e, HasObject e) =>
                 GroupFlag -> e -> S.Seq Bullet -> (Int, S.Seq Bullet)
    collideTo flag e = first S.length . S.partition (\b -> collide e b && (b^.group) == flag)

    damage :: (HasPiece c, HasChara c) => (StatePiece -> StatePiece) -> c -> Int -> c
    damage s e n
      | n == 0 = e
      | otherwise = let e' = e & hp -~ n in
      case e'^.hp > 0 of
        True -> e' & statePiece %~ s
        False -> e' & statePiece .~ Dead

    collideSeq :: (HasPiece e, HasChara e, HasObject e) => 
                  GroupFlag -> S.Seq e -> S.Seq Bullet -> (S.Seq e, S.Seq Bullet)
    collideSeq flag es bs = F.foldl k (S.empty, bs) es where
      k (es', bs') e = let (dmg, bsc) = collideTo flag e bs' in case flag of
        GPlayer -> (es' S.|> damage id e dmg, bsc)
        _ -> (es' S.|> damage (const Damaged) e dmg, bsc)

addBullet :: (Given Resource) => State Field ()
addBullet = do
  keys <- use (player.keysPlayer)
  cnt <- use (player.counter)
  when (keys M.! charToKey 'Z' > 0 && cnt `mod` 10 == 0) $ do
    s <- use (player.shotZ)
    p <- use (player.charaPlayer)
    bullets ><= evalState s p

  n <- use (player.bombCount)
  when (keys M.! charToKey 'X' > 0 && cnt `mod` 20 == 0 && n > 0) $ do
    s <- use (player.shotX)
    p <- use (player.charaPlayer)
    bullets ><= evalState s p
    zoom player $ bombCount -= 1

effPlayerDead :: (Given Resource) => Vec2 -> Effect
effPlayerDead = go . effCommonAnimated 1 where
  go :: Effect -> Effect
  go e = e & size .~ V2 0.8 0.8 & slowRate .~ 5 & runAuto %~ (>> (hook $ Left $ size *= 1.01))

effEnemyDead :: (Given Resource) => Vec2 -> Effect
effEnemyDead = effCommonAnimated 0

effCommonAnimated :: (Given Resource) => Int -> Vec2 -> Effect
effCommonAnimated k p = def & pos .~ p & zIndex .~ OnObject & runAuto .~ run where
  run = hook $ Left $ do
    let resource = given :: Resource
    f <- get
    let i = (f^.counter) `div` (f^.slowRate)
    drawing .= (bitmap $ (resource^.effectImg) V.! k V.! i)
    when (i == V.length ((resource^.effectImg) V.! k)) $ statePiece .= Dead
