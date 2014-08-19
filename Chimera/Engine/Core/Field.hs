{-# LANGUAGE TemplateHaskell, Rank2Types, FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances, ConstraintKinds, MultiWayIf #-}
module Chimera.Engine.Core.Field where

import FreeGame
import FreeGame.Class (ButtonState(..))
import Control.Lens
import qualified Data.Vector as V
import qualified Data.Map as M
import qualified Data.IntMap.Strict as IM
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import Data.Reflection
import Data.Default
import Data.Char (digitToInt)

import Chimera.State
import Chimera.Engine.Core.Util
import Chimera.Engine.Core.Types

data GroupFlag = GPlayer | GEnemy | None deriving (Eq, Show)
data ZIndex = Invisible | Background | OnObject | Foreground deriving (Eq, Show)
data StatePiece = Standby | Alive | Attack | Damaged | Dead deriving (Eq, Show)

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
  _effectIndexes :: [Int]
  }

data Player = Player {
  _charaPlayer :: Chara,
  _shotZ :: (Given Resource) => State Chara [Bullet],
  _shotX :: (Given Resource) => State Chara [Bullet],
  _bombCount :: Int,
  _cshots :: [Bullet]
  }

data Field = Field {
  _player :: Player,
  _enemies :: IM.IntMap Enemy,
  _bullets :: IM.IntMap Bullet,
  _effects :: IM.IntMap Effect,
  _counterF :: Int,
  _isDebug :: Bool,
  _danmakuTitle :: String,
  _sceneEffects :: [Int]
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

type EnemyLike c = (HasObject c, HasChara c, HasPiece c)

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
  def = Piece def None (return ()) Standby 1.0

instance Default EffectPiece where
  def = EffectPiece def Background 3

instance Default Chara where
  def = Chara def 10 []

instance Default Player where
  def = Player {
    _charaPlayer =
      pos .~ V2 320 420 $ 
      speed .~ 2.5 $
      size .~ V2 5 5 $
      hp .~ 10 $
      def,
    _shotZ = error "uninitialized shotZ",
    _shotX = error "uninitialized shotX",
    _bombCount = error "uninitialized bombCount",
    _cshots = []
    }

instance Default Field where
  def = Field def IM.empty IM.empty IM.empty 0 False "" [] where

instance GUIClass Player where
  update = do
    counter %= (+1)
    use spXY >>= \sp -> pos += sp
    spXY .= 0
    pos %= clamp

    dir <- keyStates_ >>= \keys -> return $ sum $ [v | (k,v) <-
        [(KeyUp, V2 0 (-1)),(KeyDown, V2 0 1),(KeyRight,V2 1 0),(KeyLeft,V2 (-1) 0)],
        case keys M.! k of Press -> True; _ -> False]
    shift <- (||) <$> keyPress KeyLeftShift <*> keyPress KeyRightShift
    s <- use speed
    spXY .= case shift of
      True -> (0.5 * s) *^ (dir :: Vec2)
      False -> s *^ dir
    addBullet

    where
      addBullet = do
        cnt <- use counter
        cshots .= []

        z <- keyPress (charToKey 'Z')
        when (z && cnt `mod` 10 == 0) $ do
          s <- use shotZ
          p <- use charaPlayer
          cshots .= evalState s p

        n <- use bombCount
        x <- keyPress (charToKey 'X')
        when (x && cnt `mod` 20 == 0 && n > 0) $ do
          s <- use shotX
          p <- use charaPlayer
          cshots .= evalState s p
          bombCount -= 1

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

    counter += 1

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
    counterF += 1
    danmakuTitle .= ""

    collideObj
    deadEnemyEffects
    damagedEffects
    
    scanAutonomies enemies
    scanAutonomies bullets
    scanAutonomies effects

    enemies <=~ T.mapM (\x -> lift . execStateT update $! x) . IM.filter (\p -> p^.statePiece /= Dead)
    bullets <=~ T.mapM (\x -> lift . execStateT update $! x) . IM.filter (\p -> p^.statePiece /= Dead)
    effects <=~ T.mapM (\x -> lift . execStateT update $! x) . IM.filter (\p -> p^.statePiece /= Dead)
    player <=~ \x -> lift . execStateT update $! x
    cs <- use (player.cshots)
    bullets %= insertsIM' cs
    
    where
      deadEnemyEffects = do
        ds <- IM.filter (\p -> p^.statePiece == Dead) `fmap` use enemies
        F.forM_ ds $ \e -> do
          effects %= insertIM (effEnemyDead $ e^.pos)
          F.forM_ (e^.effectIndexes) $ \i ->
            effects %= IM.adjust (execState $ statePiece .= Dead) i

      damagedEffects = do
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
        F.mapM_ (\x -> lift . execStateT paint $! x) =<< use bullets
        F.mapM_ (\x -> lift . execStateT paint $! x) =<< use enemies
      
      drawEffs z = do
        F.mapM_ (\x -> lift . execStateT paint $! x) . IM.filter (\r -> r^.zIndex == z)
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
        drawScore 170 . IM.size =<< use bullets

        lift $ translate (V2 430 190) $ ls M.! "enemies"
        drawScore 190 . IM.size =<< use enemies

        lift $ translate (V2 430 210) $ ls M.! "effects"
        drawScore 210 . IM.size =<< use effects

      drawScore y sc = do
        forM_ (zip (show $ maximum [sc, 0]) [1..]) $ \(n, i) -> 
          when (n /= '-') $
            lift $ translate (V2 (550 + i*13) y) $ (resource^.numbers) V.! digitToInt n
      
      drawTitle = do
        translate (V2 40 30) . text (resource^.font) 10 =<< use danmakuTitle

clamp :: (Given Config) => Vec2 -> Vec2
clamp (V2 x y) = V2 (edgeX x) (edgeY y)
  where
    config = given :: Config

    Box (V2 areaLeft areaTop) (V2 areaRight areaBottom) = config ^. gameArea

    edgeX = (\p -> bool p areaLeft (p < areaLeft)) .
            (\p -> bool p areaRight (p > areaRight))
    
    edgeY = (\p -> bool p areaTop (p < areaTop)) .
            (\p -> bool p areaBottom (p > areaBottom))

scanAutonomies :: (Monad m) => Lens' Field (IM.IntMap (Component a)) -> StateT Field m ()
scanAutonomies member = do
  put =<< liftM2 (IM.foldrWithKey' iter) get (use member)
  put =<< liftM2 (IM.foldr' iter2) get (use member)
  where
    iter k a f = let (aut,_) = execState (a^.runAuto) (a^.auto,f) in 
      f & member %~ IM.adjust (auto .~ aut) k

    iter2 a f = let (_,f') = execState (a^.runAuto) (a^.auto,f) in f'

collide :: (HasObject c, HasObject b) => c -> b -> Bool
collide oc ob = let oc' = extend oc; ob' = extend ob; in
  detect ob' oc' || detect oc' ob'
  where
    extend :: (HasObject c) => c -> c
    extend x = x & size -~ V2 (x^.speed) 0 + (x^.spXY)

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

makeBullet :: (Given Resource, HasPiece c, HasObject c) => BKind -> BColor -> c -> c
makeBullet bk bc b = let resource = given :: Resource in b
  & size .~ (resource^.areaBullet) bk & group .~ GEnemy & statePiece .~ Alive
  & drawing .~ (bitmap $ (resource^.bulletImg) V.! (fromEnum bk) V.! (fromEnum bc))

collideObj :: (Given Resource, Monad m) => StateT Field m ()
collideObj = do
  es <- use enemies
  bs <- use bullets
  let (es',bs') = collides GPlayer es bs
  enemies .= es'

  p <- use player
  let (p',bs'') = collideTo GEnemy p bs'
  player .= p'
  bullets .= bs''

  where
    collides :: (EnemyLike e) =>
                GroupFlag -> IM.IntMap e -> IM.IntMap Bullet -> (IM.IntMap e, IM.IntMap Bullet)
    collides flag es bs = IM.foldrWithKey' f (es,bs) es where
      f k e (xs,ys) = let (e',ys') = collideTo flag e ys in (IM.insert k (hpCheck e') xs,ys')
      hpCheck x = if (x^.hp) <= 0 then x & statePiece .~ Dead else x

    collideTo :: (EnemyLike e) =>
                 GroupFlag -> e -> IM.IntMap Bullet -> (e, IM.IntMap Bullet)
    collideTo flag e bs = IM.foldrWithKey' f (e,bs) bs where
      f k b (x,ys) = if (b^.group) == flag && collide e b
        then (x & hp -~ 1 & statePiece %~ if flag == GEnemy then const Damaged else id, IM.adjust (statePiece .~ Dead) k ys)
        else (x,ys)

effPlayerDead :: (Given Resource) => Vec2 -> Effect
effPlayerDead = go . effCommonAnimated 1 where
  go :: Effect -> Effect
  go e = e & size .~ V2 0.8 0.8 & slowRate .~ 5 & runAuto %~ (>> (zoom self $ size *= 1.01))

effEnemyDead :: (Given Resource) => Vec2 -> Effect
effEnemyDead = effCommonAnimated 0

effCommonAnimated :: (Given Resource) => Int -> Vec2 -> Effect
effCommonAnimated k p = def & pos .~ p & zIndex .~ OnObject & runAuto .~ run where
  run :: Danmaku EffectPiece ()
  run = zoom self $ do
    let resource = given :: Resource
    i <- liftM2 div (use counter) (use slowRate)
    drawing .= (bitmap $ (resource^.effectImg) V.! k V.! i)
    when (i == V.length ((resource^.effectImg) V.! k)) $ statePiece .= Dead
