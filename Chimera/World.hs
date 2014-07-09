{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts, RankNTypes #-}
module Chimera.World (
  actPlayer, runDanmaku, scanAutonomies, addBullet
  , module M
  ) where

import FreeGame
import Control.Lens
import Control.Monad.State.Strict
import Data.Char (digitToInt)
import Data.Default (def)
import qualified Data.Vector as V
import qualified Data.Sequence as S
import qualified Data.Map as M
import qualified Data.Traversable as T
import qualified Data.IntMap.Strict as IM
import qualified Data.Foldable as F
import Data.Functor.Product
import Data.Reflection (Given, given)

import Chimera.Core.Types as M
import Chimera.Core.Util as M
import Chimera.Core.Layers as M
import Chimera.Core.Menu as M
import Chimera.Scripts as M
import Chimera.Scripts.Common as M

instance GUIClass Player where
  update = do
    counter %= (+1)
    do
      sp <- use spXY
      pos += sp
    spXY .= 0
    pos %= clamp

    do
      s <- use speed
      k <- use keysPlayer
      spXY .= case k M.! KeyLeftShift > 0 || k M.! KeyRightShift > 0 of
        True -> ((0.5 * s) *^ dir k)
        False -> s *^ dir k

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

    where
      getBulletBitmap :: V.Vector (V.Vector Bitmap) -> BKind -> BColor -> Bitmap
      getBulletBitmap imgs bk bc = imgs V.! (fromEnum bk) V.! (fromEnum bc)

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
    when_ (use isDebug) debugging
    translate (V2 320 240) . bitmap $ resource ^. board
    drawMessages
    drawEffs Foreground
    when_ ((/= "") `fmap` use danmakuTitle) drawTitle
    
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
