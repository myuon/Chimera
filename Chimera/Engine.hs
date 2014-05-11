module Chimera.Engine ( module M, addBullet ) where

import FreeGame
import Control.Lens
import Control.Monad.State.Strict (lift, execStateT, execState, State)
import Data.Char (digitToInt)
import Data.Default (def)
import qualified Data.Sequence as S
import qualified Data.Vector as V
import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.Foldable as F

import Chimera.Core.Load as M
import Chimera.Core.World as M
import Chimera.Scripts as M
import Chimera.Scripts.Common

instance GUIClass Field where
  update = do
    counterF %= (+1)
    danmakuTitle .= ""
    
    collideObj
    use resource >>= deadEnemyEffects
    
    scanAutonomies enemy
    scanAutonomies bullets
    
    bullets %= S.filter (\b -> b^.stateBullet /= Outside) . fmap (execState update)
    enemy %= fmap (execState update) . S.filter (\e -> e^.stateChara /= Dead)
    player %= execState update
    effects %= IM.filter (\e -> e^.stateEffect /= Inactive) . fmap (execState update)
    
    where
      deadEnemies = S.filter ((== Dead) . (^.stateChara))
      
      deadEnemyEffects res = do
        ds <- deadEnemies `fmap` use enemy
        F.forM_ ds $ \e -> do
          effects %= insertIM (effEnemyDead res $ e^.pos)
          F.forM_ (e^.effectIndexes) $ \i ->
            effects %= IM.adjust (execState $ stateEffect .= Inactive) i
      
  paint _ = do
    drawEffs Background
    drawObj
    drawEffs OnObject
    when_ (use isDebug) debugging
    translate (V2 320 240) . bitmap =<< use (resource.board)
    drawMessages
    drawEffs Foreground
    when_ ((/= "") `fmap` use danmakuTitle) drawTitle
    
    where
      drawObj = do
        res <- use resource
        _ <- lift . execStateT (paint res) =<< use player
        F.mapM_ (lift . execStateT (paint res)) =<< use bullets
        F.mapM_ (lift . execStateT (paint res)) =<< use enemy
      
      drawEffs z = do
        res <- use resource
        F.mapM_ (lift . execStateT (paint res)) . IM.filter (\r -> r^.zIndex == z)
          =<< use effects
      
      debugging = do
        F.mapM_ (\b -> color blue . polygon $ 
                       boxVertexRotated (b^.pos) (b^.size) (b^.ang)) =<< use bullets
        _ <- (\p -> color yellow . polygon $ 
                    boxVertex (p^.pos) (p^.size)) =<< use player
        F.mapM_ (\e -> color green . polygon $ 
                       boxVertex (e^.pos) (e^.size)) =<< use enemy
      
      drawMessages = do
        ls <- use (resource.labels)
        
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
        nums <- use (resource.numbers)
        forM_ (zip (show $ maximum [sc, 0]) [1..]) $ \(n, i) -> 
          when (n /= '-') $
            lift $ translate (V2 (550 + i*13) y) $ nums V.! digitToInt n
      
      drawTitle = do
        font' <- use (resource.font)
        translate (V2 40 30) . text font' 10 =<< use danmakuTitle

collideObj :: State Field ()
collideObj = do
  p <- use player
  res <- use resource
  let run' = run (createEffect res)
  
  (n, bs') <- runPair PlayerB p `fmap` use bullets
  player %= (hp -~ n)
  when (n>0) $ effects %= (insertIM $ effPlayerDead res (p^.pos))
  
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
    run eff s cseq bs = iter (S.viewl cseq) (S.empty, bs, S.empty) where
      iter S.EmptyL acc = acc
      iter (h S.:< rest) (cs, bs, es) = iter (S.viewl rest) (cs', bs', es') where
        (n, bs') = runPair s h bs
        cs' = (h & hp -~ n) S.<| cs
        es' = if n>0 then es else es S.|> eff s h
    
    createEffect :: (HasChara c, HasObject c) => 
                    Resource -> StateBullet -> c -> Effect
    createEffect res PlayerB e = effPlayerDead res (e^.pos)
    createEffect res EnemyB e = effPlayerDead res (e^.pos)
    createEffect _ _ _ = error "otherwise case in createEffect"

addBullet :: State Field ()
addBullet = do
  keys <- use (player.keysPlayer)
  cnt <- use (player.counter)
  when (keys M.! charToKey 'Z' > 0 && cnt `mod` 10 == 0) $ do
    p <- use (player.pos)
    bullets ><= (S.fromList
      [def' & pos .~ p + V2 5 0,
       def' & pos .~ p + V2 15 0,
       def' & pos .~ p - V2 5 0,
       def' & pos .~ p - V2 15 0])
  
  when (keys M.! charToKey 'X' > 0 && cnt `mod` 20 == 0) $ do
    p <- use (player.pos)
    res <- use resource
    bullets ><= (S.singleton $ chaosBomb res $ p)
  
  where
    def' :: Bullet
    def' = 
      makeBullet $
      speed .~ 15 $
      ang .~ pi/2 $ 
      kind .~ Diamond $
      bcolor .~ Red $
      stateBullet .~ PlayerB $
      def
