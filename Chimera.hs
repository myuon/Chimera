{-# LANGUAGE TemplateHaskell, RankNTypes #-}
module Chimera where

import Graphics.UI.FreeGame
import Control.Lens
import Control.Monad.State (execStateT)

import qualified Chimera.STG as STG
import Chimera.Load
import Data.Time (UTCTime, getCurrentTime, diffUTCTime)

makeLenses ''GUIParam

data GameFrame = GameFrame {
  _screenMode :: Int,
  _field :: STG.Field,
  _resource :: Resource,
  _prevTime :: UTCTime
  }

makeLenses ''GameFrame

initGameFrame :: GameFrame
initGameFrame = GameFrame {
  _screenMode = 0,
  _field = undefined,
  _resource = undefined,
  _prevTime = undefined
  }

start :: GUIParam
start =
  windowTitle .~ "Chimera" $
  clearColor .~ Color 0 0 0.2 1.0 $
  def

step :: Game Bool
step = do
  tick
  keySpecial KeyEsc

mainloop :: GameFrame -> Game GameFrame
mainloop gf = do
  time' <- embedIO getCurrentTime
  let fps' = getFPS $ diffUTCTime time' (gf ^. prevTime)

  STG.draw `execStateT` (gf ^. field)
  write 20 $ "fps:" ++ show fps'
  write 40 $ "bulletP:" ++ show (length $ gf ^. field ^. STG.bulletP)
  write 60 $ "bulletP:" ++ show (length $ gf ^. field ^. STG.bulletE)
  
  f' <- STG.update `execStateT` (gf ^. field)
  keys' <- STG.updateKeys (gf ^. field ^. STG.player ^. STG.keys)

  return $
    field .~ (STG.player . STG.keys .~ keys' $ f') $
    prevTime .~ time' $
    gf

  where  
    write :: Float -> String -> Game ()
    write y = translate (V2 0 y) .
               colored white .
               text (gf ^. resource ^. font) 20

    getFPS :: (RealFrac a, Fractional a) => a -> Int
    getFPS diff = floor $ 1 / diff

game :: IO (Maybe a)
game = runGame start $ do
  res' <- load
  time' <- embedIO getCurrentTime
  
  field' <- STG.loadStage `execStateT` STG.initField res'
  
  run $
    field .~ field' $
    resource .~ res' $
    prevTime .~ time' $
    initGameFrame
  quit
  
  where
    run :: GameFrame -> Game ()
    run gf = do
      gf' <- mainloop gf
      step >>= flip unless (run gf')
  
