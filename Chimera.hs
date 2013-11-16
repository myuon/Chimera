{-# LANGUAGE TemplateHaskell, RankNTypes #-}
module Chimera where

import Graphics.UI.FreeGame
import Control.Lens
--import Control.Monad.State (evalStateT, execState)
import Control.Monad.State

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
  time' <- embedIO $ getCurrentTime
  let fps' = getFPS $ diffUTCTime time' (gf ^. prevTime)

  STG.draw `evalStateT` (gf ^. field)
  writeFPS $ "fps:" ++ (show $ fps')
  
  f' <- STG.update `execStateT` (gf ^. field)
  keys' <- STG.updateKeys (gf ^. field ^. STG.player ^. STG.keys)

  return $
    field .~ (STG.player . STG.keys .~ keys' $ f') $
    prevTime .~ time' $
    gf

  where
    writeFPS :: String -> Game ()
    writeFPS = translate (V2 0 20) .
               colored white .
               text (gf ^. resource ^. font) 20

    getFPS :: (RealFrac a, Fractional a) => a -> Int
    getFPS diff = floor $ (1 / diff)

main :: IO (Maybe a)
main = runGame start $ do
  res' <- load
  time' <- embedIO $ getCurrentTime
  
  run $
    field .~ STG.initField res' $
    resource .~ res' $
    prevTime .~ time' $
    initGameFrame
  quit
  
  where
    run :: GameFrame -> Game ()
    run gf = do
      gf' <- mainloop gf
      step >>= flip unless (run gf')
  
