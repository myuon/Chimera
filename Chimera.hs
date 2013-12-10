{-# LANGUAGE TemplateHaskell, RankNTypes #-}
module Chimera where

import Graphics.UI.FreeGame
import Control.Lens
import Control.Monad.State.Strict (execState, execStateT)
import qualified Data.Sequence as S
import Data.Default

import qualified Chimera.STG as STG
import Chimera.Load
import Data.Time (UTCTime, getCurrentTime, diffUTCTime)

makeLenses ''GUIParam

data GameFrame = GameFrame {
  _screenMode :: Int,
  _field :: STG.Field,
  _font :: Font,
  _prevTime :: UTCTime
  }

makeLenses ''GameFrame

initGameFrame :: GameFrame
initGameFrame = GameFrame {
  _screenMode = 0,
  _field = undefined,
  _font = undefined,
  _prevTime = undefined
  }

start :: GUIParam
start =
  windowRegion .~ BoundingBox 0 0 640 480 $
  framePerSecond .~ 60 $
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
  write 40 $ "bulletE:" ++ show (S.length $ gf ^. field ^. STG.bulletE)
  
  let f' = STG.update `execState` (gf ^. field)
  keys' <- STG.updateKeys (gf ^. field ^. STG.player ^. STG.keys)
  
  return $
    field .~ (STG.player . STG.keys .~ keys' $ f') $
    prevTime .~ time' $
    gf

  where  
    write :: Float -> String -> Game ()
    write y = translate (V2 0 y) . colored white . text (gf ^. font) 20

    getFPS :: (RealFrac a, Fractional a) => a -> Int
    getFPS diff = floor $ 1 / diff

game :: IO (Maybe a)
game = runGame start $ do
  font' <- embedIO $ loadFont "data/font/VL-PGothic-Regular.ttf"
  time' <- embedIO getCurrentTime
  let field' = STG.loadStage (STG.isDebug .~ True $ def)
  
  step
  translate (V2 30 30) . colored white . text (font') 20 $ "読み込み中…"
  step
  execLoad font' (field'^.STG.resource)
  
  run $
    field .~ field' $
    font .~ font' $
    prevTime .~ time' $
    initGameFrame
  quit
  
  where
    run :: GameFrame -> Game ()
    run gf = do
      gf' <- mainloop gf
      step >>= flip unless (run gf')
  
