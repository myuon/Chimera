{-# LANGUAGE TemplateHaskell #-}
module Chimera where

import Graphics.UI.FreeGame
import Control.Lens
import Control.Monad.State.Strict
import qualified Data.Sequence as S
import Data.Default

import qualified Chimera.STG as STG
import Data.Time (UTCTime, getCurrentTime, diffUTCTime)

makeLenses ''GUIParam

data GameFrame = GameFrame {
  _screenMode :: Int,
  _field :: STG.Field,
  _font :: Font,
  _prevTime :: UTCTime
  }

makeLenses ''GameFrame

start :: GUIParam
start =
  windowRegion .~ BoundingBox 0 0 640 480 $
  framePerSecond .~ 60 $
  windowTitle .~ "Chimera" $
  clearColor .~ Color 0 0 0.2 1.0 $
  def

mainloop :: StateT GameFrame Game ()
mainloop = do
  gf <- get
  time' <- embedIO getCurrentTime
  prevTime .= time'
  let fps' = getFPS $ diffUTCTime time' (gf ^. prevTime)

  lift $ STG.draw undefined `execStateT` (gf ^. field)
  
  let write y = translate (V2 0 y) . colored white . text (gf ^. font) 20
  write 20 $ "fps:" ++ show fps'
  write 40 $ "bullets:" ++ show (S.length $ gf ^. field ^. STG.bullets)
  write 60 $ "enemies:" ++ show (S.length $ gf ^. field ^. STG.enemy)
  
  keys <- use $ field . STG.player . STG.keys
  field . STG.player . STG.keys <~ (lift $ STG.updateKeys keys)
  field %= execState STG.update
  
  where  
    getFPS :: (RealFrac a, Fractional a) => a -> Int
    getFPS diff = floor $ 1 / diff

game :: IO (Maybe a)
game = runGame start $ do
  font' <- embedIO $ loadFont "data/font/VL-PGothic-Regular.ttf"
  time' <- embedIO getCurrentTime
  let field' = STG.loadStage $ def & STG.resource .~ def
  
  STG.execLoad font' (field'^.STG.resource)
  
  let frame = GameFrame {
        _screenMode = 0,
        _field = field',
        _font = font',
        _prevTime = time'
        }
    
  flip evalStateT frame $ foreverTick $ do
    mainloop
    
    esc <- keySpecial KeyEsc
    when esc $ quit

