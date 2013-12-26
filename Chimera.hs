{-# LANGUAGE TemplateHaskell #-}
module Chimera where

import Graphics.UI.FreeGame
import Control.Lens
import Control.Monad.State.Strict
import Data.Maybe (fromJust, isJust)
import qualified Data.Vector as V
import qualified Data.Sequence as S

import qualified Chimera.STG as STG
import Chimera.STG.Util
import Chimera.Menu

makeLenses ''GUIParam

type GameLoop = StateT GameFrame Game

data GameFrame = GameFrame {
  _running :: GameLoop (),
  _field :: STG.Field,
  _menu :: Select GameLoop,
  _font :: Font
  }

makeLenses ''GameFrame

start :: GUIParam
start =
  windowRegion .~ BoundingBox 0 0 640 480 $
  framePerSecond .~ 60 $
  windowTitle .~ "Chimera" $
  clearColor .~ Color 0 0 0.2 1.0 $
  def

menuloop :: GameLoop ()
menuloop = do
  menu' <- use menu
  font' <- use font
  (m, s) <- (lift $ selectloop font' `runStateT` menu')
  when (isJust m) $ running .= fromJust m
  menu .= s

loadloop :: GameLoop ()
loadloop = do
  font' <- use font
  field' <- use field
  lift $ STG.execLoad font' (field'^.STG.resource)
  running .= mainloop

mainloop :: GameLoop ()
mainloop = do
  gf <- get

  lift $ STG.draw undefined `execStateT` (gf ^. field)
  
  let write y = translate (V2 0 y) . colored white . text (gf ^. font) 20
  fps <- getFPS
  write 20 $ "fps:" ++ show fps
  write 40 $ "bullets:" ++ show (S.length $ gf ^. field ^. STG.bullets)
  write 60 $ "enemies:" ++ show (S.length $ gf ^. field ^. STG.enemy)
  
  field . STG.player . STG.keys <~ ((lift . STG.updateKeys) 
                                    =<< (use $ field . STG.player . STG.keys))
  field %= execState STG.update
  
game :: IO (Maybe a)
game = runGame start $ do
  font' <- embedIO $ loadFont "data/font/VL-PGothic-Regular.ttf"
  let field' = STG.loadStage $ def & STG.resource .~ def
  
  let its = V.fromList [Item "Game Start" loadloop,
                        Item "Quit" quit]
  
  flip evalStateT (GameFrame {
                      _font = font',
                      _field = field',
                      _menu = def & items .~ its,
                      _running = menuloop }) $ foreverTick $ do
    go <- use running
    go
    
    when_ (keySpecial KeyEsc) $ quit
