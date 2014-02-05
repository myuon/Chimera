{-# LANGUAGE TemplateHaskell, GADTs #-}
module Chimera where

import Graphics.UI.FreeGame
import Control.Lens
import Control.Monad.State.Strict
import Control.Monad.Operational.Mini
import Data.Maybe (fromJust, isJust)
import qualified Data.Vector as V
import qualified Data.Sequence as S
import Control.Concurrent

import Chimera.Engine
import Chimera.Menu
import Chimera.Scripts.Stage1

makeLenses ''GUIParam

type GameLoop = StateT GameFrame Game

data GameFrame = GameFrame {
  _running :: GameLoop (),
  _stage :: Stage (),
  _field :: Field,
  _menu :: Select GameLoop,
  _mEngine :: MessageEngine
  }

makeLenses ''GameFrame

window :: GUIParam
window =
  windowRegion .~ BoundingBox 0 0 640 480 $
  framePerSecond .~ 60 $
  windowTitle .~ "Chimera" $
  clearColor .~ Color 0 0 0.2 1.0 $
  def

menuloop :: GameLoop ()
menuloop = do
  menu' <- use menu
  font' <- use (field.resource.font)
  (m, s) <- (lift $ selectloop font' `runStateT` menu')
  when (isJust m) $ running .= fromJust m
  menu .= s

loadloop :: GameLoop ()
loadloop = do
  w <- embedIO $ forkIO $ waiting 0
  field.resource <~ (lift . execLoad =<< use (field.resource))
  embedIO $ killThread w

  running .= stgloop
  where
    waiting :: Int -> IO ()
    waiting n = do
      putStrLn $ "Loading.." ++ replicate n '.'
--      translate (V2 30 30) . colored white . text (font') 20 $ "読み込み中…"
      threadDelay 1000000
      waiting (n+1)

stgloop :: GameLoop ()
stgloop = do
  gf <- get
  
  lift $ draw undefined `execStateT` (gf ^. field)
  
  font' <- use (field.resource.font)
  let write y = translate (V2 0 y) . colored white . text font' 20
  fps <- getFPS
  write 20 $ "fps:" ++ show fps
  write 40 $ "bullets:" ++ show (S.length $ gf^.field^.bullets)
  write 60 $ "enemies:" ++ show (S.length $ gf^.field^.enemy)
  write 80 $ "effects:" ++ show (S.length $ gf^.field^.effects)
  
  field.player.keys <~ (lift.updateKeys =<< (use $ field.player.keys))
  field %= execState update
  when_ ((Talking ==) `fmap` (use $ field . stateField)) $ running .= talkloop
  when_ ((Shooting ==) `fmap` (use $ field . stateField)) $ do
    g <- get
    let (s', f') = runStage (g^.stage) `runState` (g^.field)
    field .= f'
    stage .= s'
  
talkloop :: GameLoop ()
talkloop = do
  stgloop
  
  me <- use mEngine
  f <- use field
  lift $ draw (f^.resource) `execStateT` me
  mEngine %= execState update
  
  when_ ((== End) `fmap` use (mEngine.stateEngine)) $ do
    g <- get
    let (s', g') = runTalk (g^.stage) `runState` g
    put g'
    stage .= s'
  when_ ((== Waiting) `fmap` use (mEngine.stateEngine)) $ 
    when_ (keyChar 'Z') $ mEngine.stateEngine .= Parsing
  when_ (keySpecial KeyLeftControl) $ do
    when_ ((== Waiting) `fmap` use (mEngine.stateEngine)) $ do
      mEngine.stateEngine .= Parsing
    when_ ((== Printing) `fmap` use (mEngine.stateEngine)) $ do
      p <- use (mEngine.printing)
      mEngine.cursor .= (length p - 1)
  
  where
    runTalk :: Stage () -> State GameFrame (Stage ())
    runTalk (Speak s :>>= next) = do
      mEngine.stateEngine .= Parsing
      mEngine.message .= s
      return (next ())
    runTalk (Endtalk :>>= next) = do
      field.stateField .= Shooting
      running .= stgloop
      return (next ())
    runTalk (LiftField f :>>= next) = field %= execState f >> return (next ())
    runTalk (GetField :>>= next) = next `fmap` use field
    runTalk u = return u

game :: IO (Maybe a)
game = runGame window $ do
  let field' = def & resource .~ def & isDebug .~ False
  
  let its = V.fromList [Item "Game Start" loadloop,
                        Item "Quit" quit]
  
  flip evalStateT (GameFrame {
                      _field = field',
                      _menu = def & items .~ its,
                      _mEngine = def,
                      _stage = stage1,
                      _running = menuloop }) $ foreverTick $ do
    go <- use running
    go
    
    when_ (keySpecial KeyEsc) $ quit
