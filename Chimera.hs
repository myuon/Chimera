{-# LANGUAGE TemplateHaskell, GADTs #-}
module Chimera where

import FreeGame
import Control.Lens
import Control.Monad.State.Strict
import Control.Monad.Operational.Mini
import Data.Maybe (fromJust, isJust)
import Data.Default (def)
import qualified Data.Vector as V
import qualified Data.Sequence as S

import Chimera.Engine
import Chimera.Menu
import Chimera.Scripts.Stage1

type GameLoop = StateT GameFrame Game

data GameFrame = GameFrame {
  _running :: GameLoop (),
  _stage :: Stage (),
  _field :: Field,
  _menu :: Select GameLoop,
  _mEngine :: MessageEngine,
  _quit :: Bool
  }

makeLenses ''GameFrame

menuloop :: GameLoop ()
menuloop = do
  menu' <- use menu
  font' <- use (field.resource.font)
  (m, s) <- (lift $ selectloop font' `runStateT` menu')
  when (isJust m) $ running .= fromJust m
  menu .= s

loadloop :: GameLoop ()
loadloop = do
  font' <- use (field.resource.font)
  field.resource <~ (lift . execLoad =<< use (field.resource))
  lift $ waiting font' 0

  running .= stgloop
  where
    waiting :: Font -> Int -> Game ()
    waiting font' n = do
      translate (V2 30 30) . color white . text (font') 20 $ "読み込み中…" ++ show n
      tick
      when (n < 1) $ waiting font' $ n+1

stgloop :: GameLoop ()
stgloop = do
  gf <- get

  lift $ paint (error "_") `execStateT` (gf ^. field)
  field.player `zoom` actPlayer
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
  lift $ paint (f^.resource) `execStateT` me
  mEngine %= execState update
  
  when_ ((== End) `fmap` use (mEngine.stateEngine)) $ do
    g <- get
    let (s', g') = runTalk (g^.stage) `runState` g
    put g'
    stage .= s'
  when_ ((== Waiting) `fmap` use (mEngine.stateEngine)) $ 
    when_ (keyChar 'Z') $ mEngine.stateEngine .= Parsing
  when_ (keyPress KeyLeftControl) $ do
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

game :: IO (Maybe ())
game = runGame Windowed (BoundingBox 0 0 640 480) $ do
  setFPS 60
  setTitle "Chimera"
  clearColor $ Color 0 0 0.2 1.0
  r <- initResource
  let field' = def & resource .~ r & isDebug .~ False
  
  let its = V.fromList [Item "Game Start" loadloop,
                        Item "Quit" $ quit .= True]
  
  flip evalStateT (GameFrame {
                      _field = field',
                      _menu = def & items .~ its,
                      _mEngine = def,
                      _stage = stage1,
                      _running = menuloop, 
                      _quit = False }) $ mainloop where
    mainloop :: GameLoop ()
    mainloop = do
      go <- use running
      go
    
      when_ (keyPress KeyEscape) $ quit .= True
      q <- use quit
      tick
      unless q $ mainloop
