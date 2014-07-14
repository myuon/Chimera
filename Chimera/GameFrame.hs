{-# LANGUAGE TemplateHaskell, GADTs, FlexibleContexts #-}
module Chimera.GameFrame (game) where

import FreeGame
import Control.Lens
import Control.Monad.State.Strict
import Control.Monad.Reader
import Data.Maybe (isJust)
import Data.Default (def)
import qualified Data.Vector as V
import Data.Reflection (Given, give, given)
import Data.Functor.Product

import Chimera.Engine.Core
import Chimera.Engine.Scripts
import Chimera.Scripts.Stage1
import Chimera.Config

type GameLoop = StateT GameFrame Game

data GameFrame = GameFrame {
  _running :: GameLoop (),
  _stage :: Stage (),
  _controller :: Controller,
  _field :: Field,
  _menu :: Select GameLoop,
  _mapMenu :: SelectMap,
  _mEngine :: MessageEngine,
  _quit :: Bool,
  _memory :: Memory
  }

makeLenses ''GameFrame

runStage :: GameLoop ()
runStage = do
  controller <~ liftM2 (\c -> runReader (runController c)) (use controller) (use field)
  use controller >>= \c -> when (isStageRunning c) $ do
    (s, Pair f g) <- liftM3 run (use controller) (use field) (use stage)
    stage .= s
    field %= execState g
    controller .= execState f c

  where
    run p q m = runLookAt p q m `runState` Pair (return ()) (return ())

menuloop :: (Given Resource) => GameLoop ()
menuloop = do
  menu' <- use menu
  let resource = given :: Resource
  (m, s) <- (lift $ selectloop (resource^.font) `runStateT` menu')
  maybe (return ()) (running .=) m
  menu .= s

maploop :: (Given Resource, Given Config) => Bitmap -> GameLoop ()
maploop bmp = do
  lift $ translate (V2 320 240) $ bitmap bmp
  menu' <- use mapMenu
  keys <- use (memory.cities)
  (m, s) <- (lift $ posloop ((given :: Resource)^.font) keys `runStateT` menu')
  when (isJust m) $ running .= stgloop
  mapMenu .= s

stgloop :: (Given Resource, Given Config) => GameLoop ()
stgloop = do
  _ <- use field >>= lift . execStateT paint
  field.player `zoom` actPlayer
  use controller >>= \c -> when (isShooting c) $ 
    field %= execState addBullet
  field %= execState update

  use controller >>= \r -> case isShooting r of 
    True -> runStage
    False -> running .= talkloop
  
talkloop :: (Given Resource, Given Config) => GameLoop ()
talkloop = do
  stgloop

  _ <- use mEngine >>= lift . execStateT paint
  mEngine %= execState update

  use (mEngine.stateEngine) >>= \s -> do
    when (s == End) $ do
      runStage
      use controller >>= \c -> id %= execState (runTalk c)

  use (mEngine.stateEngine) >>= \s -> do
    keyChar 'Z' >>= \z -> when (z && s == Waiting) $ do
      mEngine.stateEngine .= Parsing

  keyPress KeyLeftControl >>= \k -> when k $ do
    use (mEngine.stateEngine) >>= \s -> do
      when (s == Waiting) $
        mEngine.stateEngine .= Parsing
    use (mEngine.stateEngine) >>= \s -> do
      when (s == Printing) $ do
        use (mEngine.printing) >>= \p -> mEngine.cursor .= (length p - 1)
  
  where
    runTalk :: (Given Resource) => Controller -> State GameFrame ()
    runTalk Talk = mEngine.stateEngine .= End
    runTalk (Speak s) = do
      mEngine.stateEngine .= Parsing
      mEngine.message .= s
    runTalk Go = running .= stgloop
    runTalk _ = return ()

game :: IO ()
game = do
  c <- loadConfig
  runGame (c^.windowMode) (c^.windowSize) $ do
    setFPS 60
    setTitle (c^.titleName)
    clearColor $ Color 0 0 0.2 1.0
    r <- loadResource
    s <- give r loadGameConfig
    give r $ give c $
      evalStateT mainloop GameFrame {
        _field = def & player .~ (s^.defPlayer),
        _menu = def & items .~ menuItems (s^.defMapBitmap),
        _mapMenu = s^.defSelectMap,
        _mEngine = def,
        _stage = stage1,
        _controller = Go,
        _running = menuloop,
        _quit = False,
        _memory = (s^.defMemory) }
  return ()

  where
    menuItems :: (Given Config, Given Resource) => Bitmap -> V.Vector (Item GameLoop)
    menuItems m = V.fromList [
      Item "Game Start" stgloop,
      Item "Go somewhere" (maploop m),
      Item "Quit" $ quit .= True]

    mainloop :: (Given Resource) => GameLoop ()
    mainloop = do
      join $ use running
    
      keyPress KeyEscape >>= \k -> when k $ quit .= True
      tick
      use quit >>= \q -> unless q mainloop
