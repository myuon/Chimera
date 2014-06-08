{-# LANGUAGE TemplateHaskell, GADTs, FlexibleContexts #-}
module Chimera where

import FreeGame
import Control.Lens
import Control.Monad.State.Strict
import Data.Maybe (isJust)
import Data.Default (def)
import qualified Data.Vector as V
import Data.Reflection (Given, give, given)

import Chimera.Engine
import Chimera.Menu
import Chimera.Scripts.Stage1
import Chimera.Config (loadConfig)
import Chimera.Load (loadResource)

type GameLoop = StateT GameFrame Game

data GameFrame = GameFrame {
  _running :: GameLoop (),
  _stage :: Stage (),
  _controller :: Controller,
  _field :: Field,
  _menu :: Select GameLoop,
  _mapMenu :: SelectMap,
  _mEngine :: MessageEngine,
  _quit :: Bool
  }

makeLenses ''GameFrame

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
  let resource = given :: Resource
  (m, s) <- (lift $ posloop (resource^.font) `runStateT` menu')
  when (isJust m) $ running .= stgloop
  mapMenu .= s

stepStage :: GameFrame -> GameLoop ()
stepStage g = do
  let ((c', s'), g') = runStage (g^.controller) (g^.stage) `runState` (g^.field)
  field .= g'
  stage .= s'
  controller .= c'

stgloop :: (Given Resource, Given Config) => GameLoop ()
stgloop = do
  use field >>= lift . execStateT paint
  field.player `zoom` actPlayer
  when_ (isShooting `fmap` use controller) $ 
    field %= execState addBullet
  field %= execState update

  use controller >>= \r -> case isShooting r of 
    True -> get >>= stepStage
    False -> running .= talkloop
  
talkloop :: (Given Resource, Given Config) => GameLoop ()
talkloop = do
  stgloop

  use mEngine >>= lift . execStateT paint
  mEngine %= execState update
  
  when_ ((== End) `fmap` use (mEngine.stateEngine)) $ do
    get >>= stepStage
    use controller >>= \c -> id %= execState (runTalk c)
  when_ ((== Waiting) `fmap` use (mEngine.stateEngine)) $ 
    when_ (keyChar 'Z') $ mEngine.stateEngine .= Parsing
  when_ (keyPress KeyLeftControl) $ do
    when_ ((== Waiting) `fmap` use (mEngine.stateEngine)) $
      mEngine.stateEngine .= Parsing
    when_ ((== Printing) `fmap` use (mEngine.stateEngine)) $ do
      use (mEngine.printing) >>= \p -> mEngine.cursor .= (length p - 1)
  
  where
    runTalk :: (Given Resource) => Controller -> State GameFrame ()
    runTalk Talk = mEngine.stateEngine .= End
    runTalk (Speak s) = do
      mEngine.stateEngine .= Parsing
      mEngine.message .= s
    runTalk Go = running .= stgloop
    runTalk _ = return ()

game :: IO (Maybe ())
game = do
  c <- loadConfig
  give c $ do
    let config = given :: Config
    runGame (config^.windowMode) (config^.windowSize) $ do
      setFPS 60
      setTitle (config^.titleName)
      clearColor $ Color 0 0 0.2 1.0
      m <- readBitmap "data/img/map0.png"
      r <- loadResource
      give r $
        let its = V.fromList [Item "Game Start" stgloop,
                              Item "Go somewhere" (maploop m),
                              Item "Quit" $ quit .= True] in
        evalStateT mainloop GameFrame {
                            _field = def,
                            _menu = def & items .~ its,
                            _mapMenu = def,
                            _mEngine = def,
                            _stage = stage1,
                            _controller = Go,
                            _running = menuloop, 
                            _quit = False }

  where
    mainloop :: (Given Resource) => GameLoop ()
    mainloop = do
      join $ use running
    
      when_ (keyPress KeyEscape) $ quit .= True
      tick
      use quit >>= \q -> unless q mainloop
