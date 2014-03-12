{-# LANGUAGE TemplateHaskell, GADTs #-}
module Chimera where

import FreeGame
import Control.Lens
import Control.Monad.State.Strict
import Data.Maybe (isJust)
import Data.Default (def)
import qualified Data.Vector as V

import Chimera.Engine
import Chimera.Menu
import Chimera.Scripts.Stage1

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

menuloop :: GameLoop ()
menuloop = do
  menu' <- use menu
  font' <- use (field.resource.font)
  (m, s) <- (lift $ selectloop font' `runStateT` menu')
  maybe (return ()) (running .=) m
  menu .= s

maploop :: Bitmap -> GameLoop ()
maploop bmp = do
  lift $ translate (V2 320 240) $ bitmap bmp
  menu' <- use mapMenu
  font' <- use (field.resource.font)
  (m, s) <- (lift $ posloop font' `runStateT` menu')
  when (isJust m) $ running .= loadloop
  mapMenu .= s

loadloop :: GameLoop ()
loadloop = do
  font' <- use (field.resource.font)
  field.resource <~ (lift . execLoad =<< use (field.resource))
--  lift $ waiting font' 0

  running .= stgloop
  where
    waiting :: Font -> Int -> Game ()
    waiting font' n = do
      translate (V2 30 30) . color white . text font' 20 $ "読み込み中…" ++ show n
      tick
      when (n > 0) $ waiting font' $ n-1

stgloop :: GameLoop ()
stgloop = do
  use field >>= \f -> lift $ paint (error "_") `execStateT` f
  
  _ <- do
    field.player `zoom` actPlayer
    when_ (isShooting `fmap` use controller) $ 
      field %= execState addBullet
  field %= execState update
  use controller >>= \r -> case isShooting r of 
    True -> do
      g <- get
      let ((c', s'), g') = runStage (g^.controller) (g^.stage) `runState` (g^.field)
      field .= g'
      stage .= s'
      controller .= c'
    False -> running .= talkloop
  
talkloop :: GameLoop ()
talkloop = do
  stgloop
  
  _ <- do
    me <- use mEngine
    res <- use (field.resource)
    lift $ paint res `execStateT` me
  mEngine %= execState update
  
  when_ ((== End) `fmap` use (mEngine.stateEngine)) $ do
    g <- use id
    let ((c', s'), g') = runStage (g^.controller) (g^.stage) `runState` (g^.field)
    field .= g'
    stage .= s'
    controller .= c'
    
    use controller >>= \c -> id %= execState (runTalk c)
  when_ ((== Waiting) `fmap` use (mEngine.stateEngine)) $ 
    when_ (keyChar 'Z') $ mEngine.stateEngine .= Parsing
  when_ (keyPress KeyLeftControl) $ do
    when_ ((== Waiting) `fmap` use (mEngine.stateEngine)) $
      mEngine.stateEngine .= Parsing
    when_ ((== Printing) `fmap` use (mEngine.stateEngine)) $ do
      p <- use (mEngine.printing)
      mEngine.cursor .= (length p - 1)
  
  where
    runTalk :: Controller -> State GameFrame ()
    runTalk Talk = mEngine.stateEngine .= End
    runTalk (Speak s) = do
      mEngine.stateEngine .= Parsing
      mEngine.message .= s
    runTalk Go = running .= stgloop
    runTalk _ = return ()

game :: IO (Maybe ())
game = runGame Windowed (BoundingBox 0 0 640 480) $ do
  setFPS 60
  setTitle "Chimera"
  clearColor $ Color 0 0 0.2 1.0
  r <- initResource
  let field' = def & resource .~ r & isDebug .~ False
  m <- readBitmap "data/img/map0.png"
  
  let its = V.fromList [Item "Game Start" loadloop,
                        Item "Go somewhere" (maploop m),
                        Item "Quit" $ quit .= True]
  
  evalStateT mainloop GameFrame {
                      _field = field',
                      _menu = def & items .~ its,
                      _mapMenu = def,
                      _mEngine = def,
                      _stage = stage1,
                      _controller = Go,
                      _running = menuloop, 
                      _quit = False } where
    mainloop :: GameLoop ()
    mainloop = do
      join $ use running
    
      when_ (keyPress KeyEscape) $ quit .= True
      q <- use quit
      tick
      unless q mainloop
