{-# LANGUAGE TemplateHaskell #-}
module Chimera.Layers ( 
  Layer, posLayer, sizeLayer, imgLayer
  , StateEngine(..), MessageEngine
  , Token(..), Expr(..), msingle
  , click, clickend, aline
  , message, cursor, stateEngine, typingTime, layer
  ) where

import Graphics.UI.FreeGame
import Control.Lens
import Data.Default
import Data.Monoid (Monoid, mempty, mappend, (<>))
import Control.Monad.State.Strict

import Chimera.Core.Types
import Chimera.Core.Util

data Layer = Layer {
  _posLayer :: Vec,
  _sizeLayer :: Vec,
  _imgLayer :: Resource -> Bitmap
  }
             
makeClassy ''Layer

instance Default Layer where
  def = Layer {
    _posLayer = V2 320 420,
    _sizeLayer = V2 550 100,
    _imgLayer = \res -> res^.layerBoard
    }

instance GUIClass Layer where
  update = return ()
  draw res = do
    a <- get
    s <- use sizeLayer
    b <- ((\f -> f res) `fmap` use imgLayer)
    let s' = fmap fromIntegral $ fromPair $ bitmapSize b
    translate (a^.posLayer) $ scale (s / s') $ fromBitmap b

data StateEngine = Printing | Parsing | Waiting | End deriving (Eq, Show)
data Token = Text String | ClickWait deriving (Eq, Show)
data Expr = Token :+: Expr | Empty deriving (Eq, Show)
infixr 9 :+:

instance Monoid Expr where
  mempty = Empty
  mappend Empty a = a
  mappend (t :+: e) a = t :+: mappend e a

msingle :: Token -> Expr
msingle u = u :+: mempty

click :: Expr -> Expr -> Expr
click a b = a <> (msingle ClickWait) <> b

clickend :: Expr
clickend = msingle ClickWait

aline :: String -> Expr
aline s = Text s :+: Empty

data MessageEngine = MessageEngine {
  _message :: Expr,
  _printing :: String,
  _cursor :: Int,
  _stateEngine :: StateEngine,
  _typingTime :: Int,
  _layerMessageEngine :: Layer
  }

makeLenses ''MessageEngine

instance Default MessageEngine where
  def = MessageEngine {
    _message = Empty,
    _printing = "",
    _cursor = 0,
    _stateEngine = End,
    _typingTime = 3,
    _layerMessageEngine = def
    }

instance HasLayer MessageEngine where layer = layerMessageEngine

runToken :: Token -> State MessageEngine ()
runToken (Text u) = do
  cursor .= 0
  printing .= u
  stateEngine .= Printing
runToken (ClickWait) = stateEngine .= Waiting

runExpr :: Expr -> State MessageEngine ()
runExpr Empty = return ()
runExpr (t :+: es) = do
  runToken t
  message .= es

instance GUIClass MessageEngine where
  update = do
    when_ ((== Parsing) `fmap` use stateEngine) $ do
      when_ ((== Empty) `fmap` use message) $ stateEngine .= End
      m <- use message
      runExpr m
    when_ ((== Printing) `fmap` use stateEngine) $ do
      cursor %= (+1)
      m <- use printing
      t <- use typingTime
      when_ ((== t * length m) `fmap` use cursor) $ stateEngine .= Parsing
    
  draw res = do
    la <- use layer
    lift $ draw res `execStateT` la
    c <- use cursor
    m <- use printing
    t <- use typingTime
    translate (topleft la) . colored black . text (res^.font) 20 $ take (c `div` t) m

    where
      topleft :: Layer -> Vec
      topleft la = la^.posLayer - la^.sizeLayer/2 + V2 25 25

