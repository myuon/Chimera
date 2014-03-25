{-# LANGUAGE TemplateHaskell #-}
module Chimera.Layers ( 
  Layer, posLayer, sizeLayer, imgLayer
  , StateEngine(..), MessageEngine
  , Token(..), Expr(..), msingle
  , click, clickend, aline
  , message, printing, cursor, stateEngine, typingTime, layer
  ) where

import FreeGame
import Control.Lens
import Data.Default
import Data.Monoid (Monoid, mempty, mappend, (<>))
import Data.List (unfoldr)
import Control.Monad.State.Strict

import Chimera.Core.Types
import Chimera.Core.Util

data Layer = Layer {
  _posLayer :: Vec2,
  _sizeLayer :: Vec2,
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
  paint res = do
    a <- get
    s <- use sizeLayer
    b <- ((\f -> f res) `fmap` use imgLayer)
    let s' = fmap fromIntegral $ uncurry V2 $ bitmapSize b
    translate (a^.posLayer) $ scale (s / s') $ bitmap b

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
  _layerMessageEngine :: Layer,
  _counterEngine :: Int
  }

makeLenses ''MessageEngine

instance Default MessageEngine where
  def = MessageEngine {
    _message = Empty,
    _printing = "",
    _cursor = 0,
    _stateEngine = End,
    _typingTime = 3,
    _layerMessageEngine = def,
    _counterEngine = 0
    }

instance HasLayer MessageEngine where layer = layerMessageEngine

runToken :: Token -> State MessageEngine ()
runToken (Text u) = do
  la <- use layerMessageEngine
  cursor .= 0
  printing .= overflow u (la^.sizeLayer)
  stateEngine .= Printing
  
  where
    overflow :: String -> Vec2 -> String
    overflow s (V2 w _) = foldr (\x y -> x ++ ('\n' : y)) "" $ unfoldr go s where
      go :: String -> Maybe (String, String)
      go [] = Nothing
      go t = Just . splitAt (truncate $ w/20) $ t

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
      counterEngine %= (+1)
      t <- use typingTime
      c <- use counterEngine
      when (c `mod` t == 0) $ cursor %= (+1)
      m <- use printing
      when_ ((== length m) `fmap` use cursor) $ stateEngine .= Parsing
    
  paint res = do
    la <- use layer
    lift $ paint res `execStateT` la
    c <- use cursor
    m <- use printing
    translate (topleft la) . color black . text (res^.font) 20 $ take c m

    where
      topleft :: Layer -> Vec2
      topleft la = la^.posLayer - la^.sizeLayer/2 + V2 25 25

