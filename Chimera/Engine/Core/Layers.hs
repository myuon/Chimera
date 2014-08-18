{-# LANGUAGE TemplateHaskell #-}
module Chimera.Engine.Core.Layers ( 
  Layer, tileImg
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
--import Control.Monad.State.Strict
import CState
import Control.Monad.Trans
import Data.Reflection (given)

import Chimera.Engine.Core.Types
import Chimera.Engine.Core.Util

data Layer = Layer {
  _objLayer :: Object,
  _tileImg :: Resource -> Bitmap
  }

makeClassy ''Layer

instance HasObject Layer where object = objLayer

instance Default Layer where
  def = Layer {
    _objLayer = def
      & pos .~ V2 320 420
      & size .~ V2 550 100,
    _tileImg = \res -> res^.layerBoard
    }

instance GUIClass Layer where
  update = return ()
  paint = do
    a <- get
    s <- use size
    let resource = given :: Resource
    b <- ((\f -> f resource) `fmap` use tileImg)
    let s' = fmap fromIntegral $ uncurry V2 $ bitmapSize b
    translate (a^.pos) $ scale (s / s') $ bitmap b

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

runToken :: (Monad m) => Token -> StateT MessageEngine m ()
runToken (Text u) = do
  cursor .= 0
  printing <~ liftM (overflow u . (^.size)) (use layerMessageEngine)
  stateEngine .= Printing
  
  where
    overflow :: String -> Vec2 -> String
    overflow s (V2 w _) = foldr (\x y -> x ++ ('\n' : y)) "" $ unfoldr go s where
      go :: String -> Maybe (String, String)
      go [] = Nothing
      go t = Just . splitAt (truncate $ w/20) $ t

runToken (ClickWait) = stateEngine .= Waiting

runExpr :: (Monad m) => Expr -> StateT MessageEngine m ()
runExpr Empty = return ()
runExpr (t :+: es) = do
  runToken t
  message .= es

instance GUIClass MessageEngine where
  update = do
    use stateEngine >>= \s -> do
      when (s == Parsing) $ do
        use message >>= \m -> do
          when (m == Empty) $ stateEngine .= End
          runExpr m
    use stateEngine >>= \s -> do
      when (s == Printing) $ do
        counterEngine %= (+1)
        t <- use typingTime
        c <- use counterEngine
        when (c `mod` t == 0) $ cursor %= (+1)
        p <- use printing
        k <- use cursor
        when (k == length p) $ stateEngine .= Parsing
    
  paint = do
    la <- use layer
    let res = given :: Resource
    lift $ paint `execStateT` la
    c <- use cursor
    m <- use printing
    translate (topleft la) . color black . text (res^.font) 20 $ take c m

    where
      topleft :: Layer -> Vec2
      topleft la = la^.pos - la^.size/2 + V2 25 25

