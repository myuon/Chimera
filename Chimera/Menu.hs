{-# LANGUAGE TemplateHaskell #-}
module Chimera.Menu where

import FreeGame
import Control.Lens
import Data.Default
import Control.Monad.State.Strict
import qualified Data.Vector as V

import Chimera.Core.Util

data Item m = Item {
  _caption :: String,
  _exec :: m ()
  }

makeLenses ''Item

data Select m = Select {
  _items :: V.Vector (Item m),
  _pointing :: Int
  }

makeLenses ''Select

instance Default (Select m) where
  def = Select {
    _items = V.empty,
    _pointing = 0
    }

selectloop :: Font -> StateT (Select m) Game (Maybe (m ()))
selectloop font = do
  its <- use items
  let write y = translate (V2 100 (200+y)) . color white . text font 20
  mapM_ (\i -> write (fromIntegral (i+1)*30) $ (its V.! i)^.caption) [0..(V.length its)-1]
  
  p <- use pointing
  translate (V2 70 (200+fromIntegral (p+1)*30)) . color white . text font 20 $ "|>"
  
  when_ (keyDown KeyDown) $ pointing += 1
  when_ (keyDown KeyUp) $ pointing -= 1
  
  when_ ((<0) `fmap` use pointing) $ pointing .= 0
  when_ ((> V.length its - 1) `fmap` use pointing) $ pointing .= (V.length its - 1)
  
  z <- keyChar 'Z'
  case z of
    True -> return $ Just $ its V.! p ^. exec
    False -> return $ Nothing
