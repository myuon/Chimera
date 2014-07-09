{-# LANGUAGE TemplateHaskell #-}
module Chimera.Core.Menu where

import FreeGame
import Control.Lens
import Data.Default
import Control.Monad.State.Strict
import qualified Data.Vector as V
import qualified Data.Map as M
import qualified Data.Foldable as F

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
  mapM_ (\i -> write (fromIntegral (i+1)*30) $ (its V.! i)^.caption) [0..V.length its-1]
  
  p <- use pointing
  translate (V2 70 (200+fromIntegral (p+1)*30)) . color white . text font 20 $ "|>"
  
  when_ (keyDown KeyDown) $ pointing += 1
  when_ (keyDown KeyUp) $ pointing -= 1
  
  when_ ((<0) `fmap` use pointing) $ pointing .= 0
  when_ ((> V.length its - 1) `fmap` use pointing) $ pointing .= (V.length its - 1)
  
  z <- keyChar 'Z'
  case z of
    True -> return $ Just $ its V.! p ^. exec
    False -> return Nothing

type MapInfo = M.Map String (Vec2, M.Map Key String)

data SelectMap = SelectMap {
  _mapinfo :: MapInfo,
  _pointing2 :: (String, Vec2)
  }

makeLenses ''SelectMap

posloop :: Font -> StateT SelectMap Game (Maybe String)
posloop font = do
  (s,p) <- use pointing2
  m <- use mapinfo
  
  forM_ (wires s m) $ \(x,y) ->
    color white . thickness 2 . line $ [x,y]

  translate (p + 3) . color (Color 0.4 0.4 0.4 1.0) . text font 20 $ s
  translate p . color white . text font 20 $ s
  
  let keyMap = snd $ m M.! s
  forM_ [KeyUp, KeyRight, KeyDown, KeyLeft] $ \k ->
    when_ (keyDown k) $ case k `M.lookup` keyMap of
      Just u -> pointing2 .= (u, fst $ m M.! u)
      Nothing -> return ()
  
  z <- keyDown $ charToKey 'Z'
  case z of
    True -> (Just . fst) `fmap` use pointing2
    False -> return Nothing
  
  where
    wires :: String -> MapInfo -> [(Vec2, Vec2)]
    wires s m = let (p,ps) = m M.! s in
      F.toList $ fmap ((,) p . fst . (m M.!)) $ ps
