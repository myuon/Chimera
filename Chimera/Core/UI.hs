{-# LANGUAGE TemplateHaskell #-}
module Chimera.Core.UI (
  Keys(..)
  , space, up, down, right, left, zKey, xKey, shift
  , updateKeys
  ) where

import Graphics.UI.FreeGame
import Control.Lens
import Data.Default

data Keys = Keys {
  _space :: Int,
  _up :: Int,
  _down :: Int,
  _right :: Int,
  _left :: Int,
  _shift :: Int,
  _zKey :: Int,
  _xKey :: Int
  } deriving (Show)

makeLenses ''Keys

instance Default Keys where
  def = Keys 0 0 0 0 0 0 0 0

updateKeys :: Keys -> Game Keys
updateKeys keys = do
  keySpace <- keySpecial KeySpace
  keyUp <- keySpecial KeyUp
  keyDown <- keySpecial KeyDown
  keyRight <- keySpecial KeyRight
  keyLeft <- keySpecial KeyLeft
  keyShift <- keySpecial KeyLeftShift
  keyZ <- keyChar 'Z'
  keyX <- keyChar 'X'
  
  return $
    space %~ keyFun keySpace $
    up %~ keyFun keyUp $
    down %~ keyFun keyDown $
    right %~ keyFun keyRight $
    left %~ keyFun keyLeft $
    shift %~ keyFun keyShift $
    zKey %~ keyFun keyZ $
    xKey %~ keyFun keyX $
    keys

  where
    keyFun :: Bool -> Int -> Int
    keyFun True = (+1)
    keyFun False = const 0


