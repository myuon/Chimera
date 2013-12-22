{-# LANGUAGE TemplateHaskell #-}
module Chimera.STG.UI (
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
  space' <- keySpecial KeySpace
  up' <- keySpecial KeyUp
  down' <- keySpecial KeyDown
  right' <- keySpecial KeyRight
  left' <- keySpecial KeyLeft
  shift' <- keySpecial KeyLeftShift
  z' <- keyChar 'Z'
  x' <- keyChar 'X'
  
  return $
    space %~ keyFun space' $
    up %~ keyFun up' $
    down %~ keyFun down' $
    right %~ keyFun right' $
    left %~ keyFun left' $
    shift %~ keyFun shift' $
    zKey %~ keyFun z' $
    xKey %~ keyFun x' $
    keys

  where
    keyFun :: Bool -> Int -> Int
    keyFun True = (+1)
    keyFun False = const 0


