{-# LANGUAGE TemplateHaskell #-}
module Chimera.STG.UI (
  Keys(..)
  , space, up, down, right, left, zKey
  , initKeys
  , updateKeys
  ) where

import Graphics.UI.FreeGame
import Control.Lens

data Keys = Keys {
  _space :: Int,
  _up :: Int,
  _down :: Int,
  _right :: Int,
  _left :: Int,
  _zKey :: Int
  } deriving (Show)

makeLenses ''Keys

initKeys :: Keys
initKeys = Keys 0 0 0 0 0 0

updateKeys :: Keys -> Game Keys
updateKeys keys = do
  space' <- keySpecial KeySpace
  up' <- keySpecial KeyUp
  down' <- keySpecial KeyDown
  right' <- keySpecial KeyRight
  left' <- keySpecial KeyLeft
  z' <- keyChar 'Z'
  
  return $
    space %~ keyFun space' $
    up %~ keyFun up' $
    down %~ keyFun down' $
    right %~ keyFun right' $
    left %~ keyFun left' $
    zKey %~ keyFun z' $
    keys

  where
    keyFun :: Bool -> Int -> Int
    keyFun True = (+1)
    keyFun False = const 0


