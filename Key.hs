{-# LANGUAGE TemplateHaskell #-}
module Key where

import qualified Graphics.UI.FreeGame as Game
import Control.Lens
import Control.Monad.State

data Keys = Keys {
  _space :: Int,
  _up :: Int,
  _down :: Int,
  _right :: Int,
  _left :: Int,
  _z :: Int
  } deriving (Show)

makeLenses ''Keys

initKeys :: Keys
initKeys = Keys 0 0 0 0 0 0

update :: Keys -> Game.Game Keys
update keys = do
  space' <- Game.keySpecial Game.KeySpace
  up' <- Game.keySpecial Game.KeyUp
  down' <- Game.keySpecial Game.KeyDown
  right' <- Game.keySpecial Game.KeyRight
  left' <- Game.keySpecial Game.KeyLeft
  z' <- Game.keyChar 'Z'

  return $
    space %~ keyFun space' $
    up %~ keyFun up' $
    down %~ keyFun down' $
    right %~ keyFun right' $
    left %~ keyFun left' $
    z %~ keyFun z' $
    keys

  where
    keyFun :: Bool -> (Int -> Int)
    keyFun True = (+1)
    keyFun False = const 0

