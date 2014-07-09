{-# LANGUAGE FlexibleContexts #-}
module Chimera.Config where

import FreeGame
import Control.Lens
import Data.Reflection
import Data.Default

import Chimera.World
import Chimera.Scripts.Geography

loadConfig :: IO Config
loadConfig = return $ Config {}
  & windowMode .~ Windowed
  & windowSize .~ Box (V2 0 0) (V2 640 480)
  & gameArea .~ Box (V2 32 16) (V2 416 444)
  & validArea .~ Box (V2 0 (-16)) (V2 448 476)
  & debugMode .~ False
  & titleName .~ "Chimera"

loadGameConfig :: (Given Resource) => Game GameConfig
loadGameConfig = do
  m <- readBitmap "data/img/map0.png"
  return $ GameConfig {}
    & defPlayer .~ def { _shotZ = fourDiamond, _shotX = silentBomb }
    & defSelectMap .~ SelectMap { _mapinfo = marf, _pointing2 = ("マーフの街", V2 468 371) }
    & defMapBitmap .~ m
