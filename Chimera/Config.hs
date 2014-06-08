module Chimera.Config where

import FreeGame
import Data.Default

import Chimera.Core.Types

instance Default Config where
  def = Config {
    _windowMode = Windowed,
    _windowSize = Box (V2 0 0) (V2 640 480),
    _gameArea = Box (V2 32 16) (V2 416 444),
    _validArea = Box (V2 0 (-16)) (V2 448 476),
    _debugMode = False,
    _titleName = "Chimera"
  }

loadConfig :: IO Config
loadConfig = return $ def
