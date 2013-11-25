module Chimera.Scripts.Stage1 (
  load1, stage1
  )
  where

import Graphics.UI.FreeGame
import Control.Lens
import Chimera.STG.Types
import Chimera.Load

load1 :: Resource
load1 = def

stage1 :: Stage ()
stage1 = do
  res <- getResource

  appear 30 $ initEnemy (V2 320 (-40)) 2 (snd $ res ^. charaImg) (Zako 10)
  appear 50 $ initEnemy (V2 300 (-40)) 2 (snd $ res ^. charaImg) (Zako 10)
  appear 70 $ initEnemy (V2 280 (-40)) 2 (snd $ res ^. charaImg) (Zako 10)
  appear 90 $ initEnemy (V2 260 (-40)) 2 (snd $ res ^. charaImg) (Zako 10)
  appear 110 $ initEnemy (V2 240 (-40)) 2 (snd $ res ^. charaImg) (Zako 10)

  appear 150 $ initEnemy (V2 220 (-40)) 2 (snd $ res ^. charaImg) (Zako 11)
  appear 170 $ initEnemy (V2 200 (-40)) 2 (snd $ res ^. charaImg) (Zako 11)
  appear 190 $ initEnemy (V2 180 (-40)) 2 (snd $ res ^. charaImg) (Zako 11)
  appear 210 $ initEnemy (V2 160 (-40)) 2 (snd $ res ^. charaImg) (Zako 11)
  appear 230 $ initEnemy (V2 140 (-40)) 2 (snd $ res ^. charaImg) (Zako 11)

  appear 400 $ initEnemy (V2 300 (-40)) 2 (snd $ res ^. charaImg) (Zako 20)
  appear 420 $ initEnemy (V2 100 (-40)) 2 (snd $ res ^. charaImg) (Zako 21)
  appear 440 $ initEnemy (V2 280 (-40)) 2 (snd $ res ^. charaImg) (Zako 20)
  appear 460 $ initEnemy (V2 120 (-40)) 2 (snd $ res ^. charaImg) (Zako 21)
  appear 480 $ initEnemy (V2 260 (-40)) 2 (snd $ res ^. charaImg) (Zako 20)
  appear 500 $ initEnemy (V2 140 (-40)) 2 (snd $ res ^. charaImg) (Zako 21)

  appear 570 $ initEnemy (V2 260 (-40)) 10 (snd $ res ^. charaImg) (Zako 30)

  appear 870 $ initEnemy (V2 260 (-40)) 10 (snd $ res ^. charaImg) (Zako 40)

  appear 1170 $ initEnemy (V2 260 (-40)) 10 (snd $ res ^. charaImg) (Zako 50)

  appear 1500 $ initEnemy (V2 260 (-40)) 10 (snd $ res ^. charaImg) (Zako 60)

  appear 2000 $ initEnemy (V2 260 (-40)) 10 (snd $ res ^. charaImg) (Boss 0)

  appear 2500 $ initEnemy (V2 260 (-40)) 10 (snd $ res ^. charaImg) (Boss 1)

