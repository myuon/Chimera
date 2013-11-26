module Chimera.Scripts.Stage1 (
  load1, stage1
  )
  where

import Graphics.UI.FreeGame
import Control.Lens
import Chimera.STG.Util
import Chimera.STG.Types
import Chimera.STG.World
import Chimera.Load

load1 :: Resource
load1 = def

stage1 :: Stage ()
stage1 = do
  res <- getResource

  appearAt 30 $ initEnemy (V2 320 (-40)) 2 res (Zako 10)
  appearAt 20 $ initEnemy (V2 300 (-40)) 2 res (Zako 10)
  appearAt 20 $ initEnemy (V2 280 (-40)) 2 res (Zako 10)
  appearAt 20 $ initEnemy (V2 260 (-40)) 2 res (Zako 10)
  appearAt 20 $ initEnemy (V2 240 (-40)) 2 res (Zako 10)
  
  wait 30

  appearAt 20 $ initEnemy (V2 220 (-40)) 2 res (Zako 11)
  appearAt 20 $ initEnemy (V2 200 (-40)) 2 res (Zako 11)
  appearAt 20 $ initEnemy (V2 180 (-40)) 2 res (Zako 11)
  appearAt 20 $ initEnemy (V2 160 (-40)) 2 res (Zako 11)
  appearAt 20 $ initEnemy (V2 140 (-40)) 2 res (Zako 11)
  
  wait 30

  appearAt 20 $ initEnemy (V2 300 (-40)) 2 res (Zako 20)
  appearAt 20 $ initEnemy (V2 100 (-40)) 2 res (Zako 21)
  appearAt 20 $ initEnemy (V2 280 (-40)) 2 res (Zako 20)
  appearAt 20 $ initEnemy (V2 120 (-40)) 2 res (Zako 21)
  appearAt 20 $ initEnemy (V2 260 (-40)) 2 res (Zako 20)
  appearAt 20 $ initEnemy (V2 140 (-40)) 2 res (Zako 21)
  
  wait 30
  
  appearAt 20 $ initEnemy (V2 260 (-40)) 10 res (Zako 30)
  appearAt 300 $ initEnemy (V2 260 (-40)) 10 res (Zako 40)
  appearAt 300 $ initEnemy (V2 260 (-40)) 10 res (Zako 50)
  appearAt 300 $ initEnemy (V2 260 (-40)) 10 res (Zako 60)
  
  wait 500

  keeper $ initEnemy (V2 260 (-40)) 10 res (Boss 0)

  wait 20

  keeper $ initEnemy (V2 260 (-40)) 10 res (Boss 1)

