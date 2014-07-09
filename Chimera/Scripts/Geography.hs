module Chimera.Scripts.Geography where

import FreeGame
import qualified Data.Map as M

import Chimera.Core.Menu

marf :: MapInfo
marf = M.fromList [
  ("湖の洞窟",
   (V2 62 42, M.fromList [(KeyRight, "山奥の小屋"), (KeyDown, "光の神殿")])),
  ("山奥の小屋",
   (V2 204 85, M.fromList [(KeyRight, "リナの街"), (KeyLeft, "湖の洞窟")])),
  ("光の神殿",
   (V2 71 252, M.fromList [(KeyUp, "湖の洞窟")])),
  ("リナの街", 
   (V2 527 118, M.fromList [(KeyDown, "水門"), (KeyLeft, "山奥の小屋")])),
  ("水門",
   (V2 468 234, M.fromList [(KeyUp, "リナの街"), (KeyDown, "マーフの街")])),
  ("マーフの街",
   (V2 468 371, M.fromList [(KeyUp, "水門"), (KeyLeft, "砂漠の小屋")])),
  ("砂漠の小屋",
   (V2 245 400, M.fromList [(KeyRight, "マーフの街")]))
  ]
