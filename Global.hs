module Global where

import Control.Lens
import Control.Applicative

type Pos = (Int, Int)

($+) :: Pos -> Pos -> Pos
($+) (a,b) (c,d) = (a+c, b+d)

