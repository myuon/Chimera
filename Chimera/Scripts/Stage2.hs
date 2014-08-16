{-# LANGUAGE FlexibleContexts #-}
module Chimera.Scripts.Stage2 (
  stage2
  )
  where

import FreeGame
import Control.Lens
import Control.Arrow
import Control.Monad.Trans (lift)
import Data.Default (def)
import Data.Reflection (Given, given)
import qualified Data.Map as M
import Debug.Trace as Trace

import Chimera.Engine.Core
import Chimera.Engine.Scripts
import Chimera.Scripts.Common

type Board a = M.Map (Int,Int) a

(!?) :: Board a -> (Int,Int) -> Maybe a
(!?) = flip M.lookup

rule :: Bool -> Int -> Bool
rule True n
  | 2 <= n && n <= 3 = True
  | otherwise = False
rule False n
  | n == 3 = True
  | otherwise = False

step :: Board Bool -> Board Bool
step b = M.mapWithKey (\k _ -> sur k) b where
  sur (x,y) = rule ((\(Just x) -> x)$ b !? (x,y)) $ length $ filter (== Just True) $
    fmap (b!?) [(x-1,y-1),(x-1,y),(x-1,y+1),(x,y-1),(x,y+1),(x+1,y-1),(x+1,y),(x+1,y+1)]

stage2 :: (Given Resource, Given Config) => Stage ()
stage2 = do
  lift $ addEffect effPlayerBack

  wait 50

  keeper $ initEnemy (V2 224 (-40)) 100 & runAuto .~ gameOfLife

gameOfLife :: (Given Resource, Given Config, HasChara c, HasPiece c, HasObject c) =>
              Danmaku c ()
gameOfLife = do
  setName "Conway's Game of Life"
  
  hook $ Left $ motionCommon 100 Stay

  s <- self
  when (s^.counter == 130) $ enemyEffect $ effGrid
  when (s^.counter `mod` 100 == 0 && s^.counter >= 300) $ do
    let b = (iterate step $ lineBoard w' h') !! ((s^.counter - 300) `div` 100)
    shots $ flip fmap (M.toList $ M.filter id b) $ \((x,y),_) ->
      makeBullet BallTiny Green def
        & pos .~ (fromIntegral $ n`div`2) + V2 (fromIntegral $ x*n) (fromIntegral $ y*n)
        & speed .~ 0
        & runAuto .~ do 
          hook $ Left $ do
            use counter >>= \c -> when (c == 50) $ statePiece .= Dead

  where
    n = 15
    intv = 4
    V2 w h = (\(Box x y) -> fmap abs $ x-y) $ (given :: Config)^.validArea
    tl (Box x _) = x
    centerX = fromIntegral $ (n`div`2) + n*(truncate w)`div`n`div`2
    w' = truncate w `div` n
    h' = truncate h `div` n

    line' = color (Color 1 1 1 0.5) . thickness 2.0 .
            translate (tl $ (given :: Config)^.validArea) . line
    lx x = forM_ [0..w'+1] $ \i -> do
              line' $ fmap (flip V2 (fromIntegral $ (i+1)*n)) $ [0,x]
    ly y = forM_ [0..h'+1] $ \i -> do
              line' $ fmap (V2 (fromIntegral $ (i+1)*n)) $ [0,y]

    effGrid :: Effect
    effGrid =
      def & zIndex .~ Background & runAuto .~ do
        c <- (^.counter) `fmap` self
        hook $ Left $ do
          drawing .= case ((truncate h) `div` intv <= c) of
            True -> lx w >> ly h
            False -> do
              lx $ fromIntegral $ c*intv
              ly $ fromIntegral $ c*intv

    lineBoard :: Int -> Int -> Board Bool
    lineBoard w h = M.fromList [((x,y),f (x,y))|x<-[0..w'-1], y<-[0..h'+100]] where
      f (x,y)
        | x == w' `div` 2 && y >= 3 = True
        | otherwise = False
