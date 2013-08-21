{-# LANGUAGE RankNTypes, FlexibleContexts, TypeSynonymInstances, FlexibleInstances #-}
module Global where

import Graphics.UI.SDL.Rect as SDLR
import Control.Lens
import Control.Applicative

type Point a = (a,a)

apply :: (a -> a -> a) -> Point a -> Point a -> Point a
apply f (a,b) (c,d) = (f a c, f b d)

mapp :: (a -> b) -> Point a -> Point b
mapp f (a,b) = (a,b) & both %~ f

($+) :: (Num a) => Point a -> Point a -> Point a
($+) = apply (+)

($*) :: (Num a) => a -> Point a -> Point a
($*) k = mapp (*k)

type Pos = Point Int

toNum :: (Num a) => Pos -> Point a
toNum = mapp fromIntegral

center :: Rect -> Pos
center r = (rectX r, rectY r) $+ (mapp floor $ (-1/2) $* toNum (rectW r, rectH r))