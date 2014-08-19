{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, RankNTypes #-}
{-# LANGUAGE TypeFamilies, ImpredicativeTypes #-}
module Chimera.State (
  StateT(..), State(..),
  execStateT, evalStateT, runStateT,
  execState, evalState, runState,
  get, put, state, modify,
  lift
  ) where

import Control.Applicative
import Control.Monad.State.Class (MonadState(..))
import Control.Monad.Trans
import Data.Functor.Identity
import Control.Lens
import Control.Lens.Internal.Zoom
import Control.Lens.Internal.Action
import FreeGame.Class
import FreeGame (FreeGame(..))

newtype StateT s m a = StateT { runStateT' :: forall r. s -> (a -> s -> m r) -> m r }

instance Functor (StateT s m) where
  fmap f (StateT m) = StateT $ \s h -> m s $ \a -> h (f a)

instance Applicative (StateT s m) where
  pure a = StateT $ \s h -> h a s
  (StateT f) <*> (StateT m) = StateT $ \s h -> f s $ \g s' -> m s' (h . g)

instance Monad (StateT s m) where
  return a = StateT $ \s h -> h a s
  (StateT m) >>= k = StateT $ \s h -> m s $ \a s' -> runStateT' (k a) s' h

instance MonadState s (StateT s m) where
  get = state $ \s -> (s, s)
  put s = state $ \_ -> ((), s)
  state f = StateT $ \s h -> uncurry h (f s)

modify :: (Monad m) => (s -> s) -> StateT s m ()
modify f = get >>= \s -> put $! f s

instance MonadTrans (StateT s) where
  lift m = StateT $ \s h -> m >>= \a -> h a s

instance (FromFinalizer m, Monad m) => FromFinalizer (StateT s m) where
  fromFinalizer = lift . fromFinalizer

instance (Keyboard m, Monad m) => Keyboard (StateT s m) where
  keyStates_ = lift keyStates_

instance (Local m, Monad m) => Local (StateT s m) where
  getLocation = lift getLocation

instance (Mouse m, Monad m) => Mouse (StateT s m) where
  globalMousePosition = lift globalMousePosition
  mouseButtons_ = lift mouseButtons_
  mouseInWindow = lift mouseInWindow

instance (Affine m, Monad m) => Affine (StateT s m) where
  rotateD a = mapStateT (rotateD a)
  {-# INLINE rotateD #-}
  rotateR a = mapStateT (rotateR a)
  {-# INLINE rotateR #-}
  translate a = mapStateT (translate a)
  {-# INLINE translate #-}
  scale a = mapStateT (scale a)
  {-# INLINE scale #-}

instance (Picture2D m, Monad m) => Picture2D (StateT s m) where
  bitmap b = lift (bitmap b)
  {-# INLINE bitmap #-}
  bitmapOnce b = lift (bitmapOnce b)
  {-# INLINE bitmapOnce #-}
  line = lift . line
  polygon = lift . polygon
  polygonOutline = lift . polygonOutline
  circle = lift . circle
  circleOutline = lift . circleOutline
  thickness k = mapStateT (thickness k)
  color k = mapStateT (color k)
  {-# INLINE color #-}
  blendMode m = mapStateT (blendMode m)
  {-# INLINE blendMode #-}

instance (FreeGame m, Monad m) => FreeGame (StateT s m) where
  draw = lift . draw
  preloadBitmap = lift . preloadBitmap
  takeScreenshot = lift takeScreenshot
  bracket m = lift (bracket m)
  forkFrame m = lift (forkFrame m)
  setFPS a = lift (setFPS a)
  setTitle t = lift (setTitle t)
  showCursor = lift showCursor
  hideCursor = lift hideCursor
  clearColor c = lift (clearColor c)
  getFPS = lift getFPS
  getBoundingBox = lift getBoundingBox
  setBoundingBox b = lift (setBoundingBox b)

runStateT :: (Monad m) => StateT s m a -> s -> m (a,s)
runStateT (StateT m) s = m s (\a s' -> return $ (a,s'))

evalStateT :: (Monad m) => StateT s m a -> s -> m a
evalStateT m s = do
    (a, _) <- runStateT m s
    return a

execStateT :: (Monad m) => StateT s m a -> s -> m s
execStateT m s = do
    (_, s') <- runStateT m s
    return s'

mapStateT :: (Monad m, Monad n) => (m (a, s) -> n (b, s)) -> StateT s m a -> StateT s n b
mapStateT f m = stateT $ f . runStateT m

type State s = StateT s Identity

runState :: State s a -> s -> (a,s)
runState m = runIdentity . runStateT m

evalState :: State s a -> s -> a
evalState m s = fst $ runState m s

execState :: State s a -> s -> s
execState m s = snd $ runState m s

type instance Zoomed (StateT s z) = Focusing z

stateT :: (Monad m) => (s -> m (a,s)) -> StateT s m a
stateT f = StateT $ \s h -> f s >>= \as -> uncurry h as

instance Monad z => Zoom (StateT s z) (StateT t z) s t where
  zoom l m = stateT $ unfocusing . l (Focusing . (runStateT m))
