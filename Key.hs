{-# LANGUAGE TemplateHaskell, ForeignFunctionInterface #-}
module Key where

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Utilities as SDLU
import Control.Lens

import Language.Haskell.TH
import Data.List
import Foreign
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array

data Keys = Keys {
  _space :: Int,
  _up :: Int,
  _down :: Int,
  _right :: Int,
  _left :: Int
  } deriving Show

makeLenses ''Keys

initKeys :: Keys
initKeys = Keys 0 0 0 0 0

initSpKeys :: SDL.SDLKey -> Keys
initSpKeys SDL.SDLK_SPACE = space .~ 1 $ initKeys
initSpKeys SDL.SDLK_UP    = up    .~ 1 $ initKeys
initSpKeys SDL.SDLK_DOWN  = down  .~ 1 $ initKeys
initSpKeys SDL.SDLK_RIGHT = right .~ 1 $ initKeys
initSpKeys SDL.SDLK_LEFT  = left  .~ 1 $ initKeys
initSpKeys _ = initKeys

plusKeys :: Keys -> Keys -> Keys
plusKeys k k' = 
  space .~ (k ^. space) + (k' ^. space) $
  up    .~ (k ^.    up) + (k' ^.    up) $
  down  .~ (k ^.  down) + (k' ^.  down) $
  right .~ (k ^. right) + (k' ^. right) $
  left  .~ (k ^.  left) + (k' ^.  left) $
  initKeys

foreign import ccall unsafe "SDL_GetKeyState" sdlGetKeyState :: Ptr CInt -> IO (Ptr Word8)

getKeyState :: IO [SDL.SDLKey]
getKeyState = alloca $ \numkeysPtr -> do
  keysPtr <- sdlGetKeyState numkeysPtr
  numkeys <- peek numkeysPtr
  (map SDLU.toEnum . map fromIntegral . findIndices (== 1)) `fmap` 
    peekArray (fromIntegral numkeys) keysPtr

isKeyPressed :: SDL.SDLKey -> SDL.Event -> Bool
isKeyPressed spKey (SDL.KeyUp (SDL.Keysym key _ _)) = key == spKey
isKeyPressed _ _ = False

update :: Keys -> IO Keys
update keys = do
  k <- getKeyState
  return $ mergeKeys (putKeys k) keys
  
  where
    putKeys :: [SDL.SDLKey] -> Keys
    putKeys [] = initKeys
    putKeys (k:ks) = initSpKeys k `plusKeys` putKeys ks
    
    mergeKeys :: Keys -> Keys -> Keys
    mergeKeys k keys =
      space %~ keyFun (k ^. space) $
      up    %~ keyFun (k ^.    up) $
      down  %~ keyFun (k ^.  down) $
      right %~ keyFun (k ^. right) $
      left  %~ keyFun (k ^.  left) $
      keys
  
      where
        keyFun :: Int -> (Int -> Int)
        keyFun = keyFun' . (> 0)
        
        keyFun' :: Bool -> (Int -> Int)
        keyFun' True = (+1)
        keyFun' False = const 0

