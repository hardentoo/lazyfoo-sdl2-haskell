{-# LANGUAGE OverloadedStrings #-}

import SDL
import Linear
import Prelude hiding (init)
import FRP.Yampa (reactimate, arr)
import Data.IORef


init :: IO Window
init = do
  initialize [InitVideo]

  createWindow "Image SDL" $
    defaultWindow { windowInitialSize = V2 600 480 }

loadMedia :: Window -> IO ()
loadMedia w = do
  windowSurface <- getWindowSurface w
  imageSurface <- loadBMP "./data/kitty.bmp"
  surfaceBlit imageSurface Nothing windowSurface Nothing
  updateWindowSurface w

close :: IO ()
close = do
  delay 2000
  quit

main :: IO ()
main = do
  window <- init
  t <- time
  timeRef <- newIORef t

  let sense   = (\_ -> do
        t' <- readIORef timeRef
        let dt = t' - t
        writeIORef timeRef dt
        pollEvent >>= return . (,) dt)
      actuate = (\_ b -> if b then return True else loadMedia window >> return False)
      sf      = arr ((== QuitEvent) . eventPayload)

  reactimate waitEvent sense actuate sf
  close
