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

reloadMedia :: Window -> Surface -> Surface -> IO ()
reloadMedia w windowSurface imageSurface = do
  surfaceBlit imageSurface Nothing windowSurface Nothing
  updateWindowSurface w

close :: IO ()
close = do
  quit

main :: IO ()
main = do
  window <- init
  windowSurface <- getWindowSurface window
  imageSurface <- loadBMP "./data/kitty.bmp"

  t <- time
  timeRef <- newIORef t

  let sense   = (\_ -> do
        t' <- readIORef timeRef
        let dt = t' - t
        writeIORef timeRef dt
        pollEvent >>= return . (,) dt)
      actuate = (\_ b -> if b
                         then return True
                         else reloadMedia window windowSurface imageSurface >> return False)
      sf      = arr ((== QuitEvent) . eventPayload)

  reactimate waitEvent sense actuate sf
  close
