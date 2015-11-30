{-# LANGUAGE OverloadedStrings #-}

import SDL
import Linear
import Prelude hiding (init)


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
  init >>=
    loadMedia
  close
