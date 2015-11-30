{-# LANGUAGE OverloadedStrings #-}

import SDL
import Linear


main :: IO ()
main = do
  initialize [InitVideo]

  window <- createWindow "A First SDL Try" $
    defaultWindow { windowInitialSize = V2 600 480 }

  surface <- getWindowSurface window
  surfaceFillRect surface Nothing (V4 0 0 255 0)

  updateWindowSurface window

  delay 2000
  quit
