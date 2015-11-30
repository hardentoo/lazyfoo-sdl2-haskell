{-# LANGUAGE OverloadedStrings #-}

import SDL
import Linear
import Prelude hiding (init)


init :: IO Window
init = do
  initialize [InitVideo]

  createWindow "Image SDL" $
    defaultWindow { windowInitialSize = V2 600 480 }

loop :: Window -> IO ()
loop w = do
  windowSurface <- getWindowSurface w
  imageSurface <- loadBMP "./kitty.bmp"

  surfaceBlit imageSurface Nothing windowSurface Nothing
  updateWindowSurface w

  while areWeQuit (reload w windowSurface imageSurface)

while :: IO Bool -> IO () -> IO ()
while iobool io = do
  b <- iobool
  case b of
    True -> return ()
    _    -> io >> while iobool io

areWeQuit :: IO Bool
areWeQuit = any isJustQuitEvent <$> pollEvents

isJustQuitEvent :: Event -> Bool
isJustQuitEvent (Event _ QuitEvent) = True
isJustQuitEvent _                   = False

reload :: Window -> Surface -> Surface -> IO ()
reload w windowSurface imageSurface = do
  surfaceBlit imageSurface Nothing windowSurface Nothing
  updateWindowSurface w

close :: IO ()
close = do
  delay 2000
  quit

main :: IO ()
main = do
  init >>=
    loop
  close
