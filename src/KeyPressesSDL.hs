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

loadMedia :: IO [Surface]
loadMedia = mapM loadBMP
  ["./data/kitty.bmp", "./data/kitty-up.bmp", "./data/kitty-down.bmp"]

reloadMedia :: Window -> Surface -> Surface -> IO ()
reloadMedia w windowSurface imageSurface = do
  surfaceBlit imageSurface Nothing windowSurface Nothing
  updateWindowSurface w

close :: IO ()
close = do
  quit

data Action = UpKey | DownKey | Quit | Other deriving (Eq)

getAction :: Event -> Action
getAction (Event {eventPayload = QuitEvent}) = Quit
getAction (Event {eventPayload = KeyboardEvent KeyboardEventData
                  { keyboardEventKeyMotion = Pressed
                  , keyboardEventKeysym = Keysym k _ _}}) =
  case k of
  ScancodeUp   -> UpKey
  ScancodeDown -> DownKey
  _            -> Other
getAction _ = Other


main :: IO ()
main = do
  window <- init
  windowSurface <- getWindowSurface window
  [defaultSurface, upSurface, downSurface] <- loadMedia

  t <- time
  timeRef <- newIORef t

  let sense   = (\_ -> do
        t' <- readIORef timeRef
        let dt = t' - t
        writeIORef timeRef dt
        pollEvent >>= return . (,) dt)
      actuate = (\_ action -> reloadMedia window windowSurface
                                (case action of
                                  UpKey   -> upSurface
                                  DownKey -> downSurface
                                  _       -> defaultSurface)
                            >> return (action == Quit))
      sf      = arr getAction

  reactimate waitEvent sense actuate sf
  close
