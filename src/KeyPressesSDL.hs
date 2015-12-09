{-# LANGUAGE OverloadedStrings #-}

import SDL
import Control.Monad (unless)
import Control.Monad.State
import Core


data Action = UpKey | DownKey | Quit | Other deriving (Eq)

getAction :: EventPayload -> Action
getAction QuitEvent = Quit
getAction (KeyboardEvent KeyboardEventData
                  { keyboardEventKeyMotion = Pressed
                  , keyboardEventKeysym = Keysym k _ _}) =
  case k of
  ScancodeUp   -> UpKey
  ScancodeDown -> DownKey
  _            -> Other
getAction _ = Other


main :: IO ()
main = do
  s <- execStateT initState undefined
  s <- execStateT (loadMedia [ (DefaultS, "./data/kitty.bmp")
                             , (ImageUpS, "./data/kitty-up.bmp")
                             , (ImageDnS, "./data/kitty-down.bmp")
                             ]) s

  let loop = do
        event <- fmap eventPayload waitEvent
        evalStateT (reloadWindowSurface
          (case getAction event of
            UpKey   -> ImageUpS
            DownKey -> ImageDnS
            _       -> DefaultS
          )) s

        unless (event == QuitEvent) loop

  loop

  evalStateT close s
