{-# LANGUAGE OverloadedStrings #-}

import SDL
import Linear
import Data.Array
import Control.Monad
import qualified Control.Monad.State as ST
import Core
import Control.Arrow


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
  s <- ST.execStateT initState undefined
  s <- ST.execStateT (loadMedia [ (DefaultS, "./data/kitty.bmp")
                                , (ImageUpS, "./data/kitty-up.bmp")
                                , (ImageDnS, "./data/kitty-down.bmp")
                                ]) s

  let loop = do
        event <- fmap eventPayload waitEvent
        ST.evalStateT (reloadWindowSurface
          (case getAction event of
            UpKey   -> ImageUpS
            DownKey -> ImageDnS
            _       -> DefaultS
          )) s

        unless (event == QuitEvent) loop

  loop

  ST.evalStateT close s
