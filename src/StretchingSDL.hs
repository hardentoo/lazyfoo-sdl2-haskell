{-# LANGUAGE OverloadedStrings #-}

import SDL
import Control.Monad (unless)
import Control.Monad.State
import Core


main :: IO ()
main = do
  s <- execStateT initState undefined
  s <- execStateT (loadMedia [(DefaultS, "./data/kitty.bmp")]) s
  s <- execStateT (convertSurfaces DefaultS) s

  let loop = do
        event <- fmap eventPayload waitEvent
        execStateT (reloadWindowSurface DefaultS) s
        unless (event == QuitEvent) loop
  loop

  evalStateT close s
