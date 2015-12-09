{-# LANGUAGE OverloadedStrings #-}

import SDL
import Data.Array
import Control.Monad
import Control.Monad.State as ST
import Control.Arrow
import Core


main :: IO ()
main = do
  s <- execStateT initState undefined
  s <- execStateT (loadMedia [(DefaultS, "./data/kitty.bmp")]) s

  let loop = do
        event <- fmap eventPayload waitEvent
        evalStateT (reloadWindowSurface DefaultS) s
        unless (event == QuitEvent) loop

  loop

  evalStateT close s
