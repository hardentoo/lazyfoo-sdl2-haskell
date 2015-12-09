{-# LANGUAGE OverloadedStrings #-}

import SDL
import Data.Array
import Control.Monad.State
import Control.Arrow
import Core


main :: IO ()
main = do
  s <- execStateT initState undefined
  s <- execStateT (loadMedia [(DefaultS, "./data/kitty.bmp")]) s
  s <- execStateT (reloadWindowSurface DefaultS) s
  delay 2000
  evalStateT close s
