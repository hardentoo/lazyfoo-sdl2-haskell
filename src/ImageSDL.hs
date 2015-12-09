{-# LANGUAGE OverloadedStrings #-}

import SDL
import Control.Monad.State
import Core


main :: IO ()
main = do
  s <- execStateT initState undefined
  s <- execStateT (loadMedia [(DefaultS, "./data/kitty.bmp")]) s
  s <- execStateT (reloadWindowSurface DefaultS) s
  delay 2000
  evalStateT close s
