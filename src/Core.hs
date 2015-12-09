{-# LANGUAGE OverloadedStrings #-}

module Core where

import qualified SDL
import Linear (V2(..))
import qualified Data.Array as A
import qualified Control.Monad.State as ST
import Control.Arrow
import Foreign.Ptr (nullPtr)


data SurfaceName = WindowS | ImageUpS | ImageDnS | DefaultS
  deriving (Eq, Ord, Enum)

instance A.Ix SurfaceName where
  range   (from, to)   = [from..to]
  inRange (from, to) v = v >= from && v <= to
  index   (from, to) v = go 0 [from..to]
    where go i (x:xs) = if x == v then i else go (i + 1) xs
          go _ _      = -1

type Surfaces   = A.Array SurfaceName SDL.Surface
type VideoState = ST.StateT (SDL.Window, Surfaces) IO


initState :: VideoState ()
initState = do
  SDL.initialize [SDL.InitVideo]

  window <- SDL.createWindow "SDL Window" 
    SDL.defaultWindow { SDL.windowInitialSize = V2 600 480 }
  windowSurface <- SDL.getWindowSurface window

  ST.put $ (window, A.accumArray (flip const)
                                 (SDL.Surface nullPtr Nothing)
                                 (WindowS, DefaultS)
                                 [(WindowS, windowSurface)])

loadMedia :: [(SurfaceName, String)] -> VideoState ()
loadMedia surfaces = do
  -- TODO:
  loadedSurfaces <- mapM (\(a, b) -> (,) a <$> SDL.loadBMP b) surfaces
  ST.modify $ second (A.// loadedSurfaces)

reloadWindowSurface :: SurfaceName -> VideoState ()
reloadWindowSurface surface = do
  (window, surfaces) <- ST.get
  SDL.surfaceBlit (surfaces A.! surface) Nothing (surfaces A.! WindowS) Nothing
  SDL.updateWindowSurface window

close :: VideoState ()
close = do
  (window, surfaces) <- ST.get
  mapM_ SDL.freeSurface (A.elems surfaces)
  SDL.destroyWindow window
  SDL.quit
