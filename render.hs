-- import Graphics.Rasterific
import Reanimate
import 
import NumberGenerator
module Render where

test :: IO()
test = Reanimate $ addStatic (mkBackground "cyan") $ staticFrame 1 $ mkText "Hello world"

--renderAnim = return ()
-- renderImg = return ()

-- setScreenWidth :: IO() -> Width
-- setScreenWidth = do {Reanimate.Width <- getLine} -- if custom

-- setScreenHeight :: IO() -> Height
-- setScreenHeight = do {Reanimate.Height <- getLine} -- if custom



-- are we using rasterific or reanimate or both?
  -- think i prefer reanimate since it has useful built in constructors/svg output

-- REANIMATE TO-DOs
-- constructor for rgb in package 
  -- colormaps are an option (returns a color from a specific palette for a given no_
     -- NumberGenerator -> colormap of choice -> generate PixelRGB8
     -- feed PixelRGB8 to image rendering
-- set screen constants (screenWidth/Height/TBLR/set dpi conversion)
-- one scene per turing randomization generated
-- convert scene -> animation

--

-- color maps
-- generateColor f = f (randomnum 0 1) -- where f -> Reanimate.magma/turbo etc depending on palette of choice, random num placeholder,
-- range 0 - 1 inclusive