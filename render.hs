-- import Graphics.Rasterific
import Reanimate
module render where

renderAnim = return ()
renderImg = return ()

-- are we using rasterific or reanimate or both?
  -- think i prefer reanimate since it has useful built in constructors/svg output
  -- limited to gifs with rasterific and we'll need special architecture to build frame-by-frame execution (rasterific.cacheDrawing is an option)
-- are we creating a new module for the rendering or is it all in main like main.js in the ref project?

-- REANIMATE TO-DOs
-- constructor for rgb in package 
  -- colormaps are an option (returns a color from a specific palette for a given no_
     -- NumberGenerator -> colormap of choice -> generate PixelRGB8
     -- feed PixelRGB8 to image rendering
-- set screen constants (screenWidth/Height/TBLR/set dpi conversion)
-- one scene per turing randomization generated
-- convert scene -> animation

-- RASTERIFIC TO-DOs
-- fn to generate color from rgb val (randomly generated) - fill function
-- fn to transition --> withTransition ?
-- fn to render --> render/cacheDrawing

-- data rgb = rgb Int Int Int -- using int on the assumption that no frac are generated, get values from 
