module Render where

  import Reanimate
  import Reanimate.Parameters
  import Reanimate.Scene
  import Reanimate.ColorMap
  import NumberGenerator
  import TuringMachine  

  -- basic main function for accessing animation render. variables and data
  -- from turing machine algorithm TBA.
  -- creates blank canvas and sets colour, dimensions of canvas
  -- initialises canvas animation function turingcanvas with max duration of play
  -- and function for animating symbols (input for states and symbols thru main or later?)
  main :: [State] -> IO ()
  main states = reanimate $ scene $ do 
    newSpriteSVG_ $ mkBackground "black" $ setWidth mapWidth $ setHeight mapHeight 
    fork $ play $ map (transitionAnim 60) states 
  -- output: SVG file with black canvas and Turing output. call in Main module to render?

  -- generates a group of animations for all symbols received from the turing machine according
  -- to initial state
  -- initialises symbols as PixelRGB8 represenations using the generateSymbol function
  -- TODO expand stub with functionality for full list -- check if it is compatible with
  -- Turing functionality. figure out how to call symbTransition
  -- use parLoopA for simultaneous animation for all symbs?

  -- generate animation for a single symbol/render using generateSymbol -> call for each
  -- symbol on State list and return to symbsAnimation as mkGroup ; might be easier than 
  -- doing it all at once in symbsAnimation
  transitionAnimation :: State -> Animation

  -- (?) 
  -- generates a group of animations for the transitions of symbols from one state to another.
  -- TODO expand stub, integrate into symbAnimation, figure out delay and how to implement 
  -- TuringMachine functionality with this code
  -- symbsTransition :: [Transition] -> Animation
  -- symbsTransition lst = mkAnimation 600    

  -- generate animation for a single symbol for one transition and return to symbsTransition
  -- using mkGroup; like symbAnimation might be easier than doing it all at once
  -- symbTransition :: Transition -> Animation

  -- REDUNDANT WITH FORK
  -- compiles the diff symbol animations onto one canvas and returns it until duration > 0
  -- duration currently dependent on one of the symbols' full duration to traverse canvas, but
  -- might change to set dur since we plan on wrapping around the canvas
  -- TODO expand stub
  -- turingCanvas :: Double -> Animation -> Animation
  -- turingCanvas dur animlst = mkAnimation (duration (last animlst)) 

  -- takes an input of Symbol type and extracts the number value that represents the symbol and
  -- uses Reanimate's ColorMap functions to generate a pixel with a color that corresponds to the
  -- given [0,1] double value. this will be the representation for that symbol in rendering.
  -- TODO figure out if the [0,1] double representation actually works with the TuringMachine
  -- functionality or if we will have to devise some kind of calculation to convert it to the 
  -- required representation.
  generateSymbol :: Symbol -> PixelRGB8
  generateSymbol (symb dbl) = turbo dbl -- assuming Symbol is of type (Symbol Double) containing a
                                        -- double value between 0 and 1. can adjust as needed.
