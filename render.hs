module Render where

  import Reanimate
  import Reanimate.Parameters
  import Reanimate.Scene
  import Reanimate.ColorMap
  import NumberGenerator
  import TuringMachine  

  -- TURING MACHINE FULL ANIMATION MAIN
  -- basic main function for accessing animation render. variables and datazxcv 
  -- from turing machine algorithm:
  -- a. dimensions from Dimensions data type. fst -> width and snd -> height
  -- b. states from TuringMachine data type. TuringMachine is a list of lists of states. 
  -- creates blank canvas and sets colour, dimensions of canvas from Dimensions data type
  -- the purpose of main is the sequential animation of each list of states one after
  -- another by calling transitionAnim on each of the lists in TuringMachine.
  -- the sequential animation is done through seqA

  -- TBD: conversion to scene needed? only there for play to remove the animation once its
  -- done but seqA also does that.
  -- use fold for seqA ?
  rendermain :: Duration -> Dimensions -> TuringMachine -> IO ()
  rendermain dur dims machine = reanimate $ mkAnimation dur $ scene $ do 
    $ withViewBox(0, 0, (fst dims), (snd dims)) $ mkBackground "black" $ 
    play $ seqA $ map (transitionAnim 1) machine
  -- output: SVG file with black canvas and Turing output.
  -- TO CALL:
  -- main Double (Int, Int) blankInit 

  -- **in ghci. 
  -- **duration in seconds. for duration --> 120s or less recommended.
  -- **dimensions in pixels. for pixels --> 256/512 recommended
  -- **make sure to have the full reanimate library installed or cloned. 

  -- test cases:
  -- rendermain 120 (512, 512) blankInit
  -- rendermain 60 (256, 256) blankInit

  -- LIST OF STATES ANIMATION PER FRAME (generates transition as a frame)
  -- generate animation for a list of states of every symbol present and return it as an 
  -- animation to main. creates a transition since it changes the states of the Turing Machine 
  -- from one list of states to the consecutive one. 
  -- the list of states represents the state of the entire Turing Machine at one point in
  -- time or one frame. in main: assume 1 frame lasts 1s. can change later. 
  -- since the animations for changing the states of all symbols on screen happens 
  -- simultaneously, use fork for this animation.
  transitionAnim :: Duration -> [State] -> Animation
  transitionAnim dur lostates = mkAnimation dur $ scene $ fork 
    $ play $ mkGroup $ map (stateTransition 1) lostates

  -- STATE TRANSITION
  -- generate a new sprite for the given state as specified for this particular frame. call 
  -- symbol transition to generate a new symbol at the given coordinates and with the 
  -- specified change in color. 
  -- TBD: verify functionality of oTranslate, need animation ? 
  -- generate a list of symbols beforehand so we don't get random generation?
  stateTransition :: Duration -> State -> Animation
  stateTransition dur currstate = do $ scene $ newSprite $ mkCircle 1 $ 
    oTranslate (V2 (first currstate) (second currstate)) $ 
    withFillColorPixel $ generateSymbol  (third currstate)

  -- takes an input of Symbol type and extracts the number value that represents the symbol 
  -- and uses Reanimate's ColorMap functions to generate a pixel with a color that corresponds 
  -- to the given [0,1] double value. this will be the representation for that symbol in 
  -- rendering.

  -- TODO figure out if the [0,1] double representation actually works with the TuringMachine
  -- functionality or if we will have to devise some kind of calculation to convert it to the 
  -- required representation.
  generateSymbol :: Symbol -> PixelRGB8
  generateSymbol (symb dbl) = turbo dbl 
  -- assuming Symbol is of type (Symbol Double) containing a double value between 0 and 1. 
  -- can adjust as needed.
