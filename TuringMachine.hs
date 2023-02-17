module TuringMachine where

    -- TODO:
    -- state and symbol should probably be custom data types
    -- action as a function/series of functions
    -- (?) should the turing machine iterate over here or where it is used?

    -- programs.js in Maxime's code

    -- machine has:
    -- states - some sort of collection of states the machine can be in
    -- symbols - some sort of collection of symbols (like changing the pixel colour?)
    -- actions - the types of actions the machine can do (i.e. move left, right, up, down)
    -- transitions - from state and symbol to new state and symbol, then do transition
    -- mapWidth - dimension of the canvas
    -- mapHeight - dimension of the canvas

    -- I'm assuming that the actions should wrap around the map,
    -- so moving right, past the canvas, will land you on the left side

    -- ((x, y) pixel value)
    type State = ((Integer, Integer), Symbol)
    -- (?) deriving (Eq)

    -- pixel value
    type Symbol = Integer -- stub for symbol, figure out what symbol is representing (RGB pixel?)
    -- (?) 

    -- go from one state to another
    type Action = State -> State

    -- given a start state, new state, and action to choose the new start state using the bounds of the canvas, 
    -- return the new start state
    type Transition = State -> State -> Action -> Dimensions -> State

    -- (height, width)
    type Dimensions = (Integer, Integer)

    -- go from one state to another using a transition
    type TuringMachine = State -> [Transition] -> State
