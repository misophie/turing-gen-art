module TuringMachine where

    -- TODO:
    -- state and symbol should probably be custom data types
    -- action as a function/series of functions
    -- (?) should the turing machine iterate over here or where it is used?
        -- the ref code has program.update updating the states (5000 steps per call) and main.updateRender sets a cap on how many times update is called + renders

    -- programs.js in Maxime's code

    -- machine has:
    -- states - some sort of collection of states the machine can be in
    -- symbols - some sort of collection of symbols (like changing the pixel colour?)
    -- actions - the types of actions the machine can do (i.e. move left, right, up, down)
    -- mapWidth - dimension of the canvas
    -- mapHeight - dimension of the canvas

    -- I'm assuming that the actions should wrap around the map,
    -- so moving right, past the canvas, will land you on the left side

    -- data State
    -- data Symbol
    -- (?) data Action

    -- type Dimensions = 
    -- type TuringMachine = State -> Symbol -> [Action] -> Dimensions -> State
