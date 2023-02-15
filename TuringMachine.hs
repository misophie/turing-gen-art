module TuringMachine where

    -- programs.js in Maxime's code

    -- machine has:
    -- states - some sort of collection of states the machine can be in
    -- symbols - some sort of collection of symbols (like changing the pixel colour?)
    -- actions - the types of actions the machine can do (i.e. move left, right, up, down)
    -- mapWidth - dimension of the canvas
    -- mapHeight - dimension of the canvas

    -- I'm assuming that the actions should wrap around the map,
    -- so moving right, past the canvas, will land you on the left side

    -- should the turing machine iterate over here or where it is used?

    -- functions for each action