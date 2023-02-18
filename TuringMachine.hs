module TuringMachine where

    -- TODO:
    -- (?) should the turing machine iterate over here or where it is used?
    -- create some initialization fn
    -- representation of symbol according to reanimate requirements

    -- programs.js in Maxime's code

    -- machine has:
    -- states - some sort of collection of states the machine can be in
    -- symbols - some sort of collection of symbols (like changing the pixel colour?)
    -- actions - the types of actions the machine can do (i.e. move left, right, up, down)
    -- transitions - from state and symbol to new state and symbol, then do transition
    -- mapWidth - dimension of the canvas
    -- mapHeight - dimension of the canvas

    -- ((x, y) pixel value)
    -- newtype State = State ((Integer, Integer), Symbol)
    -- (x, y, pixel)
    type State = (Int, Int, Symbol)
    -- (?) deriving (Eq)

    first :: (a, b, c) -> a
    first (a, _, _) = a

    second :: (a, b, c) -> b
    second (_, b, _) = b

    third :: (a, b, c) -> c
    third (_, _, c) = c

    -- pixel value
    data Symbol = Integer -- stub for symbol, figure out what symbol is representing (RGB pixel?)
    -- (?) 

    -- go from one state to another
    type Action = State -> State

    -- given a start state, new state, 
    -- and action to choose the new start state using the bounds of the canvas (wrap around if out of bounds), 
    -- return the new start state
    type Transition = State -> State -> Action -> Dimensions -> State

    -- (height, width)
    type Dimensions = (Integer, Integer)

    -- 
    -- type TuringMachine = State -> [Transition] -> State
    type TuringMachine = [[State]]

    -- defining different actions

    -- y++
    -- moveUp :: Action
    -- moveUp (State state1) = State ((fst (fst state1), snd (fst state1) + 1), snd state1)
    moveUp :: State -> TuringMachine -> State
    moveUp state1 machine = (machine !! first state1) !! (second state1 + 1)

    -- y--
    -- moveDown :: Action
    -- moveDown (State state1) = State ((fst (fst state1), snd (fst state1) - 1), snd state1)
    moveDown :: State -> TuringMachine -> State
    moveDown state1 machine = (machine !! first state1) !! (second state1 - 1)

    -- x--
    -- moveLeft :: Action
    -- moveLeft (State state1) = State ((fst (fst state1) - 1, snd (fst state1)), snd state1)
    moveLeft :: State -> TuringMachine -> State
    moveLeft state1 machine = (machine !! (first state1 - 1)) !! second state1

    -- x++
    -- moveRight :: Action
    -- moveRight (State state1) = State ((fst (fst state1) + 1, snd (fst state1)), snd state1)
    moveRight :: State -> TuringMachine -> State
    moveRight state1 machine = (machine !! (first state1 + 1)) !! second state1


