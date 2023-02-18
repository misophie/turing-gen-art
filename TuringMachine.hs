module TuringMachine where

    -- TODO:
    -- (?) should the turing machine iterate over here or where it is used?
    -- representation of symbol according to reanimate requirements
    -- write some test cases for the 4 diff actions + basic init
    -- figure out how to also intialize the transitions along with the Turing Machine
    -- implement wrapping in move using dimensions

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
    -- newtype State = State (Int, Int, Symbol)
    newtype State = State (Int, Int, Symbol)
        -- deriving (Show)
    -- (?) deriving (Eq)

    -- show as a triple
    instance Show State where
        show (State (a, b, c)) = "(" ++ show a ++ ", " ++ show b ++ ", " ++ show c ++ ")"

    -- functions to access the triple
    first :: (a, b, c) -> a
    first (a, _, _) = a

    second :: (a, b, c) -> b
    second (_, b, _) = b

    third :: (a, b, c) -> c
    third (_, _, c) = c

    -- pixel value
    newtype Symbol = Symbol Int -- stub for symbol, figure out what symbol is representing (RGB pixel?)
        -- deriving (Show)
    -- (?) 

    instance Show Symbol where
        show (Symbol a) = show a

    -- go from one state to another
    type Action = State -> State

    -- given a start state, new state, 
    -- and action to choose the new start state using the bounds of the canvas (wrap around if out of bounds), 
    -- return the new start state
    type Transition = State -> State -> Action -> Dimensions -> State

    -- (height, width)
    type Dimensions = (Int, Int)

    -- 
    -- type TuringMachine = State -> [Transition] -> State
    type TuringMachine = [[State]]

    -- defining different actions

    -- y++
    -- moveUp :: Action
    -- moveUp (State state1) = State ((fst (fst state1), snd (fst state1) + 1), snd state1)
    moveUp :: State -> TuringMachine -> State
    moveUp (State state1) machine = (machine !! (second state1 + 1)) !! first state1
    -- Test Cases:
    -- moveUp (State (0, 0, Symbol 0)) [[State (0, 0, Symbol 0), State (1, 0, Symbol 1)], [State (0, 1, Symbol 2), State (1, 1, Symbol 3)]]
    -- returns/shows (0, 1, 2)
    -- moveUp (State (1, 0, Symbol 1)) [[State (0, 0, Symbol 0), State (1, 0, Symbol 1)], [State (0, 1, Symbol 2), State (1, 1, Symbol 3)]]
    -- returns/shows (1, 1, 3)

    -- y--
    -- moveDown :: Action
    -- moveDown (State state1) = State ((fst (fst state1), snd (fst state1) - 1), snd state1)
    moveDown :: State -> TuringMachine -> State
    -- moveDown (State state1) machine = (machine !! first state1) !! (second state1 - 1)
    moveDown (State state1) machine = (machine !! (second state1 - 1)) !! first state1
    -- Test Cases:
    -- moveUp (State (0, 1, Symbol 2)) [[State (0, 0, Symbol 0), State (1, 0, Symbol 1)], [State (0, 1, Symbol 2), State (1, 1, Symbol 3)]]
    -- returns/shows (0, 0, 0)
    -- moveUp (State (1, 1, Symbol 3)) [[State (0, 0, Symbol 0), State (1, 0, Symbol 1)], [State (0, 1, Symbol 2), State (1, 1, Symbol 3)]]
    -- returns/shows (1, 1, 3)

    -- x--
    -- moveLeft :: Action
    -- moveLeft (State state1) = State ((fst (fst state1) - 1, snd (fst state1)), snd state1)
    moveLeft :: State -> TuringMachine -> State
    moveLeft (State state1) machine = (machine !! (first state1 - 1)) !! second state1

    -- x++
    -- moveRight :: Action
    -- moveRight (State state1) = State ((fst (fst state1) + 1, snd (fst state1)), snd state1)
    moveRight :: State -> TuringMachine -> State
    moveRight (State state1) machine = (machine !! (first state1 + 1)) !! second state1


    -- initialize the Turing Machine given the same symbol across the canvas
    blankInit :: Dimensions -> Symbol -> TuringMachine
    -- blankInit dims symbol = [[symbol] * second dims] * first dims
    blankInit dims symbol = [[State (x, y, symbol) | x <- [0..fst dims]] | y <- [0..snd dims]]