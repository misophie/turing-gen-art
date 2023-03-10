module TuringMachine where

    -- this code defines:
    -- states - triplet that represents the index in the canvas and the colour of the pixel
    -- symbols - Int that represents the colour (enumerated in Render)
    -- actions - the types of actions the machine can do (i.e. move left, right, up, down)
    -- transitions - pair of state (the new state) and action, indexed through 2d list in transInt
    -- dimensions - pair of int that define the width and height of the canvas
    -- turingmachine - current states in the canvas as a 2d list

    -- (x, y, pixel colour)
    newtype State = State (Int, Int, Symbol)
        -- deriving (Show)

    -- show as a triple
    instance Show State where
        show (State (a, b, c)) = "(" ++ show a ++ ", " ++ show b ++ ", " ++ show c ++ ")"

    -- functions to access the triple
    first :: State -> Int
    first (State (a, _, _)) = a

    second :: State -> Int
    second (State (_, b, _)) = b

    third :: State -> Symbol
    third (State (_, _, c)) = c

    -- pixel value
    newtype Symbol = Symbol Int
        -- deriving (Show)

    instance Show Symbol where
        show (Symbol a) = show a

    -- go from one state to another
    type Action = State -> Dimensions -> TuringMachine -> State
        -- deriving (Show)

    -- Show for functions doesn't really work well, this is for debugging and testing purposes
    instance Show Action where
        show _ = "An action!"

    -- (action to move to the next state, )
    newtype Transition = Transition (State, Action)
        -- deriving (Show)

    instance Show Transition where
        show (Transition (a, b)) = "(" ++ show a ++ "," ++ show b ++ ")"

    fstTrans :: Transition -> State
    fstTrans (Transition (s, _)) = s

    sndTrans :: Transition -> Action
    sndTrans (Transition (_, a)) = a

    -- (height, width)
    type Dimensions = (Int, Int)

    type TuringMachine = [[State]]

    -- defining different actions
    -- each action manipulates either the x or y coordinate to access the next
    -- appropriate state by using the TuringMachine type
    -- and if the action would move out of bounds, wrap around to the other end of the 2D tape

    -- y++
    moveUp :: Action
    moveUp state1 (x, y) machine
        | (second state1 + 1) < y = (machine !! (second state1 + 1)) !! first state1
        | otherwise = head machine !! first state1
    -- Test Cases:
    -- moveUp (State (0, 0, Symbol 0)) (2, 2) [[State (0, 0, Symbol 0), State (1, 0, Symbol 1)], [State (0, 1, Symbol 2), State (1, 1, Symbol 3)]]
    -- returns/shows (0, 1, 2)
    -- moveUp (State (1, 0, Symbol 1)) (2, 2) [[State (0, 0, Symbol 0), State (1, 0, Symbol 1)], [State (0, 1, Symbol 2), State (1, 1, Symbol 3)]]
    -- returns/shows (1, 1, 3)
    -- moveUp (State (0, 1, Symbol 2)) (2, 2) [[State (0, 0, Symbol 0), State (1, 0, Symbol 1)], [State (0, 1, Symbol 2), State (1, 1, Symbol 3)]]
    -- returns/shows (0, 0, 0)
    -- moveUp (State (1, 1, Symbol 3)) (2, 2) [[State (0, 0, Symbol 0), State (1, 0, Symbol 1)], [State (0, 1, Symbol 2), State (1, 1, Symbol 3)]]
    -- returns/shows (1, 0, 1)

    -- y--
    moveDown :: Action
    moveDown state1 (x, y) machine 
        | (second state1 - 1) >= 0 = (machine !! (second state1 - 1)) !! first state1
        | otherwise = (machine !! (y-1)) !! first state1
    -- Test Cases:
    -- moveDown (State (0, 1, Symbol 2)) (2, 2) [[State (0, 0, Symbol 0), State (1, 0, Symbol 1)], [State (0, 1, Symbol 2), State (1, 1, Symbol 3)]]
    -- returns/shows (0, 0, 0)
    -- moveDown (State (1, 1, Symbol 3)) (2, 2) [[State (0, 0, Symbol 0), State (1, 0, Symbol 1)], [State (0, 1, Symbol 2), State (1, 1, Symbol 3)]]
    -- returns/shows (1, 0, 1)
    -- moveDown (State (0, 0, Symbol 0)) (2, 2) [[State (0, 0, Symbol 0), State (1, 0, Symbol 1)], [State (0, 1, Symbol 2), State (1, 1, Symbol 3)]]
    -- returns/shows (0, 1, 2)
    -- moveDown (State (1, 0, Symbol 1)) (2, 2) [[State (0, 0, Symbol 0), State (1, 0, Symbol 1)], [State (0, 1, Symbol 2), State (1, 1, Symbol 3)]]
    -- returns/shows (1, 1, 3)

    -- x--
    moveLeft :: Action
    moveLeft state1 (x, y) machine 
        | (first state1 - 1) >= 0 = (machine !! second state1) !! (first state1 - 1)
        | otherwise = (machine !! second state1) !! (x-1)

    -- Test Cases:
    -- moveLeft (State (1, 0, Symbol 1)) (2, 2) [[State (0, 0, Symbol 0), State (1, 0, Symbol 1)], [State (0, 1, Symbol 2), State (1, 1, Symbol 3)]]
    -- returns/shows (0, 0, 0)
    -- moveLeft (State (1, 1, Symbol 3)) (2, 2) [[State (0, 0, Symbol 0), State (1, 0, Symbol 1)], [State (0, 1, Symbol 2), State (1, 1, Symbol 3)]]
    -- returns/shows (0, 1, 2)
    -- moveLeft (State (0, 0, Symbol 0)) (2, 2) [[State (0, 0, Symbol 0), State (1, 0, Symbol 1)], [State (0, 1, Symbol 2), State (1, 1, Symbol 3)]]
    -- returns/shows (1, 0, 1)
    -- moveLeft (State (0, 1, Symbol 1)) (2, 2) [[State (0, 0, Symbol 0), State (1, 0, Symbol 1)], [State (0, 1, Symbol 2), State (1, 1, Symbol 3)]]
    -- returns/shows (1, 1, 3)

    -- x++
    moveRight :: Action
    moveRight state1 (x, y) machine 
        | (first state1 + 1) < x = (machine !! second state1) !! (first state1 + 1)
        | otherwise = head (machine !! second state1)
    -- Test Cases:
    -- moveRight (State (0, 0, Symbol 0)) (2, 2) [[State (0, 0, Symbol 0), State (1, 0, Symbol 1)], [State (0, 1, Symbol 2), State (1, 1, Symbol 3)]]
    -- returns/shows (1, 0, 1)
    -- moveRight (State (0, 1, Symbol 1)) (2, 2) [[State (0, 0, Symbol 0), State (1, 0, Symbol 1)], [State (0, 1, Symbol 2), State (1, 1, Symbol 3)]]
    -- returns/shows (1, 1, 3)
    -- moveRight (State (1, 0, Symbol 1)) (2, 2) [[State (0, 0, Symbol 0), State (1, 0, Symbol 1)], [State (0, 1, Symbol 2), State (1, 1, Symbol 3)]]
    -- returns/shows (0, 0, 0)
    -- moveRight (State (1, 1, Symbol 3)) (2, 2) [[State (0, 0, Symbol 0), State (1, 0, Symbol 1)], [State (0, 1, Symbol 2), State (1, 1, Symbol 3)]]
    -- returns/shows (0, 1, 2)

    -- initialize the Turing Machine given the same symbol across the canvas
    blankInit :: Dimensions -> Symbol -> TuringMachine
    blankInit dims symbol = [[State (x, y, symbol) | x <- [0..fst dims]] | y <- [0..snd dims]]

    -- initialize a 2d list of transitions using the 
    -- current turingmachine states, actions that can be taken, a function to change the symbols appropriately, 
    -- the dimensions of the canvas, an index (for generation purposes)
    transInit :: TuringMachine -> [[Action]] -> [[Symbol -> Symbol]] -> Dimensions -> (Int, Int) -> [[Transition]]
    transInit [[]] [[]] [[]] _ _ = [[]]
    transInit states actions rules dims index
        | fst index + 1 >= fst dims && snd index + 1 >= snd dims = [[Transition (newState, action)]]
        | notWrapped = [[Transition (newState, action)] ++ head (transInit states actions rules dims ((fst index) + 1, snd index))] ++ tail (transInit states actions rules dims ((fst index) + 1, snd index))
        | otherwise = [[Transition (newState, action)]] ++ transInit states actions rules dims (0, snd index + 1)
                            where 
                                currState = (states !! snd index) !! fst index
                                newState = State (first currState, second currState, rule (third currState))
                                rule = (rules !! snd index) !! fst index
                                action = (actions !! snd index) !! fst index
                                notWrapped = (fst index + 1) < fst dims
    
    -- Test Cases:
    sub1 :: Symbol -> Symbol
    sub1 (Symbol x) = Symbol (x - 1)
    -- transInit [[]] [[]] [[]] (0,0) (0,0)
    -- machine = [[State (0, 0, Symbol 0), State (1, 0, Symbol 1)], [State (0, 1, Symbol 2), State (1, 1, Symbol 3)]]
    -- actions = [[moveRight, moveLeft], [moveUp, moveDown]]
    -- symbolFns = [[sub1, sub1], [sub1, sub1]]
    -- dims = (2, 2)
    -- index = (0, 0)
    -- transInit [[State (0, 0, Symbol 0), State (1, 0, Symbol 1)], [State (0, 1, Symbol 2), State (1, 1, Symbol 3)]] [[moveRight, moveLeft], [moveUp, moveDown]] [[sub1, sub1], [sub1, sub1]] (2, 2) (0, 0)
    -- transInit machine actions symbolFns dims index