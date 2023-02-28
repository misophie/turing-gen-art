module Main where

import System.IO
import Text.Read
import Data.Maybe
import TuringMachine
import Render

statesLimit = 5
symbolsLimit = 5

-- 
main :: IO ()
main = 
    do 
        numStates <- intRequest "How many states do you want your turing machine to have?"
        numStates <- checkLimit numStates statesLimit

        numSymbols <- intRequest "How many symbols?"
        numSymbols <- checkLimit numSymbols symbolsLimit

        render <- rendermain (512,512) (blankInit (512,512) (TuringMachine.Symbol 1))

        return render

checkLimit :: Int -> Int -> IO Int
checkLimit num limit
    | num > limit || num < 1 = 
    do
        newNum <- intRequest ("Your input must be between 1 and "++(show limit))
        newNum <- checkLimit newNum limit
        return newNum
    | otherwise = return num

intRequest :: String -> IO Int
intRequest str = 
    do 
        putStrLn str
        ln <- getLine
        let num = readMaybe ln :: Maybe Int
        if num == Nothing
            then do
                num <- intRequest "Your input must be an integer"
                return num
            else return (fromJust num)