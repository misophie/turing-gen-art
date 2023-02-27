module NumberGenerator where

    import System.IO
    import System.Random

    -- randInt m enerates a random number between 1 and m
    randInt :: Int -> IO Int
    randInt m = 
        do
            g <- newStdGen
            return (fst (randomR (1,m) g))
