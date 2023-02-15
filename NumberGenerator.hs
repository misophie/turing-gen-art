module NumberGenerator where

    -- from Feb 15 lec - for Blackjack
    -- trand :: IO Bool
    -- trand = do
    --     g <- newStdGen
    --     return (ace_first [min 10 (1 + (abs n) `mod` 13) | n <- (randoms g :: [Int])])

    -- -- no IO for this function even though it is always used in trand which is IO
    -- ace_first :: (Eq a, Num a) => [a] -> Bool
    -- ace_first (1:_) = True
    -- ace_first (10:_) = False
    -- ace_first(_:t) = ace_first t