module Lib
    ( someFunc
    ) where


smallInput :: IO [Int]
smallInput = do
    f <- readFile "data/day1_small.txt"
    return ( map read ( lines f))

largeInput :: IO [Int]
largeInput = do
    f <- readFile "data/day1_input.txt"
    return ( map read ( lines f))

someFunc :: IO ()
someFunc = do
    solution <- fmap solve largeInput
    print $ solution


solve :: [Int] -> (Int, Int)
solve depths =
    foldl (\(prev, c) x -> if x > prev then (x, c + 1) else (x, c)) (0, 0) depths
