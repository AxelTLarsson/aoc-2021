module Day1
    ( part1,
      part2
    ) where


smallInput :: IO [Int]
smallInput = do
    f <- readFile "data/day1_small.txt"
    return ( map read ( lines f))

largeInput :: IO [Int]
largeInput = do
    f <- readFile "data/day1_input.txt"
    return ( map read ( lines f))

part1 :: IO ()
part1 = do
    solution <- fmap solve largeInput
    print $ solution

part2 :: IO ()
part2 = do
    windows <- fmap  (slidingWindow []) largeInput
    -- print $ windows
    print $ solve windows


solve :: [Int] -> (Int, Int)
solve depths =
    foldl (\(prev, c) x -> if x > prev then (x, c + 1) else (x, c)) (0, 0) depths


slidingWindow :: [Int] -> [Int] -> [Int]
slidingWindow sofar (x:y:z:xs) =
    slidingWindow (sofar ++ [ x + y + z ]) ([y, z] ++ xs)
slidingWindow sofar xs =
    sofar
