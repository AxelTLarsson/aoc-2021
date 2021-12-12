{-# LANGUAGE OverloadedStrings #-}

module Day3
    ( solve, bintodec, solve2
    ) where

import           Control.Applicative
import           Data.List                    (foldl', intersperse)
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Data.Text.IO                 as IO
import           Text.ParserCombinators.ReadP

input :: String -> IO [Text]
input fileName = do
    f <- IO.readFile fileName
    return ( T.lines f)

solve :: IO ()
solve = do
    nums <- (input "data/day3_small.txt")
    t <- return (cols nums)
    gamma <- return (map mostCommon t)
    gammaString <- return (T.unpack $ foldl' T.append "" gamma)
    epsilon <- return (map leastCommon t)
    epsilonString <- return (T.unpack $ foldl' T.append "" $ epsilon)
    print $ "gamma: " ++ (gammaString)
    print $ bintodec gamma
    print $ "epsilon: " ++ (epsilonString)
    print $ bintodec epsilon
    print $ (bintodec gamma) * (bintodec epsilon)


solve2 :: IO ()
solve2 = do
    nbrs <- (input "data/day3_input.txt")
    mostCommons <- return (map mostCommon ( cols nbrs))
    oxygen <- return (oxygenRating nbrs 0)
    print oxygen
    print $ bintodecS $  T.unpack oxygen
    c02 <- return (c02Rating nbrs 0)
    print c02
    print $ bintodecS $  T.unpack c02
    print $ (bintodecS $  T.unpack c02) * (bintodecS $  T.unpack oxygen)

cols :: [Text] -> [Text]
cols =
    T.transpose

mostCommon :: Text -> Text
mostCommon x = if T.count "1" x >= T.count "0" x then "1" else "0"

leastCommon :: Text -> Text
leastCommon x = if T.count "1" x < T.count "0" x then "1" else "0"

bintodec :: [Text] -> Int
bintodec bs = fst $ foldr (\b (acc, i) -> if b == "1" then (acc + 2^i, i+1) else (acc, i+1)) (0,0) bs

bintodecS :: String -> Int
bintodecS bs = fst $ foldr (\b (acc, i) -> if b == '1' then (acc + 2^i, i+1) else (acc, i+1)) (0,0) bs

oxygenRating :: [Text] ->  Int -> Text
oxygenRating (x:[]) _ = x
oxygenRating nbrs bitIdx =
    let
        mostCommons = map mostCommon $ cols (map (T.drop bitIdx) nbrs)
        most = head $ mostCommons
        kept = (filter (\x -> T.singleton (T.index x bitIdx) == most) nbrs)
    in
        oxygenRating kept  (bitIdx+1)


c02Rating :: [Text] ->  Int -> Text
c02Rating (x:[]) _ = x
c02Rating nbrs bitIdx =
    let
        leastCommons = map leastCommon $ cols (map (T.drop bitIdx) nbrs)
        least = head $ leastCommons
        kept = (filter (\x -> T.singleton (T.index x bitIdx) == least) nbrs)
    in
        c02Rating kept (bitIdx+1)

