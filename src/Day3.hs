{-# LANGUAGE OverloadedStrings #-}

module Day3
    ( solve, bintodec
    ) where

import Control.Applicative
import Text.ParserCombinators.ReadP
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.IO as IO
import Data.List (foldl')

input :: String -> IO [Text]
input fileName = do
    f <- IO.readFile fileName
    return ( T.lines f)

solve :: IO ()
solve = do
    nums <- (input "data/day3_input.txt")
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

cols :: [Text] -> [Text]
cols =
    T.transpose

mostCommon :: Text -> Text
mostCommon x = if T.count "1" x > T.count "0" x then "1" else "0"

leastCommon :: Text -> Text
leastCommon x = if T.count "1" x > T.count "0" x then "0" else "1"


bintodec :: [Text] -> Int
bintodec bs = fst $ foldr (\b (acc, i) -> if b == "1" then (acc + 2^i, i+1) else (acc, i+1)) (0,0) bs

