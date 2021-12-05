{-# LANGUAGE OverloadedStrings #-}

module Day4
    ( solve
    ) where

import Control.Applicative
import Text.ParserCombinators.ReadP
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.IO as IO
import Data.List (foldl', intersperse)
import Data.Array

input :: String -> IO Text
input fileName = do
    IO.readFile fileName

solve :: IO ()
solve = do
    inp <- (input "data/day4_small.txt")
    print $ drawnNumbers inp
    b0 <- return ( head $ boards inp)
    print b0

drawnNumbers :: Text -> [Text]
drawnNumbers inp = ( (T.splitOn ",")  . head . T.lines) inp


boards :: Text -> [Board]
boards inp = map board $ (tail . (T.splitOn "\n\n" )) inp


board :: Text -> Board
board b =
    let
        ls = T.lines b
        numbers = map (listArray (0, length ls - 1) . (filter ((/=) "") . (T.splitOn " ") . T.strip)) ls
        numbersA = listArray (0, length ls - 1) numbers
        drawn = array (0, length ls - 1) [ (j, array (0, length ls - 1) [ (i, False) | i <- [0 .. length ls - 1]]) | j <- [ 0..length ls - 1]]
    in
    Board numbersA drawn


data Board = Board {
    numbers :: Array Int (Array Int Text),
    drawn :: Array Int (Array Int Bool)
} deriving (Show)

markNumber :: Board -> Text -> Board
markNumber board@(Board numbers drawn) num =
    board
