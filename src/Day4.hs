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

input :: String -> IO Text
input fileName = do
    IO.readFile fileName

solve :: IO ()
solve = do
    inp <- (input "data/day4_small.txt")
    print $ drawnNumbers inp
    print $ boards inp

drawnNumbers :: Text -> [Text]
drawnNumbers inp = ( (T.splitOn ",")  . head . T.lines) inp


boards :: Text -> [[ Text ]]
boards inp = map board $ (tail . (T.splitOn "\n\n" )) inp


board :: Text -> [Text]
board b =
    map T.strip $ T.lines b
