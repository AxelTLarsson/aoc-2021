{-# LANGUAGE OverloadedStrings #-}

module Day6
  ( solve,
  )
where

import Control.Applicative
import Data.Array
import Data.List (foldl', intersperse)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO as IO
import Data.Text.Read as Read
import Text.ParserCombinators.ReadP

input :: String -> IO Text
input fileName = do
  IO.readFile fileName

solve :: IO ()
solve = do
  inp <- (input "data/day6_input.txt")
  parsed <- return $ map parse $ T.splitOn "," inp
  fastR <- return $ simFast 256 (initialArray parsed)
  print $ fastR
  print $ foldl (+) 0 fastR

parse :: Text -> Int
parse x =
  (fst . head) $ readP_to_S number (T.unpack x)

digit :: ReadP Char
digit = satisfy (\char -> char >= '0' && char <= '9')

number :: ReadP Int
number = fmap read (many1 digit)

spawn :: Int -> [Int]
spawn 0 = [6, 8]
spawn x = [x - 1]

simulate :: Int -> [Int] -> [Int]
simulate 0 spawned = spawned
simulate days spawned = simulate (days - 1) $ concatMap spawn spawned

initialArray :: [Int] -> Array Int Int
initialArray nums = accumArray (+) 0 (0, 8) $ zip nums [1, 1 ..]

simFast :: Int -> Array Int Int -> Array Int Int
simFast 0 spawned = spawned
simFast days spawned = simFast (days - 1) (spawnFast spawned)

spawnFast :: Array Int Int -> Array Int Int
spawnFast arr =
  let as = assocs arr
      spawned = concatMap spawn' as
   in accumArray (+) 0 (0, 8) spawned

spawn' :: (Int, Int) -> [(Int, Int)]
spawn' (0, count) = [(6, count), (8, count)]
spawn' (timer, count) = [((timer - 1), count)]
