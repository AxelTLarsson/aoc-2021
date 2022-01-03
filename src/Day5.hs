{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}

module Day5
  ( solve,
    coordinatesFromLineSegment,
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
  inp <- (input "data/day5_input.txt")
  lineSegments <- return $ fmap parse (T.lines inp)
  filteredLineSegments <- return $ filter diagonal lineSegments
  coordinates <- return $ concatMap coordinatesFromLineSegment lineSegments
  print $ lineSegments
  vents <- return $ updateVents coordinates Map.empty
  overlaps <- return $ length $ Map.filter (> 1) vents
  print overlaps

parse :: Text -> LineSegment
parse x = (fst . head) $ readP_to_S lineSegmentParser (T.unpack x)

digit :: ReadP Char
digit = satisfy (\char -> char >= '0' && char <= '9')

number :: ReadP Int
number = fmap read (many1 digit)

type Coordinate = (Int, Int)

coordinateParser :: ReadP Coordinate
coordinateParser = do
  x <- number
  string ","
  y <- number
  return $ (x, y)

type LineSegment = (Coordinate, Coordinate)

lineSegmentParser :: ReadP LineSegment
lineSegmentParser = do
  c1 <- coordinateParser
  string " -> "
  c2 <- coordinateParser
  eof
  return $ (c1, c2)

{-
 - groupby on the list of coordinates
 -}
updateVents :: [Coordinate] -> Map.Map Coordinate Int -> Map.Map Coordinate Int
updateVents (c : cs) m =
  let updatedMap =
        Map.alter
          -- Something like this might be more elegant:  Maybe.withDefault 1 >> Maybe.map (+1)
          ( \x -> case x of
              Just v -> Just (v + 1)
              Nothing -> Just 1
          )
          c
          m
   in updateVents cs updatedMap
updateVents [] m =
  m

diagonal :: LineSegment -> Bool
diagonal ((x1, y1), (x2, y2)) =
  not (x1 == x2 || y1 == y2)

coordinatesFromLineSegment :: LineSegment -> [Coordinate]
coordinatesFromLineSegment ls@(((x1, y1), (x2, y2))) =
  let xs =
        if x2 < x1
          then reverse [x2 .. x1]
          else [x1 .. x2]
      ys =
        if y2 < y1
          then reverse [y2 .. y1]
          else [y1 .. y2]
   in if diagonal ls
        then [(x, y) | x <- xs | y <- ys]
        else concat [[(x, y) | x <- xs] | y <- ys]
