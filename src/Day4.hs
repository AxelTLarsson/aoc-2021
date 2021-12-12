{-# LANGUAGE OverloadedStrings #-}

module Day4
  ( solve,
    bingo,
    unmarkedSum,
  )
where

import Control.Applicative
import Data.Array
import Data.List (foldl', intersperse)
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
  inp <- (input "data/day4_input.txt")
  numbers <- return $ drawnNumbers inp
  bs <- return $ boards inp
  winner <- return (play bs [] numbers)
  print $ "Winner"
  print $ winner
  print $ score ((head . fst) winner) (snd winner)
  print $ "Loser"
  loser <- return (playToLose bs [] numbers)
  print $ loser
  print $ score ((head . fst) loser) (snd loser)

play :: [Board] -> [Text] -> [Text] -> ([Board], [Text])
play bs drawn [] = (bs, drawn)
play bs drawn leftToDraw =
  let bingos = map (bingo $ drawn) bs
      winningBs = filter (bingo $ drawn) bs
   in if any id bingos
        then play winningBs drawn []
        else play bs (drawn ++ [head leftToDraw]) (tail leftToDraw)

playToLose :: [Board] -> [Text] -> [Text] -> ([Board], [Text])
playToLose bs drawn [] = (bs, drawn)
playToLose bs drawn leftToDraw =
  let bingos = map (bingo $ drawn) bs
      losingbs = filter (\b -> not (bingo drawn b)) bs
   in if any id bingos && length bs == 1
        then playToLose bs drawn []
        else playToLose losingbs (drawn ++ [head leftToDraw]) (tail leftToDraw)

score :: Board -> [Text] -> Int
score board drawn =
  let lastDrawn = read $ T.unpack $ last drawn
      unmarkedS = unmarkedSum board drawn
   in lastDrawn * unmarkedS

drawnNumbers :: Text -> [Text]
drawnNumbers inp = ((T.splitOn ",") . head . T.lines) inp

boards :: Text -> [Board]
boards inp = map board $ (tail . (T.splitOn "\n\n")) inp

board :: Text -> Board
board b =
  let ls = T.lines b
      rows = map (filter ((/=) "") . (T.splitOn " ") . T.strip) ls
      cols = transpose rows
   in Board rows cols

transpose :: [[Text]] -> [[Text]]
transpose ([] : _) = []
transpose xs = (map head xs) : transpose (map tail xs)

bingo :: [Text] -> Board -> Bool
bingo drawn board@(Board rows cols) =
  let rowBingo row = all (\n -> elem n drawn) row
   in any rowBingo rows || any rowBingo cols

unmarkedSum :: Board -> [Text] -> Int
unmarkedSum board@(Board rows cols) drawn =
  let unmarked = map (read . T.unpack) $ filter (\x -> not (elem x drawn)) $ concat rows
   in sum unmarked

data Board = Board
  { rows :: [[Text]],
    cols :: [[Text]]
  }
  deriving (Show)
