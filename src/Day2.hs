module Day2
  ( solve,
  )
where

import Control.Applicative
import Text.ParserCombinators.ReadP

input :: String -> IO [String]
input fileName = do
  f <- readFile fileName
  return (lines f)

solve :: IO ()
solve = do
  parsed <- fmap parse (input "data/day2_input.txt")
  -- print $ parsed
  pos <- return (run2 parsed)
  print pos
  print (result pos)

result :: (Horizontal, Depth, Aim) -> Int
result (h, d, a) = h * d

run :: [Command] -> (Horizontal, Depth)
run cmds = foldl (runCmd) (0, 0) cmds

run2 :: [Command] -> (Horizontal, Depth, Aim)
run2 cmds = foldl (runCmd2) (0, 0, 0) cmds

type Horizontal = Int

type Depth = Int

type Aim = Int

runCmd :: (Horizontal, Depth) -> Command -> (Horizontal, Depth)
runCmd (h, d) (Forward x) = (h + x, d)
runCmd (h, d) (Up x) = (h, d - x)
runCmd (h, d) (Down x) = (h, d + x)

runCmd2 :: (Horizontal, Depth, Aim) -> Command -> (Horizontal, Depth, Aim)
runCmd2 (h, d, a) (Forward x) = (h + x, d + a * x, a)
runCmd2 (h, d, a) (Up x) = (h, d, a - x)
runCmd2 (h, d, a) (Down x) = (h, d, a + x)

parse :: [String] -> [Command]
parse x = map (fst . head) $ map (readP_to_S commandParser) x

commandParser :: ReadP Command
commandParser = do
  cmd <- forward <|> up <|> down
  return cmd

digit :: ReadP Char
digit = satisfy (\char -> char >= '0' && char <= '9')

number :: ReadP Int
number = fmap read (many1 digit)

character :: ReadP Char
character = satisfy (\char -> char >= 'a' && char <= 'z')

forward :: ReadP Command
forward = do
  string "forward "
  units <- number
  return $ Forward units

down :: ReadP Command
down = do
  string "down "
  units <- number
  return $ Down units

up :: ReadP Command
up = do
  string "up "
  units <- number
  return $ Up units

data Command = Forward Int | Down Int | Up Int deriving (Show)
