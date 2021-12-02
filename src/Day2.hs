module Day2
    ( solve
    ) where

import Control.Applicative
import Text.ParserCombinators.ReadP

input :: String -> IO [String]
input fileName = do
    f <- readFile fileName
    return ( lines f)

solve :: IO ()
solve = do
    parsed <- fmap parse (input "data/day2_input.txt")
    -- print $ parsed
    pos <- return (run parsed)
    print pos
    print (fst pos * snd pos)


run :: [Command] -> (Horizontal, Depth)
run cmds = foldl (runCmd) (0, 0) cmds

type Horizontal = Int
type Depth = Int

runCmd :: (Horizontal, Depth) -> Command -> (Horizontal, Depth)
runCmd (h, d) (Forward x) = (h + x, d)
runCmd (h, d) (Up x)      = (h, d - x)
runCmd (h, d) (Down x)    = (h, d + x)

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

{-
 - solve = do
 -   input <- readFile "data/day2_full.txt"
 -   let parsed = parse $ lines input
 -   let valids = filter validPassword2 parsed
 -   print $ length valids
 - 
 - parse :: [String] -> [Password]
 - parse ls = map (fst . head) $ map (readP_to_S parser) ls
 - 
 - digit :: ReadP Char
 - digit = satisfy (\char -> char >= '0' && char <= '9')
 -}
