module Day2
    ( solve
    ) where

import Text.ParserCombinators.ReadP

input :: String -> IO [String]
input fileName = do
    f <- readFile fileName
    return ( lines f)

solve :: IO ()
solve = do
    solution <- fmap parse (input "data/day2_small.txt")
    print $ solution

parse :: [String] -> [(String, Int)]
parse x = map (fst . head) $ map (readP_to_S commandParser) x


commandParser :: ReadP (String, Int)
commandParser = do
    cmd <- many1 character
    string " "
    units <- number
    return (cmd, units)
  
digit :: ReadP Char
digit = satisfy (\char -> char >= '0' && char <= '9')

number :: ReadP Int
number = fmap read (many1 digit)

character :: ReadP Char
character = satisfy (\char -> char >= 'a' && char <= 'z')

data Command = Forward Int | Down Int | Up Int

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
