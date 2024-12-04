module Main where

import Control.Applicative
import Parser

data MulT = Mul (Int, Int) | Do | Dont
  deriving (Show)

mulP :: Parser MulT
mulP = stringP "mul" *> charP '(' *> pair <* charP ')'
  where
    pair =
      (\left _ right -> Mul (left, right)) <$> numP <*> charP ',' <*> numP

doP :: Parser MulT
doP = Do <$ stringP "do()"

dontP :: Parser MulT
dontP = Dont <$ stringP "don't()"

process :: String -> Int -> Int
process input accum = case runParser mulP input of
  Just ([], Mul (x, y)) -> accum + (x * y)
  Just (rest, Mul (x, y)) -> process rest (accum + (x * y))
  Nothing -> case input of
    [] -> accum
    (_ : rest) -> process rest accum

processEnabled :: String -> Int -> Int
processEnabled input accum = case runParser (mulP <|> dontP) input of
  Just ([], Mul (x, y)) -> accum + (x * y)
  Just (rest, Mul (x, y)) -> processEnabled rest (accum + (x * y))
  Just ([], Dont) -> accum
  Just (rest, Dont) -> processDisabled rest accum
  Nothing -> case input of
    [] -> accum
    (_ : rest) -> processEnabled rest accum

processDisabled :: String -> Int -> Int
processDisabled input accum = case runParser doP input of
  Just ([], Do) -> accum
  Just (rest, Do) -> processEnabled rest accum
  Nothing -> case input of
    [] -> accum
    (_ : rest) -> processDisabled rest accum

partOne :: String -> IO ()
partOne input = do
  content <- readFile input
  print $ process content 0

partTwo :: String -> IO ()
partTwo input = do
  content <- readFile input
  print $ processEnabled content 0

main :: IO ()
main = do
  putStrLn "Part One: "
  partOne "input/day_3_input.txt"
  putStrLn "Part Two: "
  partTwo "input/day_3_input.txt"
