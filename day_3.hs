module Main where

import Parser

newtype Mul = Mul (Int, Int)
  deriving (Show) 

mulP :: Parser Mul
mulP = stringP "mul" *> charP '(' *> pair <* charP ')'
  where 
    pair = 
      (\left _ right -> Mul (left, right)) <$> numP <*> charP ',' <*> numP

process :: String -> Int -> Int
process input accum = case runParser mulP input of
  Just ([], Mul (x, y)) -> accum + (x * y)
  Just (rest, Mul(x, y)) -> process rest (accum + (x * y))
  Nothing -> case input of 
    [] -> accum
    (_:rest) -> process rest accum

partOne :: String -> IO ()
partOne input = do
  content <- readFile input
  print $ process content 0

main :: IO ()
main = do
  putStrLn "Part One: "
  partOne "input/day_3_input.txt"
