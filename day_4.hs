module Main where

import Debug.Trace
import Data.List

directions = [(-1, -1), (0, -1), (1, -1), (-1, 0), (1, 0), (-1, 1), (0, 1), (1, 1)]

getIndices :: Int -> Int -> [(Int, Int)]
getIndices rows cols = [(x, y) | x <- [0 .. rows - 1], y <- [0 .. cols - 1]]

getElement :: [[Char]] -> (Int, Int) -> Maybe Char
getElement input (x, y)
  | x < 0 || x >= length (head input) = Nothing
  | y < 0 || y >= length input = Nothing
  | otherwise = Just ((input !! y) !! x)

process :: [[Char]] -> [Int]
process input =
  map (processCell input) (getIndices (length input) (length (head input)))

processCell :: [[Char]] -> (Int, Int) -> Int
processCell input (x, y)
  | input !! y !! x == 'X' = sum $ map (processWord input (x, y) "") directions
  | otherwise = 0

processWord :: [[Char]] -> (Int, Int) -> [Char] -> (Int, Int) -> Int
processWord input (x, y) accum (dx, dy) =
  case getElement input (x, y) of
    Nothing -> 0
    Just value ->
      let str = accum ++ [value]
       in if str == "XMAS"
            then 1
            else
              if str `isPrefixOf` "XMAS"
                then processWord input (x + dx, y + dy) str (dx, dy)
                else 0

partOne :: String -> IO()
partOne inputFile = do
  content <- readFile inputFile
  print $ sum $ process $ lines content

main :: IO ()
main = do
  print "Hello"
