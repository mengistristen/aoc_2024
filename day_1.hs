module Main where

import Data.List
import qualified Data.Map.Strict as Map

parsePair :: String -> (Int, Int)
parsePair line =
  let [x, y] = map read (words line)
  in (x, y)

distance :: (Int, Int) -> Int
distance (x, y) = abs (x - y)

countOccurances :: [Int] -> Map.Map Int Int
countOccurances = foldl (\acc num -> Map.insertWith (+) num 1 acc) Map.empty

calculateSimilarity :: Int -> Map.Map Int Int -> Int
calculateSimilarity value table = case Map.lookup value table of
  Just y -> value * y
  Nothing -> 0

partOne :: String -> IO()
partOne inputFile = do
  content <- readFile inputFile
  let (x, y) = unzip (map parsePair (lines content))
  let sorted = zip (sort x) (sort y)
  let result = sum (map distance sorted)
  print result

partTwo :: String -> IO()
partTwo inputFile = do
  content <- readFile inputFile
  let (x, y) = unzip (map parsePair (lines content))
  let occurances = countOccurances y
  let result = sum (map (`calculateSimilarity` occurances) x)
  print result

main :: IO ()
main = do
  putStrLn "Part One: "
  partOne "input/day_1_input.txt"
  putStrLn "Part Two: "
  partTwo "input/day_1_input.txt"
