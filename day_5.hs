module Main where

import Control.Applicative
import Data.List
import Debug.Trace
import Parser

data Data
  = Pair (Int, Int)
  | Pages [Int]

pair :: Parser Data
pair = (\left _ right -> Pair (left, right)) <$> numP <*> charP '|' <*> numP

pagesP :: Parser Data
pagesP = Pages <$> notNull (sepBy (charP ',') numP)

sortPages :: [(Int, Int)] -> Int -> Int -> Ordering
sortPages pairs a b
  | (a, b) `elem` pairs = LT
  | (b, a) `elem` pairs = GT
  | otherwise = EQ

process :: [String] -> [(Int, Int)] -> [[Int]] -> ([(Int, Int)], [[Int]])
process (x : xs) pairs lists = case runParser (pair <|> pagesP) x of
  Just ([], Pair (x, y)) -> process xs ((x, y) : pairs) lists
  Just ([], Pages pages) -> process xs pairs (pages : lists)
  _ -> process xs pairs lists
process [] pairs lists = (pairs, lists)

processPagesOne :: [(Int, Int)] -> [Int] -> Int
processPagesOne pairs pages =
  if pages == sortBy (sortPages pairs) pages
    then pages !! div (length pages) 2
    else 0

processPagesTwo :: [(Int, Int)] -> [Int] -> Int
processPagesTwo pairs pages =
  if pages == sorted
    then 0
    else sorted !! div (length sorted) 2
    where sorted = sortBy (sortPages pairs) pages

partOne :: String -> IO ()
partOne inputFile = do
  contents <- readFile inputFile
  let (pairs, pages) = process (lines contents) [] []
   in let a = sum $ map (processPagesOne pairs) pages
       in print a

partTwo :: String -> IO ()
partTwo inputFile = do
  contents <- readFile inputFile
  let (pairs, pages) = process (lines contents) [] []
   in let a = sum $ map (processPagesTwo pairs) pages
       in print a

main :: IO ()
main = undefined
