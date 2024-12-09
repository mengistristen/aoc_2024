module Main where

import Debug.Trace

data Dir = North | East | South | West
  deriving (Show)

turnRight :: Dir -> Dir
turnRight North = East
turnRight East = South
turnRight South = West
turnRight West = North

move :: (Int, Int) -> Dir -> (Int, Int)
move (x, y) North = (x, y - 1)
move (x, y) East = (x + 1, y)
move (x, y) South = (x, y + 1)
move (x, y) West = (x - 1, y)

process :: String -> (Int, Int) -> [(Int, Int)] -> (Int, Int) -> ([(Int, Int)], (Int, Int))
process (curr : rest) (x, y) obstructions start = case curr of
  '#' -> process rest (x + 1, y) ((x, y) : obstructions) start
  '^' -> process rest (x + 1, y) obstructions (x, y)
  '\n' -> process rest (0, y + 1) obstructions start
  _ -> process rest (x + 1, y) obstructions start
process [] _ obstructions start = (obstructions, start)

findVisited :: [(Int, Int)] -> (Int, Int) -> Dir -> Int -> Int -> [(Int, Int)] -> [(Int, Int)]
findVisited obstructions (x, y) dir width height visited
  | x < 0 || x >= width || y < 0 || y >= height = visited
  | otherwise =
      if next `elem` obstructions
        then findVisited obstructions (x, y) (turnRight dir) width height visited
        else findVisited obstructions next dir width height (if (x, y) `elem` visited then visited else (x, y) : visited)
  where
    next = move (x, y) dir

partOne :: String -> IO ()
partOne inputFile = do
  contents <- readFile inputFile
  let split = lines contents
  let (height, width) = (length split, length (head split))
      (obstructions, start) = process contents (0, 0) [] (0, 0)
   in print $ length $ findVisited obstructions start North width height []

main :: IO ()
main = undefined
