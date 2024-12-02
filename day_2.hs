process :: [Int] -> Bool
process (x:y:rest)
  | x == y = False
  | x - y > 0 = decreasing (x:y:rest)
  | otherwise = increasing (x:y:rest)
process [x] = True

decreasing :: [Int] -> Bool
decreasing (x:y:rest)
  | x == y = False
  | y > x = False
  | x - y > 3 = False
  | otherwise = decreasing (y:rest)
decreasing [x] = True

increasing :: [Int] -> Bool
increasing (x:y:rest)
  | x == y = False
  | y < x = False
  | y - x > 3 = False
  | otherwise = increasing (y:rest)
increasing [x] = True

partOne :: String -> IO()
partOne input = do
  content <- readFile input
  let split = lines content
  let text = map words split
  let reports :: [[Int]]= map (map read) text
  let result = map process reports
  print (length (filter id result))
