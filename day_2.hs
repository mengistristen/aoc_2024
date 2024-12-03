process :: [Int] -> Int -> Bool
process (x : y : rest) tolerance
  | x == y = (tolerance > 0) && process (y : rest) (tolerance - 1)
  | otherwise = check [] (x : y : rest) (<) tolerance || check [] (x : y : rest) (>) tolerance
process [x] _ = True

check :: [Int] -> [Int] -> (Int -> Int -> Bool) -> Int -> Bool
check accepted (x : y : rest) compare tolerance
  | x == y = (tolerance > 0) && check [] (accepted ++ y : rest) compare (tolerance - 1)
  | compare x y || abs (x - y) > 3 =
      (tolerance > 0)
        && ( check [] (accepted ++ x : rest) compare (tolerance - 1)
               || check [] (accepted ++ y : rest) compare (tolerance - 1)
           )
  | otherwise = check (accepted ++ [x]) (y : rest) compare tolerance
check _ [x] _ _ = True

partOne :: String -> IO ()
partOne input = do
  content <- readFile input
  let split = lines content
  let text = map words split
  let reports :: [[Int]] = map (map read) text
  let result = map (`process` 0) reports
  print (length (filter id result))

partTwo :: String -> IO ()
partTwo input = do
  content <- readFile input
  let split = lines content
  let text = map words split
  let reports :: [[Int]] = map (map read) text
  let result = map (`process` 1) reports
  print (length (filter id result))
