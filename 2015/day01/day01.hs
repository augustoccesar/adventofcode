import Data.Array (listArray)

transform :: Char -> Int
transform x
  | x == '(' = 1
  | x == ')' = - 1
  | otherwise = 0

transformString :: String -> [Int]
transformString = map transform

firstNegativePos :: String -> Int
firstNegativePos input = do
  -- This probably can be simplified by dots and dollar signs, but didn't quite got how it works yet
  let a = transformString input
  let b = scanl (+) 0 a
  let d = takeWhile (>= 0) b
  length d

partOne :: String -> IO ()
partOne input = do
  putStrLn $ "Part One: " ++ show (sum $ transformString input)

partTwo :: String -> IO ()
partTwo input = do
  putStrLn $ "Part Two: " ++ show (firstNegativePos input)

main :: IO ()
main = do
  input <- readFile "./input.txt"

  partOne input
  partTwo input