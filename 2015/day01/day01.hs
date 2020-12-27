import Data.Array (listArray)

transform :: Char -> Int
transform x
  | x == '(' = 1
  | x == ')' = - 1
  | otherwise = 0

transformString :: String -> [Int]
transformString = map transform

firstNegativePos :: String -> Int
firstNegativePos =
  length
    . takeWhile (>= 0)
    . scanl (+) 0
    . transformString

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