import Data.List (sort)

wrappingArea :: [Int] -> Int
wrappingArea input = 2 * l * w + 2 * w * h + 2 * h * l + slack
  where
    l = input !! 0
    h = input !! 1
    w = input !! 2
    slack = product $ xSmaller input 2

ribbonArea :: [Int] -> Int
ribbonArea input = do
  sum (xSmaller input 2) * 2 + product input

xSmaller :: [Int] -> Int -> [Int]
xSmaller l x = do
  take x . sort $ l

-- For supporting empty list
tail' :: [a] -> [a]
tail' (_ : xs) = xs
tail' [] = []

split :: Char -> [Char] -> [[Char]]
split _ [] = []
split d s = x : split d (tail' s')
  where
    (x, s') = span (/= d) s

parseString :: String -> [Int]
parseString str = map read (split 'x' str)

parseInput :: String -> [[Int]]
parseInput input = map parseString $ lines input

-- --------------------------------------------------------------------------------------------------------------------

partOne :: String -> IO ()
partOne input = do
  putStrLn $
    "Part One: "
      ++ show (sum . map wrappingArea $ parseInput input)

partTwo :: String -> IO ()
partTwo input = do
  putStrLn $
    "Part Two: "
      ++ show (sum . map ribbonArea $ parseInput input)

main :: IO ()
main = do
  input <- readFile "./input.txt"

  partOne input
  partTwo input
