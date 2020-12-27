import Data.List (sort)

wrappingArea :: [Int] -> Int
wrappingArea input = do
  let l = input !! 0
  let h = input !! 1
  let w = input !! 2
  let slack = product $ xSmaller input 2

  -- 2*l*w + 2*w*h + 2*h*l
  2 * l * w + 2 * w * h + 2 * h * l + slack

ribbonArea :: [Int] -> Int
ribbonArea input = do
  let small = xSmaller input 2
  let wrap = sum (small) * 2
  let bow = product input
  wrap + bow

xSmaller :: [Int] -> Int -> [Int]
xSmaller l x = do
  take x . sort $ l

split :: [Char] -> [[Char]]
split str = case break (== 'x') str of
  (a, 'x' : b) -> a : split b
  (a, "") -> [a]

parseString :: String -> [Int]
parseString str = map read (split str) :: [Int]

parseInput :: String -> [[Int]]
parseInput input = map parseString $ lines input

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
