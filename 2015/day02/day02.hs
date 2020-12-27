import Data.List (sort)

wrappingArea :: [Int] -> Int
wrappingArea input = do
  let l = input !! 0
  let h = input !! 1
  let w = input !! 2
  let slack = product $ xSmaller input 2

  -- 2*l*w + 2*w*h + 2*h*l
  2 * l * w + 2 * w * h + 2 * h * l + slack

xSmaller :: [Int] -> Int -> [Int]
xSmaller l x = do
  take x . sort $ l

split :: [Char] -> [[Char]]
split str = case break (== 'x') str of
  (a, 'x' : b) -> a : split b
  (a, "") -> [a]

parseString :: String -> [Int]
parseString str = do
  let d = split str
  map read d :: [Int]

parseInput :: String -> [[Int]]
parseInput input = do
  let ls = lines input
  map parseString ls

partOne :: String -> IO ()
partOne input = do
  let inputs = parseInput input
  let wrappingAreas = map wrappingArea inputs
  let total = sum wrappingAreas

  putStrLn $ "Part One: " ++ show total

partTwo :: String -> IO ()
partTwo input = do
  putStrLn "Part Two: "

main :: IO ()
main = do
  input <- readFile "./input.txt"

  partOne input
  partTwo input
