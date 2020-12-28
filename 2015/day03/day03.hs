import Data.List (sort)
import Debug.Trace (trace)

data Point = Point {x :: Int, y :: Int} deriving (Show, Eq)

move :: Point -> Char -> Point
move (Point x y) mc = case mc of
  '^' -> Point x (y + 1)
  '>' -> Point (x + 1) y
  'v' -> Point x (y - 1)
  '<' -> Point (x - 1) y

unique :: (Eq a) => [a] -> [a]
unique (x : xs) = x : unique (filter (/= x) xs)
unique [] = []

createHistory :: [Point] -> [Char] -> ([Point], [Char])
createHistory ph "" = (ph, "")
createHistory ph (c : x) = do
  let last_point = last ph
  let ph' = ph ++ [move last_point c]
  createHistory ph' x

-- --------------------------------------------------------------------------------------------------------------------

partOne :: String -> IO ()
partOne input = putStrLn $ "Part One: " ++ show (length $ unique ph)
  where
    (ph, _) = createHistory [Point 0 0] input -- (p)oint (h)istory

partTwo :: String -> IO ()
partTwo input = putStrLn $ "Part Two: " ++ show (length $ unique (sph ++ rsph))
  where
    idxMoves = zip [0 ..] input
    (sph, _) = createHistory [Point 0 0] [m | (_, m) <- filter (even . fst) idxMoves] -- (s)anta (p)oint (h)istory
    (rsph, _) = createHistory [Point 0 0] [m | (_, m) <- filter (odd . fst) idxMoves] -- (r)obo(s)anta (p)oint (h)istory

main :: IO ()
main = do
  input <- readFile "./input.txt"

  partOne input
  partTwo input
