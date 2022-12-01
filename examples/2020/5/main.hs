import Data.List
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> do
      contents <- readFile path
      putStr "Highest seat ID: "
      print $ maxId $ map parse $ boardingPasses contents
      putStr "My ID: "
      print $ findSeat $ map parse $ boardingPasses contents
    _ -> error "Exactly one argument must be supplied."

findSeat :: [(Int, Int, Int)] -> Int
findSeat passes
  | length passes > 3 = findSeat' $ sort $ map sel3 passes
  | otherwise = error "Empty list supplied to findSeat"

findSeat' :: [Int] -> Int
findSeat' (bfr : aft : ids)
  | bfr + 2 == aft = bfr + 1
  | otherwise = findSeat' (aft : ids)
findSeat' _ = error "Could not find seat."

maxId :: [(Int, Int, Int)] -> Int
maxId passes
  | not (null passes) = foldl max 0 (map sel3 passes)
  | otherwise = error "Empty list supplied to maxId"

parse :: String -> (Int, Int, Int)
parse cs
  | length cs == 10 = parse' cs (0, 0) (7 - 1, 3 - 1)
  | otherwise = error "Invalid length of boarding pass."

parse' :: String -> (Int, Int) -> (Int, Int) -> (Int, Int, Int)
parse' cols (r, c) (-1, cx) = parseC' cols (r, c) (-1, cx)
parse' ('F' : cs) (r, c) (rx, cx) = parse' cs (r, c) (rx - 1, cx)
parse' ('B' : cs) (r, c) (rx, cx) = parse' cs (r + 2 ^ rx, c) (rx - 1, cx)
parse' _ _ _ = error "Invalid character in boarding pass row."

parseC' :: String -> (Int, Int) -> (Int, Int) -> (Int, Int, Int)
parseC' [] (r, c) (-1, -1) = (r, c, r * 8 + c)
parseC' ('L' : cs) (r, c) (rx, cx) = parse' cs (r, c) (rx, cx -1)
parseC' ('R' : cs) (r, c) (rx, cx) = parse' cs (r, c + 2 ^ cx) (rx, cx - 1)
parseC' _ _ _ = error "Invalid character in boarding pass column."

boardingPasses :: String -> [String]
boardingPasses [] = [""]
boardingPasses ['\n'] = [""]
boardingPasses ('\n' : cs) = "" : boardingPasses cs
boardingPasses (c : cs) = (c : head rest) : tail rest
  where
    rest = boardingPasses cs

sel3 :: (Int, Int, Int) -> Int
sel3 (_, _, i) = i
