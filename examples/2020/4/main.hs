{-# LANGUAGE ViewPatterns #-}

import Data.Char
import Data.List
import System.Environment
import Text.Read

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> do
      contents <- readFile path

      putStrLn "Number of valid passports, part 1:"
      print (count_valid_passports_1 contents)

      putStrLn "Number of valid passports, part 2:"
      print (count_valid_passports_2 contents)
    _ -> error "Exactly one argument must be supplied."

count_valid_passports_1 :: String -> Int
count_valid_passports_1 cs = sum $ map (fromEnum . is_valid) (passports cs)
  where
    is_valid = (== 7) . length . filter (/= "cid") . map fst . attrs

count_valid_passports_2 :: String -> Int
count_valid_passports_2 cs = sum $ map (fromEnum . is_valid) (passports cs)
  where
    is_valid = (== 7) . length . filter (== True) . valids . attrs

valids :: [(String, String)] -> [Bool]
valids (("byr", readMaybe -> Just byr) : as) =
  (1920 <= byr && byr <= 2002) : valids as
valids (("iyr", readMaybe -> Just iyr) : as) =
  (2010 <= iyr && iyr <= 2020) : valids as
valids (("eyr", readMaybe -> Just eyr) : as) =
  (2020 <= eyr && eyr <= 2030) : valids as
valids (("hgt", hgt) : as)
  | Just reverse_height <- stripPrefix (reverse "cm") (reverse hgt),
    Just height <- readMaybe (reverse reverse_height) =
    (150 <= height && height <= 193) : valids as
  | Just reverse_height <- stripPrefix (reverse "in") (reverse hgt),
    Just height <- readMaybe (reverse reverse_height) =
    (59 <= height && height <= 76) : valids as
valids (("hcl", '#' : hex) : as) =
  (length hex == 6 && all isHexDigit hex) : valids as
valids (("ecl", ecl) : as) =
  elem ecl ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"] : valids as
valids (("pid", pid) : as) =
  (length pid == 9 && all isDigit pid) : valids as
valids (("cid", _) : as) = valids as
valids (_ : as) = False : valids as
valids [] = []

attrs :: String -> [(String, String)]
attrs passport =
  map (tuple . splitBy [":"]) (splitBy ["\n", " "] passport)
  where
    tuple (key : attr) = (key, concat attr)
    tuple [] = error "Could not parse key/value pair."

passports :: String -> [String]
passports = splitBy ["\n\n"]

splitBy :: [String] -> String -> [String]
splitBy _ [] = [""]
splitBy _ ['\n'] = [""]
splitBy [] cs = [cs]
splitBy (tag : tags) (stripPrefix tag -> Just cs) = "" : splitBy (tag : tags) cs
splitBy (tag : tags) (c : cs) = splitBy tags (c : head rest) ++ tail rest
  where
    rest = splitBy (tag : tags) cs