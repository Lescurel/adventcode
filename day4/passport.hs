import Data.Char
import Data.List

main :: IO ()
main = do
  contents <- readFile "input"
  print . countValidPassport $ lines contents
  print (map (map validateFields . getFieldsWithValue) (getPassportStrings $ lines contents))
  print . countCompletelyValidPassport $ lines contents

countValidPassport :: [String] -> Int
countValidPassport xs = length (filter validatePassport (map getFields (getPassportStrings xs)))

countCompletelyValidPassport :: [String] -> Int
countCompletelyValidPassport xs = length (filter id (map (validateOnePassport . getFieldsWithValue) (getPassportStrings xs)))


validateOnePassport :: [(String, String)] -> Bool
validateOnePassport xs = validatePassportAsTuple xs && all validateFields xs

readInt :: String -> Int
readInt = read

validateFields :: (String, String) -> Bool
validateFields (k, v)
  | k == "byr" = vint <= 2002 && vint >= 1920
  | k == "iyr" = vint <= 2020 && vint >= 2010
  | k == "eyr" = vint <= 2030 && vint >= 2020
  | k == "hgt" = (isSuffixOf "cm" v && hgt <= 193 && hgt >= 150) || (isSuffixOf "in" v && hgt <= 76 && hgt >= 59)
  | k == "hcl" = hash == "#" && length color == 6 && all validEyeColor color
  | k == "ecl" = isInfixOf [v] ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
  | k == "pid" = length v == 9 && all isDigit v
  | k == "cid" = True
  | otherwise = False
  where
    hash = take 1 v
    color = drop 1 v
    hgt = readInt (take (length v - 2) v)
    vint = readInt v

getListAsTuple :: [String] -> (String, String)
getListAsTuple (x:xs) = (x,head xs)

getFieldsWithValue :: String -> [(String, String)]
getFieldsWithValue s = map (getListAsTuple . split ':' []) (words s)

validEyeColor :: Char -> Bool
validEyeColor color = isAlpha color || ord color >= 97 || ord color <= 102

getFields :: String -> [String]
getFields s = sort (map (concat . take 1 . split ':' []) (words s))

validatePassportAsTuple :: [(String, String)] -> Bool
validatePassportAsTuple = validatePassport . sort . map fst

validatePassport :: [String] -> Bool
validatePassport xs
  | xs == ["byr", "cid", "ecl", "eyr", "hcl", "hgt", "iyr", "pid"] = True
  | xs == ["byr", "ecl", "eyr", "hcl", "hgt", "iyr", "pid"] = True
  | otherwise = False

getPassportStrings :: [String] -> [String]
getPassportStrings xs = map unwords (splitOnEmpty xs)

splitOnEmpty :: [String] -> [[String]]
splitOnEmpty = split "" []

split :: (Eq a) => a -> [a] -> [a] -> [[a]]
split sep first (x : xs)
  | x == sep = first : split sep [] xs
  | otherwise = split sep (first ++ [x]) xs
split _ first [] = [first]