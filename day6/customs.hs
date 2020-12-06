import Data.Char ( isSpace )
import Data.List ( nub )
main :: IO ()
main = do
  contents <- readFile "input"
  print . sum . map (length . nub . dropWhiteSpace) . getAnswersStrings $ lines contents
  print . sum . map countEveryYes . splitOnEmpty $ lines contents

countEveryYes :: [String] -> Int
countEveryYes xs = helperCountEveryYes unq xs
    where unq = (nub . dropWhiteSpace) (concat xs) 

helperCountEveryYes :: String -> [String] -> Int
helperCountEveryYes (c:cs) xs = everyOneSaidYes tc c (concat xs) + helperCountEveryYes cs xs
    where 
        tc = length xs
helperCountEveryYes [] _ = 0

everyOneSaidYes :: Int -> Char -> String -> Int
everyOneSaidYes targetCount char xs  
    | count char xs == targetCount = 1
    | otherwise = 0


dropWhiteSpace :: String -> String
dropWhiteSpace = filter (not . isSpace)

getAnswersStrings :: [String] -> [String]
getAnswersStrings xs = map unwords (splitOnEmpty xs)

splitOnEmpty :: [String] -> [[String]]
splitOnEmpty = split "" []

split :: (Eq a) => a -> [a] -> [a] -> [[a]]
split sep first (x : xs)
  | x == sep = first : split sep [] xs
  | otherwise = split sep (first ++ [x]) xs
split _ first [] = [first]

count   :: Eq a => a -> [a] -> Int
count x =  length . filter (==x)