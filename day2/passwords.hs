main = do  
        contents <- readFile "input"
        print . length . filter val . map (parsePasswords . words) $ lines contents
        print . length . filter val2 . map (parsePasswords . words) $ lines contents

getPass :: (Int, Int, Char, String) -> String
getPass (a,b,c,d) = d

val :: (Int, Int, Char, String) -> Bool
val (a,b,c,d) = validatePassword a b c d

val2 :: (Int, Int, Char, String) -> Bool
val2 (a,b,c,d) = validatePassword2 a b c d

validatePassword2 ::  Int -> Int -> Char -> String -> Bool
validatePassword2 a b c pass = pass!!(a-1) == c && pass!!(b-1) /= c || pass!!(a-1) /= c && pass!!(b-1) == c


validatePassword :: Int -> Int -> Char -> String -> Bool
validatePassword a b c pass = cnt >= a && cnt <= b
        where cnt= count c pass

parsePasswords :: [String] -> (Int, Int, Char, String)
parsePasswords [a,b,c] = (fst minmax, snd minmax, head b, c)
        where minmax = getMinMax a
parsePasswords ss = error "Failed to parse file"

count :: Eq a => a -> [a] -> Int
count x = length . filter (x==)

readInt :: String -> Int
readInt = read 

getMinMax :: String -> (Int, Int)
getMinMax s 
        | length splitted == 2 = (head splitted, splitted !! 1)
        | otherwise = error "Failed to parse Min Max"
        where splitted = map readInt (splitOnSep '-' s)

splitOnSep :: Char -> String -> [String]
splitOnSep sep s = words $ [if c==sep then ' ' else c| c <- s]


prod :: [[a]] -> [[a]] -> [[a]]
prod as bs = [a ++ b | a <- as, b <- bs]

rep :: Int -> [a] -> [[a]]
rep n as = foldr1 prod $ replicate n (fmap (:[]) as)

puzzleN :: (Num b, Eq b) => Int -> [b] -> [b]
puzzleN x xs = map product $ filter _cond (rep x xs)
        where _cond xs = sum xs==2020
