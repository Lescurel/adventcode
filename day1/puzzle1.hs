main = do  
        contents <- readFile "input"
        print . puzzleN 2 . map readInt . words $ contents
        print . puzzleN 3. map readInt . words $ contents

readInt :: String -> Int
readInt = read

prod :: [[a]] -> [[a]] -> [[a]]
prod as bs = [a ++ b | a <- as, b <- bs]

rep :: Int -> [a] -> [[a]]
rep n as = foldr1 prod $ replicate n (fmap (:[]) as)

puzzleN :: (Num b, Eq b) => Int -> [b] -> [b]
puzzleN x xs = map product $ filter _cond (rep x xs)
        where _cond xs = sum xs==2020
