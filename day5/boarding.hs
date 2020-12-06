import Data.List ( sort )

main :: IO ()
main = do
    contents <- readFile "input"
    print (maximum (map getBoardingPassId (lines contents)))
    print (findMissing (sort (map getBoardingPassId (lines contents))))

findMissing :: [Int] -> Int
findMissing (x:y:xs) 
    | x==(y-1) = findMissing (y:xs)
    | otherwise = x+1
findMissing [] = 0

getBoardingPassId :: String -> Int
getBoardingPassId s = getRow (take 7 s) * 8 + getColumn (drop 7 s)

getRow :: String -> Int
getRow = binarySpacePartitioning 'B' 128

getColumn :: String -> Int
getColumn = binarySpacePartitioning 'R' 8

binarySpacePartitioning :: Char -> Int -> String -> Int
binarySpacePartitioning upper start (x:xs) 
    | x == upper = half + binarySpacePartitioning upper half xs 
    | otherwise = binarySpacePartitioning upper half xs
        where half = div start 2
binarySpacePartitioning _ _ [] = 0

