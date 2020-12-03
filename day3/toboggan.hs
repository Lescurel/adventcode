main = do  
    contents <- readFile "input"
    print . collisions '#' 3 1 $ lines contents
    print . exploreCollisions '#' [1,3,5,7,1] [1,1,1,1,2] $ lines contents

exploreCollisions :: Char -> [Int] -> [Int] -> [String] -> Int
exploreCollisions c rights downs xs = product (map ($ xs) exploreFcts)
    where exploreFcts = zipWith (collisions c) rights downs

collisions :: Char -> Int -> Int -> [String] -> Int
collisions c right down xs = length (filter (c==) (squareVisited right down xs))

squareVisited :: Int -> Int -> [String] -> [Char]
squareVisited right down xs = zipWith getSquare [right,right*2..] (tail (everyf down xs))

getSquare :: Int -> [a] -> a
getSquare right s = s!!mod right (length s)

everyf :: Int -> [a] -> [a]
everyf n [] = []
everyf n as  = head as : everyf n (drop n as)


