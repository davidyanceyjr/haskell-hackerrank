import Data.List
import Data.List.Split

main = interact $ unlines . isClassCancelled . formatInput . convertInput 


-- take a list of int's (x:xs) where x is threshold and 
-- xs is the students arrival time. Return a list of Strings
-- ["YES","NO","NO","YES"] indicating class cancellation.
isClassCancelled :: [[Int]] -> [String]
isClassCancelled [] = []
isClassCancelled [[]] = []
isClassCancelled ([]:_:_) = []
isClassCancelled ((x:xs):ys)
    | x > onTimeAttendance = "YES" : isClassCancelled ys
    | otherwise = "NO" : isClassCancelled ys
    where
        onTimeAttendance = length (filter (<1) xs) 


-- take a list of int's from convertInput and format into list
-- of lists for mapping
formatInput :: [Int] -> [[Int]]
formatInput [] = []
formatInput (x:xs) = take (x+1) xs : formatInput (drop (x+1) xs)

-- convert string input to list of int's.
convertInput :: String -> [Int]
convertInput [] = []
convertInput list = tail $ map read $ words list :: [Int]
