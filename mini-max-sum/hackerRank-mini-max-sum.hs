import Data.List 

main = do
    userInput <- getLine
    putStrLn . concat $ intersperse " " $ map show $ miniMaxSum $ sort $ map read $ words userInput


-- Mini max sum.
--
miniMaxSum :: [Int] -> [Int]
miniMaxSum [] = []
miniMaxSum listOfNumbers = sum (take 4 listOfNumbers) : sum (drop 1 listOfNumbers) : []
