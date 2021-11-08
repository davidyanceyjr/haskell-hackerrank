--module Main where
--main = interact . map show $ winner . tail . map read . words
--interact $ unwords . winner . tail . map read . words 

import Data.List 
import Data.Ord

main = do
    userInput <- getContents
    print $ stepsToInt (tail userInput) 0 0

stepsToInt :: String -> Int -> Int -> Int
stepsToInt [] _ totalValleys = totalValleys
stepsToInt (x:xs) dir numValleys
    | x == 'D' = stepsToInt xs (dir -1) numValleys
    | x == 'U' && (dir == -1) = stepsToInt xs (dir +1) (numValleys +1)
    | otherwise = stepsToInt xs (dir +1) numValleys
    where
        totalValleys = numValleys
