--module Main where
--main = interact . map show $ winner . tail . map read . words

import Data.List 
import Data.Ord
--interact $ unwords . winner . tail . map read . words 

main = do
    userInput <- getContents
    mapM_ putStrLn $ winner . tail . map read $ words userInput

catX    =  "Cat A"
catY    =  "Cat B"
mouseZ  =  "Mouse C"

winner :: [Int] -> [String]
winner [] = []
winner (x:y:z:zs)
    | isX == isY  =  mouseZ  : winner zs
    | isX > isY   =  catY    : winner zs
    | otherwise   =  catX    : winner zs
    where
        isX = abs (z-x)
        isY = abs (z-y)
