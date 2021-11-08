--module Main where

-- exammple:
--      4 x 4 matrix 
--      [[1,2,3,4],
--       [5,6,7,8],
--       [9,10,11,12],
--       [13,14,15,16]] 
-- 
import Data.List.Split
import Data.Char

main = interact $ show . diffDiag . sumDiags . getDiags  . matrixFromList 
                       . tail . map read . words

matrixFromList :: [a] -> [[a]]
matrixFromList [] = []
matrixFromList list = chunksOf szOfMatrix list
    where 
          szOfMatrix = truncate $ sqrt $ fromIntegral $ length list

diffDiag :: Num a => [a] -> a
diffDiag [] = 0 
diffDiag list = abs $ foldr (-) 0 list

getDiags :: [[a]] -> [[a]]
getDiags [] = []
getDiags matrix = [getLeftDiag matrix] ++ [getRightDiag matrix]

sumDiags  []     =  []                          
sumDiags  diags  =  map sum diags

getLeftDiag :: [[a]] -> [a]
getLeftDiag [] = []
getLeftDiag matrix@(x:xs) 
    | getElement == 0 = head x : getLeftDiag xs
    | otherwise = head (drop getElement x) : getLeftDiag xs
    where 
          getElement = length x - length matrix 

getRightDiag :: [[a]] -> [a]
getRightDiag [] = []
getRightDiag matrix@(x:xs) 
    | getElement == 0 = head x : getRightDiag xs
    | otherwise = head (drop getElement x) : getRightDiag xs
    where 
          getElement = length matrix - 1



