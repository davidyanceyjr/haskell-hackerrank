--module Main where
--sortBy (flip (comparing length)) . sort . words 
--main :: IO ()
--main = print "TBA"

import Data.List 
import Data.Ord

sampleInput :: [Int]
sampleInput = [4,2,3,4,4,9,98,98,3,3,3,4,2,98,1,98,98,1,1,4,98,2,98,3,9,9,3,1,4,1,98,9,9,2,9,4,2,2,9,98,4,98,1,3,4,9,1,98,98,4,2,3,98,98,1,99,9,98,98,3,98,98,4,98,2,98,4,2,1,1,9,2,4,19]


-- This is failing - proposing the answer is 22, my answer is 21.
sampleTestCase3 = [4,2,3,4,4,9,98,98,3,3,3,4,2,98,1,98,98,1,1,4,98,2,98,3,9,9,3,1,4,1,98,9,9,2,9,4,2,2,9,98,4,98,1,3,4,9,1,98,98,4,2,3,98,98,1,99,9,98,98,3,98,98,4,98,2,98,4,2,1,1,9,2,4]


sampleTestCase4 = [84,43,11,41,2,99,31,32,56,53,42,14,98,27,64,57,16,33,48,21,46,61,87,90,28,12,62,49,29,77,82,70,80,89,95,52,13,19,9,79,35,67,51,39,7,1,66,8,17,85,71,97,34,73,75,6,91,40,96,65,37,74,20,68,23,47,76,55,24,3,30,22,55,5,69,86,54,50,10,59,15,4,36,38,83,60,72,63,78,58,88,93,45,18,94,44,92,81,25,26]

main = interact $ show . pickingNumbers . tail . map read . words

pickingNumbers :: Ord a => Num a => [a] -> Int
pickingNumbers list
    | length list < 2 = 0
    | otherwise = length . concat . take 2 . filterEmpty $ filterSet prepList 
    where
        prepList = sortBy (flip (comparing length)) . group $ sort list

filterSet :: Ord a => Num a => [[a]] -> [[a]]
filterSet [] = []
filterSet ([]:_) = []
filterSet list@((x:xs):ys)
    | isEmpty list = [] 
    | otherwise = [x:xs] ++ map (filter (oneAway x)) ys
        

isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty _ = False

filterEmpty :: Ord a => Num a => [[a]] -> [[a]]
filterEmpty [] = []
filterEmpty (x:xs)
    | isEmpty x = filterEmpty xs
    | otherwise = [x] ++ (filterEmpty xs)

oneAway :: Ord a => Num a => a -> a -> Bool
oneAway a b
    | abs (a - b) < 2 = True 
    | otherwise = False 
