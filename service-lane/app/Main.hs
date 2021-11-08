module Main where

import Lib
import System.IO


main :: IO ()
main = do
    userInput <- getContents
    --map strToInt . map words $ lines userInput
    print "hello"

--- create subset of set..
-- Ex: 2 2 [1,2,3,4,5,6,7] -> [3,4]
-- |||||||||||||||||||||||||||||||||
subSlice :: Int -> Int -> [a] -> [a]
subSlice beginWith numElems theString = take numElems $ drop beginWith theString


--- convert string to Int -- list.
strToInt :: [String] -> [Int]
strToInt = map (read::(String -> Int)) 

--- process userInput, returning a list of solutions.
--
procInput :: [[Int]] -> [Int]
procInput (x:xs) = undefined
    where
        theLanes = undefined
        theRamps = undefined

