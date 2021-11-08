module Main where

import Data.List

main :: IO ()
main = do
    userInput <- getContents
    print $ numberOfas userInput


numberOfas :: String -> Int
numberOfas [] = 0
numberOfas list = quotient * originalString + truncatedString
    where
        theString = head $ lines list
        examinNumElems = read $ last $ lines list
        szOfTheString = length theString
        quotient = examinNumElems `div` szOfTheString
        theRemainder = examinNumElems `mod` szOfTheString
        truncatedString = length . filter ('a'==) $ take theRemainder theString
        originalString = length (filter ('a'==) theString) 
