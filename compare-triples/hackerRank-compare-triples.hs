--module Main where

import Data.List
import Data.Char

main :: IO ()
main = do
    aliceScores <- getLine
    bobScores   <- getLine
    let alice = words aliceScores
    let bob = words bobScores
    let total = finalScore $ matchesWon alice bob
    putStr $ unwords $ map show total

    
printScore :: [Int] -> String
printScore [] = "printScore: empty list"
printScore (x:y) = show x ++ show y

matchesWon :: [String] -> [String] -> [String]
matchesWon [] _ = []
matchesWon _ [] = []
matchesWon (x:xs) (y:ys)
    | firstScore > secondScore = "Alice" : matchesWon xs ys
    | firstScore < secondScore  = "Bob" : matchesWon xs ys
    | otherwise = "Tied" : matchesWon xs ys
    where
        firstScore = read x :: Int
        secondScore = read y :: Int

finalScore :: [String] -> [Int]
finalScore [] = []
finalScore matchTallies 
    | (length $ filter (=="Tied") matchTallies) > 2 = [0,0]
    | otherwise = [length $ filter (=="Alice") matchTallies, length $ filter (=="Bob") matchTallies]
