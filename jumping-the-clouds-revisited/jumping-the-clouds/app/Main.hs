-- see https://www.hackerrank.com/challenges/jumping-on-the-clouds-revisited/problem
-- |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

module Main where

import Data.List
import Data.List.Split
import Lib



main :: IO ()
main = do 
    lineOne <- getLine
    lineTwo <- getLine
    let moves = head . tail $ map read $ words lineOne
    let listOfClouds = map read $ words lineTwo
    print $ energyRemaining $ circularList moves listOfClouds


-- circularList :: Int -> [Int] -> [Int] -- moves, clouds
-- input: spacePerMove, list of clouds - cumulus[0] or thunder[1]
-- output: a list of clouds landed on completing a circle given spacesPerMove
-- |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
circularList :: Int -> [Int] -> [Int]
circularList 0 _ = []
circularList spacesPerMove lstClouds = map head . take listOfLandings . tail . chunksOf spacesPerMove $ cycle lstClouds
    where
        listOfLandings 
            | (length lstClouds `mod` spacesPerMove) == 0 = length lstClouds `div` spacesPerMove
            | otherwise = length lstClouds



energyRemaining :: [Int] -> Int
energyRemaining [] = 0
energyRemaining landedOnClouds = 100 - (thunderCloudsHit + baseEnergyUsed)
    where
        baseEnergyUsed = length landedOnClouds
        thunderCloudsHit = 2 * length (filter (1 ==) landedOnClouds)


