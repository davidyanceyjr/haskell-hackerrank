module Main where

import Data.List 
import Data.Ord

main = interact $ show . magicPotion . tail . map read . words

magicPotion :: Ord a => Num a => [a] -> a
magicPotion [] = 0
magicPotion list@(x:xs)
    | result > 0 = 0
    | otherwise = abs result
    where
        canJump = x
        result = head $ sort $ map (canJump-) xs
