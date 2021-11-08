--module Main where
--main = interact $ show . tail . map read . words
 

import Data.List

--main :: IO ()
main = do
    inputBudget <- getLine
    inputKeyboards <- getLine
    inputDrives <- getLine
    print . show $ getMoneySpent inputBudget inputKeyboards inputDrives


getMoneySpent :: String -> String -> String -> Integer 
getMoneySpent budgetLine keyboardLine drivesLine
    | isEmpty sumItems = -1
    | otherwise = head sumItems
        where
            drives = map read $ words drivesLine :: [Integer]
            keyboards = map read $ words keyboardLine :: [Integer]
            budget = read . head $ words budgetLine :: Integer
            sumItems = sortBy (flip compare) $ filter (<= budget) .
                        map sumPair $ [ (x,y) | x <- keyboards, y <- drives]


sumPair :: Num a => (a,a) -> a
sumPair (a,b) = a + b

isEmpty :: [a] -> Bool
isEmpty  [] = True
isEmpty  x = False
