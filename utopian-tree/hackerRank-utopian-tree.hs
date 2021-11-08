import Data.List
import qualified Data.Map as M
import Data.Maybe


-- The Constraints: 
--      1<=t<=10
--      0<= n<=60
--
--      therefore I will build a list for that constraint.
--

main = do
    userInput <- getContents
    mapM_ print $ map growth . tail $ map read $ words userInput 


-- build the list of cycle values up to the constrained value of 60
growthCycles :: Int -> Bool -> [Int]
growthCycles num flag
    | flag = num : growthCycles (num+1) False
    | otherwise = num : growthCycles (num*2) True

-- this function should be mapped over the input list of cycles.
growth :: Int -> Int
growth  0 = 1
growth  1 = 2
growth  2 = 3
growth  num = take 60 (growthCycles 1 False) !! num

