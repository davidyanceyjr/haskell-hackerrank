import Data.List

-- See hackerrank.com/challenges/find-digits/problem
--
-- main program entry point.
main :: IO ()
main = do
    userInput <- getContents
    mapM_ (putStrLn . show) $ map divisorsOfNum $ (tail $ map read $ words userInput)


-- input: integer
-- output: list of integer extracted from input
-- ex: 27 -> [2,7]
convertToDigits :: Int -> [Int]
convertToDigits 0 = []
convertToDigits num = num `mod`10 : convertToDigits (num `quot` 10)

-- input: an integer containing more than a single digit.
-- output: the number of the digits which evenly divide into the integer.
-- ex: 1200 - 1 and 2 are divisors of 1200 so the output is 2
-- ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
divisorsOfNum :: Int -> Int
divisorsOfNum num = length $ filter (isCompleteDivisor num) (convertToDigits num)


-- input: Int -> Int 
-- output: Bool
-- ex: 2 12 -> returns True since 2 evenly divides into 12.
isCompleteDivisor :: Int -> Int -> Bool
isCompleteDivisor _ 0 = False
isCompleteDivisor _ 1 = True
isCompleteDivisor dividend divisor
    | dividend `mod` divisor == 0 = True
    | otherwise = False 
