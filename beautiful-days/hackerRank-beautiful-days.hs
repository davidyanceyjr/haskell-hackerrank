import Data.List

main = do
    userInput <- getLine
    let toInt = (map read $ words userInput)::[Int]
    print . length $ filter (==0) . map (`mod` last toInt) . beautifulDays $ reverseTheNums toInt


-- reverse the numbers in the String returning a list
-- of Int's 
reverseTheNums :: [Int] -> [[Int]]
reverseTheNums [] = []
reverseTheNums [x] = [[x]]
reverseTheNums (x:y:z) = [xTOy , xOTy]
    where 
          xTOy = [x..y]
          xOTy = map (read . reverse . show) xTOy


-- map over list to determine which days are beautiful
beautifulDays :: [[Int]] -> [Int]
beautifulDays [] = []
beautifulDays (x:y) = map abs $ zipWith (-) x $ concat y


