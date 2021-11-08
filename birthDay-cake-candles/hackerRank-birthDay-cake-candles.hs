import Data.List

main = do
    ageUserInput <- getLine
    candleSize <- getLine
    print $ tallestCandleCount $ map (read:: String -> Int) $ words candleSize

tallestCandleCount :: [Int] -> Int
tallestCandleCount [] = 0
tallestCandleCount [x] = 1
tallestCandleCount list = length . head $ groupBy (==) . reverse $ sort list
