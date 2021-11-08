module Main where



import Data.List



main :: IO ()
main = do
    userInput <- getContents
    putStrLn $ show . sum . drop 1 $ reverse . sort . map length $ prepInput userInput



prepInput :: String -> [[Int]]
prepInput [] = []
prepInput userInput = groupBy (==) $ sort . tail . map read $ words userInput
