import Data.List
import Data.List.Split


main :: IO ()
main = do
    userInput <- getContents
    mapM_ (putStrLn . show) $ mapListToSquares $ chunksOf 2 $ strToIntLst userInput
    
strToIntLst :: String -> [Int]
strToIntLst [] = [0] 
strToIntLst scoresAsStrings = tail . map read $ words scoresAsStrings

numSquares :: Int -> Int -> Int
numSquares 0 _ = 0
numSquares _ 0 = 0
numSquares minRange maxRange = 1 + (floor sqrtMax) - (ceiling sqrtMin)
    where
        sqrtMin = sqrt $ fromIntegral minRange
        sqrtMax = sqrt $ fromIntegral maxRange

mapListToSquares :: [[Int]] -> [Int]
mapListToSquares [] = []
mapListToSquares [[]] = []
mapListToSquares [[_]] = []
mapListToSquares [(_:_:_:_)] = []
mapListToSquares ([]:_:_) = []
mapListToSquares ([_]:_:_) = []
mapListToSquares ((_:_:_:_):_:_) = []
mapListToSquares ([x,y]:xs) = numSquares x y : mapListToSquares xs

 


