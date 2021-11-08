import Data.List
import Data.Maybe
import qualified Data.Map as M



main = do 
     heightOfLetters <- getLine
     inputString <- getLine
     print . returnSolution $ map (fromMaybe 0) $ szOfHiLight inputString $ buildMap $ map read $ words heightOfLetters

buildMap :: [Int] -> M.Map Char Int
buildMap [] = M.fromList []
buildMap list = M.fromList $ zip ['a'..'z'] list

szOfHiLight :: String -> M.Map Char Int -> [Maybe Int]
szOfHiLight [] _ = []
szOfHiLight (x:xs) themap
    | M.null themap = []
    | otherwise = M.lookup x themap : szOfHiLight xs themap

returnSolution :: [Int] -> Int
returnSolution [] = 0
returnSolution list = maxElement * szList
    where
        maxElement = head $ reverse $ sort list
        szList = length list
