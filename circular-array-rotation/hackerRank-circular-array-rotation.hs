import Data.List

main = do
    userInput <- getContents
    let preppedInput = prepInput userInput
    mapM_ (putStrLn . show) $ map (getOriginalList preppedInput !!) (getRotatedIndex preppedInput)

-- given a String return a list of integers (Int's)
-- ||||||||||||||||||||||||||||||||||||||||||||||||
prepInput :: String -> [Int]
prepInput []              = [0]
prepInput scoresAsStrings = map read $ words scoresAsStrings


-- newIndex. convert the indexes of a list, returning new indexes of rotated list
-- ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
newIndex :: Int -> Int -> Int -> Int -- szOfList -> rotations -> index -> newIndex
newIndex 0 _ _     = 0
newIndex _ 0 index = index
newIndex szOfList rotations position 
    | rotations == szOfList = position
    | position > szOfList - 1 = (szOfList + (position `mod` szOfList) - rotations) `mod` szOfList
    | otherwise = (szOfList + position - rotations) `mod` szOfList

-- getRotatedValue: convert a list of indexes into indexed values on a rotated list.
-- ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||| 
getRotatedIndex :: [Int] -> [Int]
getRotatedIndex []    = []
getRotatedIndex [_,_] = []
getRotatedIndex [x]   = [x]
getRotatedIndex (numElements:numRotations:numQueries:twoLists)
    | numElements  == 0 = []
    | numRotations == 0 = take numElements twoLists
    | otherwise = map (newIndex numElements numRotations) (drop numElements twoLists) 


-- return original list from input String. 
-- |||||||||||||||||||||||||||||||||||||||
getOriginalList :: [Int] -> [Int] -- input from prepInput, return original non-rotated list.
getOriginalList [] = []
getOriginalList [_] = [-1]
getOriginalList [_,_] = [-1]
getOriginalList  (numElements:numRotations:numQueries:twoLists) = take numElements twoLists
