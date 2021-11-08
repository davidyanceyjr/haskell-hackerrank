import Data.List 
import Data.Maybe
import Data.List.Split
import Data.Ord
import qualified Data.Set as IS 

main = do
    numExistingScores <- getLine
    existingScores <- getLine
    numAliceScores <- getLine
    aliceScores <- getLine
    let aliceInts = prepInput False aliceScores
    let leaderBoard = IS.fromList (map read $ words existingScores ::[Int])     
    mapM_ print $ scoresToPosition leaderBoard aliceInts

-- *************************************************************************************************
-- *************************************************************************************************
-- *************************************************************************************************
--  This is the solution
-- *************************************************************************************************
-- *************************************************************************************************
-- *************************************************************************************************
-- calculate position of player score from the origina list... 
--
scoresToPosition :: IS.Set Int -> [Int] -> [Int]
scoresToPosition _ [] = []
scoresToPosition rankings (x:xs)
    | IS.null rankings = [0]
    | x < min = IS.size rankings + 1 : scoresToPosition rankings xs
    | x == min = IS.size rankings : scoresToPosition rankings xs
    | x >= max = 1 : scoresToPosition rankings xs
    | otherwise = oldIndex : scoresToPosition rankings xs
    where
        min = IS.findMin rankings
        max = IS.findMax rankings
        nearestValue = fromMaybe (-1) $ IS.lookupLE x rankings
        indexOfNearestValue = fromMaybe (-1) $ IS.lookupIndex nearestValue rankings
        oldIndex = IS.size rankings - indexOfNearestValue

-- **********************************************************************************
-- **********************************************************************************
-- Binary breakdown of the leaderBoard .
--

binaryBreakDown :: [Int] -> Int -> Int
binaryBreakDown [] _    = 0
binaryBreakDown [x] num
    | num < x           = 2
    | otherwise         = 1
binaryBreakDown _ 0     = 0
binaryBreakDown leaderBoard score 
    | score > head leaderBoard            = 1
    | score < last leaderBoard            = sizeOfLeaderboard + 1
    | score < (leaderBoard !! pivotPoint) = pivotPoint + binaryBreakDown lowerHalf score
    | otherwise                           = binaryBreakDown upperHalf score
    where
        sizeOfLeaderboard                 = length leaderBoard
        pivotPoint                        = sizeOfLeaderboard `div` 2
        lowerHalf                         = drop pivotPoint leaderBoard
        upperHalf                         = take pivotPoint leaderBoard

-- Prepp the input : convert to list of strings...
--                 : convert strings to Int...
--                 : if flag set remove duplicates
--

prepInput :: Bool -> String -> [Int]
prepInput _ [] = [0] 
prepInput flag scoresAsStrings 
    | not flag = map read $ words scoresAsStrings
    | otherwise = rmDups . map read $ words scoresAsStrings

-- remove duplicates from [Int]
--

rmDups :: [Int] -> [Int]
rmDups [] = []
rmDups [x] = [x]
rmDups [x,y] 
    | x == y = [y]
    | otherwise = [x,y]
rmDups (x:y:ys)
    | x == y = rmDups (y:ys)
    | otherwise = x : rmDups (y:ys)
