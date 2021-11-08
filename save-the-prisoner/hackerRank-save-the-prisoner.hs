import Data.List

-- *****************************************************************************************
-- *****************************************************************************************
--                                      REFACTOR ME PLEASE
-- *****************************************************************************************
-- *****************************************************************************************
main = do
    userInput <- getContents
    mapM_ (putStrLn . show) $ prisonerToSave $ prepInput userInput

-- //The input needs to be given as a list.... 
prisonerToSave :: [Int] -> [Int]  --numPrisoners --numCandies --startingPosition
prisonerToSave [] = []
prisonerToSave [numPrisoners] = []
prisonerToSave [numPrisoners,numCandies] = []
prisonerToSave (numPrisoners:numCandies:startingPosition:rest) = boundryLimit warnPrisoner : prisonerToSave rest
    where
        warnPrisoner
            | remainingCandies == 0 && startingPosition == 1 = numPrisoners
            | numCandies <= numPrisoners = adjustedPosition + numCandies
            | otherwise = adjustedPosition + remainingCandies
        remainingCandies = numCandies `mod` numPrisoners
        adjustedPosition = startingPosition -1
        boundryLimit num 
            | num > numPrisoners = num - numPrisoners
            | otherwise = num

prepInput :: String -> [Int]
prepInput [] = [0] 
prepInput input = tail . map read $ words input


