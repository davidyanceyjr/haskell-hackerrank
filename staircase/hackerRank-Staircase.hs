import Data.List 

main = do
    userInput <- getLine
    mapM_ putStrLn $ createStaircase $ read userInput

--Staircase..
--
createStaircase :: Int -> [String]
createStaircase 0 = []
createStaircase height
    | otherwise = buildStairs height 0
    where
        buildStairs size spaces
            | spaces == height = []
            | spaces == 0 = reverse $ replicate size '#' : buildStairs (size -1) (spaces + 1)
            | otherwise = concat ([replicate spaces ' '] ++ [replicate size '#']) : buildStairs (size -1) (spaces + 1)
