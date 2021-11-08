-- see https://www.hackerrank.com/challenges/cut-the-sticks/problem
-- ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--

import Data.List


main :: IO ()
main = do
    userInput <- getContents
    mapM_ (putStrLn . show) $ cutSticks . sort $ formatInput userInput



-- formatInput producing a list of Int's..
-- ||||||||||||||||||||||||||||||||||||||| 
formatInput :: String -> [Int]
formatInput theString = tail . map read $ words theString


cutSticks :: [Int] -> [Int]
cutSticks [] = []
cutSticks list@(x:xs) = length list : cutSticks (noZeroes (minusMe list))
    where 
          minusMe = map (flip (-) x )
          noZeroes = filter (>0) 
