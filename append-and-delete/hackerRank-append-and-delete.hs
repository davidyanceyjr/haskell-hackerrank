-- see https://www.hackerrank.com/challenges/append-and-delete/problem
-- ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

import Data.List
import Data.List.Split


main :: IO ()
main = do
    userInput <- getContents
    putStrLn $ canConvert $ lines userInput

-- convert original string into mutated string.
-- input: list containing original string and string to convert to..
-- ouput: yes | no -- if original can be converted
-- ||||||||||||||||||||||||||||||||||||||||||||
canConvert :: [String] -> String
canConvert [] = "No"
canConvert [_,_] = "No"
canConvert (_:_:_:_:_) = "No"
canConvert [x] = "No"
canConvert [originalString,mutatedString,moves]
    | abs (szBase - szOriginal) >= numMoves = "No"
    | szBase == sameChar = "Yes"
    | szBase == szMutated = "Yes"
    | numMoves >= (szOriginal + szMutated) = "Yes"
    | minMoves == numMoves = "Yes"
    | otherwise = "No"
    where
        szOriginal = length originalString
        szMutated = length mutatedString
        numMoves = read moves::Int
        szBase = length (getBase originalString mutatedString)
        sameChar = length $ getBase originalString $ reverse mutatedString
        minMoves = abs (szBase - szOriginal) + abs (szMutated - szBase)

getBase :: String -> String -> String
getBase [] _ = ""
getBase _ [] = ""
getBase (x:xs) (y:ys)
    | x == y = x : getBase xs ys
    | otherwise = ""
