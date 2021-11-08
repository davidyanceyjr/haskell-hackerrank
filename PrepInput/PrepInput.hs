module PrepInput where


strToIntLst :: String -> [Int]
strToIntLst [] = [0] 
strToIntLst scoresAsStrings = map read $ words scoresAsStrings

