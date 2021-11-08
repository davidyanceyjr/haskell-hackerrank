--module Main where
--main = interact $ show . tail . map read . words
 


main = do
    input <- getLine
    putStrLn $ militaryTime input

militaryTime :: String -> String
militaryTime civyTime@(x:y:ys)
    | clock == "PM" && (hour == 12) = (show2char hour) ++ take 6 ys
    | clock == "PM" = show2char (plus12 hour) ++ take 6 ys
    | clock == "AM" && (hour == 12) = show2char 0 ++ take 6 ys
    | otherwise = (show2char hour) ++ take 6 ys
    where 
      hour = read $ take 2 civyTime :: Int
      clock = drop (length civyTime - 2) civyTime

plus12 :: Int -> Int
plus12 num = num + 12

show2char :: Int -> String
show2char n 
    | length (show n) == 1 = "0" ++ show n
    | otherwise = show n
