module Main where

main :: IO ()
main = interact $ unlines . map show . adjustGrades . map read . tail . words




calcGrade :: Int -> Int
calcGrade grade
    | grade >= 38 && (multOfFive - grade) < 3 = multOfFive
    | otherwise = grade
    where
        multOfFive = grade + (5 - grade `mod` 5)




adjustGrades :: [Int] -> [Int]
adjustGrades = map calcGrade

