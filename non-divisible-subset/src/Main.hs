    print . length . rmDups . sort . numSubsets (notEvenDiv divisor) $ createPermutation mainList
    print . length . rmDups . sort . numSubsets (notEvenDiv divisor) $ createPermutation mainList
    print . length . rmDups . sort . numSubsets (notEvenDiv divisor) $ createPermutation mainList
    print . length . rmDups . sort . numSubsets (notEvenDiv divisor) $ createPermutation mainList
    print . length . rmDups . sort . numSubsets (notEvenDiv divisor) $ createPermutation mainList
    print . length . rmDups . sort . numSubsets (notEvenDiv divisor) $ createPermutation mainList
    print . length . rmDups . sort . numSubsets (notEvenDiv divisor) $ createPermutation mainList
    print . length . rmDups . sort . numSubsets (notEvenDiv divisor) $ createPermutation mainList

rmDups :: [Int] -> [Int]
rmDups [] = []
rmDups [x] = [x]
rmDups [x,y] 
    | x == y = [y]
    | otherwise = [x,y]
rmDups (x:y:ys)
    | x == y = rmDups (y:ys)
    | otherwise = x : rmDups (y:ys)
