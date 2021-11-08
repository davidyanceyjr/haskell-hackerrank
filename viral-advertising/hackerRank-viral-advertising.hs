import Data.List

main = do 
    userInput <- getLine
    print $ last $ take (read userInput) viralAd

-- Day -- Shared   -- Liked -- Cumulative
-- 1   -- 5 /2     -- 2 (*3)    -- 2
-- 2   -- 6 /2     -- 3 (*3)    -- 5
-- 3   -- 9 /2     -- 4 (*3)    -- 9
-- 4   -- 12 /2    -- 6 (*3)    -- 15

viralAd :: [Int]
viralAd = cycle 5 0
    where
        cycle likes' accu = shared likes' + accu : cycle (liked (shared likes')) (accu + shared likes')


shared :: Int -> Int
shared 0 = 0
shared num = num `div` 2

liked :: Int -> Int
liked 0 = 0
liked num = num * 3
