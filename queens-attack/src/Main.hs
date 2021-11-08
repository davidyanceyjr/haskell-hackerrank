module Main where

import System.IO
import Data.List
import Data.List.Split
import Data.Ord 
import Data.Function 



type SzBoard = Int
type Obstacles = [Obstacle]

data Coord = Coord Int Int deriving (Show,Ord,Eq)

data Direction = NoEval | DirUp | DirDown | DirLeft | DirRight | DirPsUp | DirPsDown | DirNsUp | DirNsDown deriving (Show,Ord,Eq)

data Queen = Queen { queensCoord :: Coord }

data Obstacle = Obstacle { obsCoord :: Coord
                      , diffFromQueen :: Coord
                      , pairDirDistFromEdge :: (Direction,Int) }
           deriving (Show,Eq)

-- | Test objects.....
--lstObs = [ Obstacle {obsCoord = Coord 0 0, diffFromQueen = Coord 0 0, pairDirDistFromEdge = (NoEval, -1) },
--           Obstacle {obsCoord = Coord 4 2, diffFromQueen = Coord 0 0 ,pairDirDistFromEdge = (NoEval, -1) },
--           Obstacle {obsCoord = Coord 8 5 ,diffFromQueen = Coord 0 0 ,pairDirDistFromEdge = (NoEval, -1) },
--           Obstacle {obsCoord = Coord 1 6 ,diffFromQueen = Coord 0 0 ,pairDirDistFromEdge = (NoEval, -1) },
--           Obstacle {obsCoord = Coord 3 8 ,diffFromQueen = Coord 0 0 ,pairDirDistFromEdge = (NoEval, -1) },
--           Obstacle {obsCoord = Coord 2 6 ,diffFromQueen = Coord 0 0 ,pairDirDistFromEdge = (NoEval, -1) },
--           Obstacle {obsCoord = Coord 2 1 ,diffFromQueen = Coord 0 0 ,pairDirDistFromEdge = (NoEval, -1) },
--           Obstacle {obsCoord = Coord 0 1 ,diffFromQueen = Coord 0 0 ,pairDirDistFromEdge = (NoEval, -1) },
--           Obstacle {obsCoord = Coord 2 2 ,diffFromQueen = Coord 0 0 ,pairDirDistFromEdge = (NoEval, -1) },
--           Obstacle {obsCoord = Coord 8 8 ,diffFromQueen = Coord 0 0 ,pairDirDistFromEdge = (NoEval, -1) },
--           Obstacle {obsCoord = Coord 0 0 ,diffFromQueen = Coord 0 0 ,pairDirDistFromEdge = (NoEval, -1) }
--        ]



szOfBoard = (head $ head testcase20)::Int
theQueen = createQueen $ head $ drop 1 testcase20
theObstacles = createListObstacles (drop 2 testcase20)
vectorsElminiated = sum . map maxFromTheEdge $ groupByDirection . map (calcDirectionFromQueen szOfBoard theQueen) $ filter willCollide $ map (distanceFromQueen theQueen ) $ filter isValidObstacle theObstacles
attackVectors = totalAttackVectors szOfBoard (queensCoord theQueen) 

main :: IO ()
main = do print "QueensAttack"
--    userInput <- getContents 
--    let formatInput = chunksOf 2 $ map read $ (words userInput)::[[Int]]
--    let szOfBoard = head $ head formatInput
--    let theQueen = createQueen $ head $ drop 1 formatInput
--    let theObstacles = createListObstacles (drop 2 formatInput)
--    let vectorsElminiated = sum . map maxFromTheEdge $ groupByDirection . sortByDirection . map (calcDirectionFromQueen szOfBoard theQueen) $ filter willCollide $ map (distanceFromQueen theQueen ) $ filter isValidObstacle theObstacles
--    let attackVectors = totalAttackVectors szOfBoard (queensCoord theQueen) 
--    print $ attackVectors - vectorsElminiated 

-- composition
--print . sum . map maxFromTheEdge $ groupByDirection . map (calcDirectionFromQueen 8 myQueen) $ filter willCollide $ map (distanceFromQueen myQueen ) $ filter isValidObstacle lstObs 
-- -------------------------------------------------------------------------^-------------------------------------------------------------------------------------------------
-- -------------------------------------------------------------------------| variable = SzBoard....--------------------------------------------------------------------------
createListObstacles :: [[Int]] -> Obstacles
createListObstacles [] = [createObstacle [0,0]] -- this obstacle will be filtered out by isValidObstacle
createListObstacles input = map createObstacle input 



createQueen :: [Int] -> Queen
createQueen []      = Queen {queensCoord = Coord 0 0}
createQueen [_]     = Queen {queensCoord = Coord 0 0}
createQueen [x,y]   = Queen {queensCoord = Coord x y}
createQueen (_:_:_) = Queen {queensCoord = Coord 0 0}



-- | Create input function turning this list of lists ::
-- [[5 2], [6 4], [2 4], [7 2], [8 1]] into a list of Obstacles whose
-- Coord match these values...
createObstacle :: [Int] -> Obstacle -- Note: takes a list of Int with 2 elements only..
createObstacle []      = Obstacle {obsCoord = Coord 0 0, diffFromQueen = Coord 0 0, pairDirDistFromEdge = (NoEval, -1)}
createObstacle [_]     = Obstacle {obsCoord = Coord 0 0, diffFromQueen = Coord 0 0, pairDirDistFromEdge = (NoEval, -1)}
createObstacle [x,y]   
    | x < 1 || y < 1   = Obstacle {obsCoord = Coord 0 0, diffFromQueen = Coord 0 0, pairDirDistFromEdge = (NoEval, -1)}
    | otherwise        = Obstacle {obsCoord = Coord x y, diffFromQueen = Coord 0 0, pairDirDistFromEdge = (NoEval, -1)}
createObstacle (_:_:_) = Obstacle {obsCoord = Coord 0 0, diffFromQueen = Coord 0 0, pairDirDistFromEdge = (NoEval, -1)}



-- | get the Distance from edge of the board of the obstacle.
-- | this will depend on the board size while the distanceFromQueen
-- | will be the same from board to board.
-- ----------------------------------------------------------------
getDistFromEdge :: Obstacle -> Int
getDistFromEdge = snd . pairDirDistFromEdge 


-- | get the Direction of this obstacle in relation to the Queen.
-- --------------------------------------------------------------
getDirection :: Obstacle -> Direction
getDirection = fst . pairDirDistFromEdge 


-- | is the Obstacle null == obstacle coordinates as 0 0..
-- -------------------------------------------------------
isValidObstacle :: Obstacle -> Bool
isValidObstacle obstacle 
    | firstCoord == 0 || sndCoord == 0 = False
    | otherwise  = True
    where
        firstCoord = coordA (obsCoord obstacle)
        sndCoord   = coordB (obsCoord obstacle)


isDirection :: Obstacle -> Direction -> Bool
isDirection theObstacle theDir
    | getDirection theObstacle == theDir = True
    | otherwise = False


subCoords :: Coord -> Coord -> Coord
subCoords (Coord a b) (Coord x y) = Coord (a-x) (b-y)



coordA :: Coord -> Int
coordA (Coord a b) = a



coordB :: Coord -> Int
coordB (Coord a b) = b



willCollide :: Obstacle -> Bool
willCollide theObs 
    | a == 0 || b == 0 = True 
    | (abs(a) - abs(b)) == 0 = True
    | otherwise = False
    where
        a = coordA (diffFromQueen theObs)
        b = coordB (diffFromQueen theObs)





-- | distanceFromQueen -> calcDirectionFromQueen
distanceFromQueen :: Queen -> Obstacle -> Obstacle 
distanceFromQueen theQ theObs = Obstacle { obsCoord = obsCoord theObs
                                         , diffFromQueen = (subCoords (queensCoord theQ) (obsCoord theObs))
                                         , pairDirDistFromEdge = (pairDirDistFromEdge theObs)
                                         }



-- | calcDirectionFromQueen -> groupByDirection
calcDirectionFromQueen :: SzBoard -> Queen -> Obstacle -> Obstacle
calcDirectionFromQueen szBoard thisQueen theObstacle
    | a < 0 && b > 0 = theObstacle {obsCoord = obsCoord theObstacle
                                   , diffFromQueen = diffFromQueen theObstacle
                                   , pairDirDistFromEdge = (DirNsUp, (abs(min (x - szBoard) (y - 1)) + 1))
                                   }
    | a < 0 && b < 0 = theObstacle {obsCoord = obsCoord theObstacle
                                   , diffFromQueen = diffFromQueen theObstacle
                                   , pairDirDistFromEdge = (DirPsUp, (abs(x - szBoard) + 1))
                                   }
    | a < 0 && b == 0 = theObstacle{obsCoord = obsCoord theObstacle
                                   , diffFromQueen = diffFromQueen theObstacle
                                   , pairDirDistFromEdge = (DirUp, (abs(x - szBoard ) + 1)) 
                                   }
    | a > 0 && b > 0 = theObstacle {obsCoord = obsCoord theObstacle
                                   , diffFromQueen = diffFromQueen theObstacle
                                   , pairDirDistFromEdge = (DirPsDown, y)
                                   }
    | a > 0 && b == 0 = theObstacle {obsCoord = obsCoord theObstacle
                                   , diffFromQueen = diffFromQueen theObstacle
                                   , pairDirDistFromEdge = (DirDown, x)
                                   }
    | a == 0 && b > 0 = theObstacle{obsCoord = obsCoord theObstacle
                                   , diffFromQueen = diffFromQueen theObstacle
                                   , pairDirDistFromEdge = (DirLeft, y)
                                   }
    | a == 0 && b < 0 = theObstacle{obsCoord = obsCoord theObstacle
                                   , diffFromQueen = diffFromQueen theObstacle
                                   , pairDirDistFromEdge = (DirRight, (abs(y - szBoard) + 1))
                                   }
    | a > 0 && b < 0 = theObstacle {obsCoord = obsCoord theObstacle
                                   , diffFromQueen = diffFromQueen theObstacle
                                   , pairDirDistFromEdge = (DirNsDown, (abs(min (x - 1) (y - szBoard)) + 1))
                                   }
    | otherwise = theObstacle {obsCoord = obsCoord theObstacle
                                   , diffFromQueen = diffFromQueen theObstacle
                                   , pairDirDistFromEdge = (NoEval, -1)
                                   }
    where
        a = coordA (diffFromQueen theObstacle)
        b = coordB (diffFromQueen theObstacle)
        x = coordA (obsCoord theObstacle)
        y = coordB (obsCoord theObstacle)




totalAttackVectors :: SzBoard -> Coord -> Int
totalAttackVectors size (Coord x y) 
            | x > size || y > size = (-1) 
            |otherwise = nonSlope + positiveSlope + negativeSlope
    where 
          nonSlope = 2 * size - 2
          positiveSlope = size - abs(x - y) - 1
          negativeSlope = size - abs((size + 1 -x) - y) - 1


-- | obsSortByDirection -> closestObstacles
-- NOTE: up down down up down up -> [[up],[down,down],[up],[down],[up]]
-- what we want                  -> [[up,up,up],[down,down,down]]
groupByDirection :: Obstacles -> [Obstacles]
groupByDirection [] = []
groupByDirection theObs = groupBy ( (==) `on` getDirection) theObs



sortByDirection :: Obstacles -> Obstacles
sortByDirection [] = []
sortByDirection theObs = sortBy ( compare `on` getDirection) theObs


-- | Take a list of Obstacles and return the closest object..
-- map over groupByDirection -> [Obstacles]
maxFromTheEdge :: Obstacles -> Int
maxFromTheEdge [] = 0
maxFromTheEdge [x] = getDistFromEdge x 
maxFromTheEdge [x,y] 
    | getDistFromEdge x > getDistFromEdge y = getDistFromEdge x
    | otherwise = getDistFromEdge y
maxFromTheEdge (x:y:xs) 
    | getDistFromEdge x > getDistFromEdge y = maxFromTheEdge (x:(tail xs))
    | otherwise = maxFromTheEdge xs



testcase06 = [[100, 100],[48, 81] ,[54, 87] ,[64, 97] ,[42, 75] ,[32, 65] ,[42, 87] ,[32, 97] ,[54, 75] 
              ,[64, 65] ,[48, 87] ,[48, 75] ,[54, 81] ,[42, 81] ,[45, 17] ,[14, 24] ,[35, 15] ,[95, 64]
              ,[63, 87] ,[25, 72] ,[71, 38] ,[96, 97] ,[16, 30] ,[60, 34] ,[31, 67] ,[26, 82] ,[20, 93]
              ,[81, 38] ,[51, 94] ,[75, 41] ,[79, 84] ,[79, 65] ,[76, 80] ,[52, 87] ,[81, 54] ,[89, 52]
              ,[20, 31] ,[10, 41] ,[32, 73] ,[83, 98] ,[87, 61] ,[82, 52] ,[80, 64] ,[82, 46] ,[49, 21]
              ,[73, 86] ,[37, 70] ,[43, 12] ,[94, 28] ,[10, 93] ,[52, 25] ,[50, 61] ,[52, 68] ,[52, 23]
              ,[60, 91] ,[79, 17] ,[93, 82] ,[12, 18] ,[75, 64] ,[69, 69] ,[94, 74] ,[61, 61] ,[46, 57]
              ,[67, 45] ,[96, 64] ,[83, 89] ,[58, 87] ,[76, 53] ,[79, 21] ,[94, 70] ,[16, 10] ,[50, 82]
              ,[92, 20] ,[40, 51] ,[49, 28] ,[51, 82] ,[35, 16] ,[15, 86] ,[78, 89] ,[41, 98] ,[70, 46]
              ,[79, 79] ,[24, 40] ,[91, 13] ,[59, 73] ,[35, 32] ,[40, 31] ,[14, 31] ,[71, 35] ,[96, 18]
              ,[27, 39] ,[28, 38] ,[41, 36] ,[31, 63] ,[52, 48] ,[81, 25] ,[49, 90] ,[32, 65] ,[25, 45]
              ,[63, 94] ,[89, 50] ,[43, 41]]

testcase20 = [[88587 , 9]  , [20001, 20003] , [20001, 20002] ,
             [20001, 20004], [20000, 20003] , [20002, 20003] ,
             [20000, 20004], [20000, 20002] , [20002, 20004] , 
             [20002, 20002], [564, 323]]::[[Int]]
             
myTestCase = [[6,4], [6,5],[6,6],
              [5,4], [5,6],
              [4,4], [4,5],[4,6]]::[[Int]]
