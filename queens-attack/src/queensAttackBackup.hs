module Main where



import Data.List
import Data.Ord 
import Data.Function 



type SzBoard = Int
type Obstacles = [Piece]

data Coord = Coord Int Int deriving (Show,Ord,Eq)

data Direction = NoEval | DirUp | DirDown | DirLeft | DirRight | DirPsUp | DirPsDown | DirNsUp | DirNsDown deriving (Show,Ord,Eq)

data Queen = Queen { queensCoord :: Coord }

data Piece = Obstacle { obsCoord :: Coord
                      , diffFromQueen :: Coord
                      , pairDirDistFromEdge :: (Direction,Int) }
           deriving (Show,Eq)

-- | Test objects.....
lstObs = [ Obstacle {obsCoord = Coord 4 4, diffFromQueen = Coord 0 0, pairDirDistFromEdge = (NoEval, -1) },
           Obstacle {obsCoord = Coord 6 5, diffFromQueen = Coord 0 0 ,pairDirDistFromEdge = (NoEval, -1) },
           Obstacle {obsCoord = Coord 8 5 ,diffFromQueen = Coord 0 0 ,pairDirDistFromEdge = (NoEval, -1) },
           Obstacle {obsCoord = Coord 1 6 ,diffFromQueen = Coord 0 0 ,pairDirDistFromEdge = (NoEval, -1) },
           Obstacle {obsCoord = Coord 3 8 ,diffFromQueen = Coord 0 0 ,pairDirDistFromEdge = (NoEval, -1) },
           Obstacle {obsCoord = Coord 2 6 ,diffFromQueen = Coord 0 0 ,pairDirDistFromEdge = (NoEval, -1) },
           Obstacle {obsCoord = Coord 2 1 ,diffFromQueen = Coord 0 0 ,pairDirDistFromEdge = (NoEval, -1) }
         ]



myQueen = Queen {queensCoord = Coord 5 5}



main :: IO ()
main = do
    putStrLn "hello world"


-- composition
-- vectorsEliminated . closestObstacles  $ groupByDirection . sortByDistanceFromEdge $ map (calcDirectionFromQueen 8 myQueen) $ filter willCollide $ map (distanceFromQueen myQueen) lstObs 



getDistFromEdge :: Piece -> Int
getDistFromEdge = snd . pairDirDistFromEdge 



getDirection :: Piece -> Direction
getDirection = fst . pairDirDistFromEdge 



subCoords :: Coord -> Coord -> Coord
subCoords (Coord a b) (Coord x y) = Coord (a-x) (b-y)



coordA :: Coord -> Int
coordA (Coord a b) = a



coordB :: Coord -> Int
coordB (Coord a b) = b



willCollide :: Piece -> Bool
willCollide theObs 
    | a == 0 || b == 0 = True 
    | (abs(a) - abs(b)) == 0 = True
    | otherwise = False
    where
        a = coordA (diffFromQueen theObs)
        b = coordB (diffFromQueen theObs)





-- | distanceFromQueen -> calcDirectionFromQueen
distanceFromQueen :: Queen -> Piece -> Piece 
distanceFromQueen theQ theObs = Obstacle { obsCoord = obsCoord theObs
                                         , diffFromQueen = (subCoords (queensCoord theQ) (obsCoord theObs))
                                         , pairDirDistFromEdge = (pairDirDistFromEdge theObs)
                                         }



-- | calcDirectionFromQueen -> groupByDirection
calcDirectionFromQueen :: SzBoard -> Queen -> Piece -> Piece
calcDirectionFromQueen szBoard theQueen theObstacle
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



sortByDistanceFromEdge :: Obstacles -> Obstacles
sortByDistanceFromEdge = reverse . sortBy (comparing getDistFromEdge)



-- | obsSortByDirection -> closestObstacles
groupByDirection :: Obstacles -> [Obstacles]
groupByDirection = groupBy ( (==) `on` getDirection ) 



----------------------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------------------
--From here up everything seems to be working properly....||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
----------------------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------------------



-- | closestObstacles -> eliminatedByObstacles
closestObstacles :: [Obstacles] -> Obstacles
closestObstacles = map head



-- | eliminatedByObstacles -> remainingAttackVectors
vectorsEliminated :: Obstacles -> Int
vectorsEliminated  = sum . map getDistFromEdge

