module PE3 where

data Cell = SpaceCraft Int | Sand | Rock Int | Pit deriving (Eq, Read, Show)

type Grid = [[Cell]]
type Coordinate = (Int, Int)

data Move = North | East | South | West | PickUp | PutDown deriving (Eq, Read, Show)

data Robot = Robot { name :: String,
                     location :: Coordinate,
                     capacity :: Int,
                     energy :: Int,
                     storage :: Int } deriving (Read, Show)

-------------------------------------------------------------------------------------------
--------------------------------- DO NOT CHANGE ABOVE -------------------------------------
------------- DUMMY IMPLEMENTATIONS ARE GIVEN TO PROVIDE A COMPILABLE TEMPLATE ------------
------------------- REPLACE THEM WITH YOUR COMPILABLE IMPLEMENTATIONS ---------------------
-------------------------------------------------------------------------------------------
-------------------------------------- PART I ---------------------------------------------

isInGrid :: Grid -> Coordinate -> Bool
isInGrid grid coor = if (fst coor >= 0 && fst coor < length (head grid)) && (snd coor >= 0 && snd coor < length grid)
                        then True
                        else False

-------------------------------------------------------------------------------------------

totalCount :: Grid -> Int
totalCount grid = rockCount grid ((length grid) - 1)

rockCount :: Grid -> Int -> Int
rockCount grid (-1) = 0
rockCount grid n = (sum [rocks cell | cell <- grid !! n, isRock cell]) + rockCount grid (n - 1)

isRock :: Cell -> Bool
isRock (Rock _) = True
isRock _ = False

rocks :: Cell -> Int
rocks (Rock r) = r
rocks _ = 0

-------------------------------------------------------------------------------------------

coordinatesOfPits :: Grid -> [Coordinate]
coordinatesOfPits grid = reverse $ pitHelper grid ((length grid) - 1) ((length (head grid)) - 1) []

pitHelper :: Grid -> Int -> Int -> [Coordinate] -> [Coordinate]
pitHelper grid _ (-1) pit_coordinates = pit_coordinates
pitHelper grid (-1) column pit_coordinates = pitHelper grid ((length grid) - 1) (column - 1) pit_coordinates
pitHelper grid row column pit_coordinates
    | isPit ((grid !! row) !! column) = pitHelper grid (row - 1) column (pit_coordinates ++ [(column, row)])
    | otherwise = pitHelper grid (row - 1) column pit_coordinates

isPit :: Cell -> Bool
isPit Pit = True
isPit _ = False

-------------------------------------------------------------------------------------------

tracePath :: Grid -> Robot -> [Move] -> [Coordinate]
tracePath grid robot moves = fst (path grid robot moves [])

path :: Grid -> Robot -> [Move] -> [Coordinate] -> ([Coordinate],Robot)
path grid robot [] coordinates = (coordinates,robot)
path grid robot (x:xs) coordinates
    | x == North = if (energy robot) >= 1 && isInGrid grid (fst (location robot), (snd (location robot)) - 1) && not (elem (location robot) (coordinatesOfPits grid))
                    then path grid robot{energy = (energy robot) - 1, location = (fst (location robot), (snd (location robot)) - 1)} xs (coordinates ++ [(fst (location robot), (snd (location robot)) - 1)])
                    else path grid robot{energy = (energy robot) - 1} xs (coordinates ++ [(location robot)])
    
    | x == South = if (energy robot) >= 1 && isInGrid grid (fst (location robot), (snd (location robot)) + 1) && not (elem (location robot) (coordinatesOfPits grid))
                    then path grid robot{energy = (energy robot) - 1, location = (fst (location robot), (snd (location robot)) + 1)} xs (coordinates ++ [(fst (location robot), (snd (location robot)) + 1)])
                    else path grid robot{energy = (energy robot) - 1} xs (coordinates ++ [(location robot)])
    
    | x == East = if (energy robot) >= 1 && isInGrid grid ((fst (location robot)) + 1, snd (location robot)) && not (elem (location robot) (coordinatesOfPits grid))
                    then path grid robot{energy = (energy robot) - 1, location = ((fst (location robot)) + 1, snd (location robot))} xs (coordinates ++ [((fst (location robot)) + 1, snd (location robot))])
                    else path grid robot{energy = (energy robot) - 1} xs (coordinates ++ [(location robot)])
    
    | x == West = if (energy robot) >= 1 && isInGrid grid ((fst (location robot)) - 1, snd (location robot)) && not (elem (location robot) (coordinatesOfPits grid))
                    then path grid robot{energy = (energy robot) - 1, location = ((fst (location robot)) - 1, snd (location robot))} xs (coordinates ++ [((fst (location robot)) - 1, snd (location robot))])
                    else path grid robot{energy = (energy robot) - 1} xs (coordinates ++ [(location robot)])
    
    | x == PickUp = if (storage robot) < (capacity robot)
                        then path grid robot{energy = (energy robot) - 5, storage = (storage robot) + 1} xs (coordinates ++ [(location robot)])
                        else path grid robot{energy = (energy robot) - 5} xs (coordinates ++ [(location robot)])
    
    | x == PutDown = if (storage robot) > 0
                        then path grid robot{energy = (energy robot) - 3, storage = (storage robot) - 1} xs (coordinates ++ [(location robot)])
                        else path grid robot{energy = (energy robot) - 3} xs (coordinates ++ [(location robot)])

------------------------------------- PART II ----------------------------------------------

energiseRobots :: Grid -> [Robot] -> [Robot]
energiseRobots grid robots = energiseHelper grid robots []

energiseHelper :: Grid -> [Robot] -> [Robot] -> [Robot]
energiseHelper grid [] energized = energized
energiseHelper grid (robot:others) energized = energiseHelper grid others (energized ++ [robot{energy = min 100 ((energy robot) + (gain (location robot) (findSpaceCraft grid ((length grid) - 1) ((length (head grid)) - 1))))}])

findSpaceCraft :: Grid -> Int -> Int -> Coordinate
findSpaceCraft grid (-1) column = findSpaceCraft grid ((length grid) - 1) (column - 1)
findSpaceCraft grid row column
    | isSpaceCraft ((grid !! row) !! column) = (column, row)
    | otherwise = findSpaceCraft grid (row - 1) column

isSpaceCraft :: Cell -> Bool
isSpaceCraft (SpaceCraft n) = True
isSpaceCraft _ = False

gain :: Coordinate -> Coordinate -> Int
gain robot_location spacecraft_location = max 0 (100 - difference * 20)
    where difference = (abs (fst robot_location - fst spacecraft_location)) + (abs (snd robot_location - snd spacecraft_location))

-------------------------------------------------------------------------------------------

applyMoves :: Grid -> Robot -> [Move] -> (Grid, Robot)
applyMoves grid robot moves = (grid, (snd (path grid robot moves [])){energy = final})
                                where final = max 0 (energy (snd (path grid robot moves [])))
                                