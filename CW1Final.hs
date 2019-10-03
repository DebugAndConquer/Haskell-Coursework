-- GROUP 17 
-- Kirill Beznosov 961793
-- Artiom Serstobitov 962863
-- Eduard Zakarian 965217

{-# LANGUAGE ParallelListComp #-}
import System.Directory
import Data.Maybe
import Test.QuickCheck

-- Question 1
average :: Float -> Float -> Float -> Float
average x y z = (x + y + z) / 3

howManyAboveAverage1 :: Float -> Float -> Float -> Int
howManyAboveAverage1 x y z = 
  length (filter (> average x y z) [x, y, z])

howManyAboveAverage2 :: Float -> Float -> Float -> Int
howManyAboveAverage2 x y z = 
  length [xs | xs <- [x, y, z], xs > average x y z]
-- *Main> howManyAboveAverage1 965217 962863 961793
-- 1
-- *Main> howManyAboveAverage2 965217 962863 961793
-- 1
  
checkcorrectness x y z = 
  howManyAboveAverage1 x y z == howManyAboveAverage2 x y z
-- *Main> quickCheck checkcorrectness
-- +++ OK, passed 100 tests.

-- Question 2
alfredo :: Int -> Int -> Float
alfredo d t =  
  (pi * ((fromIntegral d) / 2) ^ 2 * 0.001) + ((fromIntegral t) * pi * ((fromIntegral d) / 2) ^ 2 * 0.002)

bambiniIsMoreExpensiveThanFamiglia = 
  alfredo 16 6 > alfredo 32 2
-- Famiglia is more expensive than Bambini
-- *Main> bambiniIsMoreExpensiveThanFamiglia
-- False

-- Question 3
divides :: Integer -> Integer -> Bool
divides x y = y `mod` x == 0

prime :: Integer -> Bool
prime n = n > 1 && and [not (divides x n) | x <- [2 .. (n - 1)]]

allprimes :: [Integer]
allprimes = [x | x <- [2 ..], prime x]


allprimesBetween :: Integer -> Integer -> [Integer]
allprimesBetween x y = [xs | xs <- [x .. y], prime xs]

primeTest :: [Bool]
primeTest = map prime [1 ..]

primeTest2 :: [(Integer,Bool)]
primeTest2 = [(x, prime x) | x <- [1 ..]]

allprimesTill :: Integer -> [Integer]
allprimesTill y = [x | x <- [2 .. y], prime x]

-- Using length to count prime twins
-- Using filter to delete all entries which are not prime twins
-- Using a lambda expression which checks rather the difference
-- between two prime numbers is actually equals to 2
-- Using zip to compare all elements of the list to each other
-- without comparing the number with itself
primeTwins :: Integer -> Int
primeTwins z = 
  length 
    (filter 
      (\(x, y) -> y - x == 2) 
      (zip (allprimesTill z) (tail (allprimesTill z))))
-- There are 61 pair of twins amongst the first 2000 primes
-- *Main> primeTwins 2000
-- 61
    
-- Question 4
expmod :: Integer -> Integer -> Integer -> Integer
expmod m 0 n = 1
expmod m e n = (expmod m (e - 1) n * m) `mod` n 

expmodfast :: Integer -> Integer -> Integer -> Integer
expmodfast m 0 n = 1
expmodfast m e n = 
  if e `mod` 2 == 0
    then (expmodfast m (e `div` 2) n * m) `mod` n
    else expmod m e n
-- *Main> expmodfast 9562 965217 2
-- 0
-- *Main> expmodfast 95 965217 2
-- 1
-- *Main> expmodfast 95 965217 10
-- 5

-- Question 5

-- a)

-- There should be minimum 3 rows of elements with minimum
-- 3 elements in each row (so we have at least a 3x3 grid.
-- Second part of the expression checks that the length of 
-- all sublists is the same
valid :: [[Bool]] -> Bool
valid xss = 
  (length xss >= 3 && all (>= 3) (map length xss)) &&
  allTheSame (map length xss)

-- Returns True if all elements in the list are the same
allTheSame :: (Eq a) => [a] -> Bool
allTheSame xs = all (== head xs) (tail xs)

-- b)

-- Recursively iterating through all values and applying
-- aliveOrDead function.
-- If we get to the end of a sublist, we add a '\n'	 
showGrid :: [[Bool]] -> String
showGrid []           = []
showGrid [[]]         = "\n"
showGrid ([]:xss)     = "\n" ++ showGrid xss
showGrid ((x:xs):xss) = aliveOrDead x ++ showGrid (xs : xss)

-- Returns a '0' if the cell is alive and a '.' otherwise
aliveOrDead :: Bool -> String
aliveOrDead x = 
  if x
    then "0"
    else "."

-- Projects the string with values to the screen
printGrid :: [[Bool]] -> IO ()
printGrid xss = putStr (showGrid xss) 

-- Cuts the string, so the first row of characters is deleted 
-- The function is used for recursive calls in next functions
filterString :: String -> String
filterString [] = []
filterString (x:xs) =
  if x /= '\n'
    then filterString xs
    else xs

-- Projects the first row of characters until the '\n' 
projectFirstRow :: String -> String
projectFirstRow [] = []
projectFirstRow (x:xs) = 
  if x /= '\n'
    then x : projectFirstRow xs
    else []

-- Splits the string by lists of rows (\n separates the row)
splitString :: String -> [String]
splitString [] = [] 
splitString (x:xs) = 
  if x /= '\n'
    then (x : projectFirstRow xs) : splitString (filterString xs)
    else splitString xs

-- Produces a list of values which are valid 
-- for alive and dead cells
-- This method is used in readGrid to join all elements
partialAppend :: String -> [Bool]
partialAppend [] = []
partialAppend (x:xs)
  | x == '0'  = True : partialAppend xs
  | x == '.'  = False : partialAppend xs
  | otherwise = partialAppend xs

-- c)

-- Creates a list for every row and maps 0 to True and . to False 
-- All these sublists are then stored in one big list
readGrid :: String -> [[Bool]]
readGrid [] = []
readGrid ('\n': xs) = [] : (readGrid xs)
readGrid xs = [partialAppend y | y <- splitString xs]

-- d)

-- Counts the amount of alive cells in the list
neighbours :: [Bool] -> Int
neighbours xs = length (filter (== True) xs)

-- Decides if the new cell is going to be alive or dead
newCell :: Int -> Bool -> Bool
newCell 3 False = True
newCell _ False = False
newCell n True = 
  if n < 2 || n > 3
    then False
    else True

-- The state of a cell with its x coordinate
type CellX = (Bool,Int)

-- The state of a cell with its x,y coordinates
type CellXY = ((Int, Int), Bool)

-- Produces a list of x coordinates for one line
makeX :: [Bool] -> [Int]
makeX [] = []
makeX (x:xs) = reverse (length (x:xs) : reverse (makeX xs))

-- Produces a list of y coordinates for all lines
makeY :: [[Bool]] -> [Int]
makeY [] = []
makeY (xs:xss) = reverse (length (xs : xss) : reverse (makeY xss))

-- Joins a cell with its x coordinate 
joinX :: [Bool] -> [Int] -> [CellX]
joinX [] _ = []
joinX _ [] = []
joinX (x:xs) (z:zs) = (x,z) : joinX xs zs

-- Getter for an x attribute from CellX
getXCellX :: CellX -> Int
getXCellX (b,x) = x

-- Getter for a Bool attribute from CellX
getBoolCellX :: CellX -> Bool
getBoolCellX (b,x) = b

-- Getter for a Bool attribute from CellXY
getBoolCellXY :: CellXY -> Bool
getBoolCellXY ((x,y),b) = b

-- Transforms CellX to CellXY adding y coordinate to it
cellXToCellXY :: [CellX] -> Int -> [CellXY]
cellXToCellXY xs y = [((getXCellX x, y), getBoolCellX x) | x <- xs]

-- Makes a list of cells with x,y coordinates
gridCoord :: [[Bool]] -> [[CellXY]]
gridCoord xss = [cellXToCellXY (joinX xs (makeX xs)) y 
                 | xs <- xss 
                 | y <- (makeY xss)
                 ]

-- Takes a grid and returns a new grid which 
-- is a one step evolution forward
step :: [[Bool]] -> [[Bool]]
step xss = 
  if valid xss
    then map (changeBool (gridCoord xss)) (gridCoord xss)
    else error "Invalid grid"

-- Activates evolution process
changeBool :: [[CellXY]] -> [CellXY] -> [Bool]
changeBool xss xs = map (neighbourCheck xss) xs

-- Checks states of the neighbours of each cell and returns this  
-- information to 'newCell' and 'neighbours' functions in order 
-- to commit a full step
neighbourCheck :: [[CellXY]] -> CellXY -> Bool
neighbourCheck (xs:xss) ((x,y),b) = 
  if y == 1
    then if x == length xs
           then newCell (neighbours [west, sWest, south]) b
           else if x == 1
                  then newCell (neighbours [south, sEast, east]) b
                  else newCell (neighbours [west, sWest, south, sEast, east]) b
    else if y == length (xs:xss)
           then if x == length xs
                  then newCell (neighbours [north, nWest, west]) b
                  else if x == 1
                         then newCell (neighbours [east, nEast, north]) b
                         else newCell (neighbours [east, nEast, north, nWest, west]) b
           else if x == 1
                  then newCell (neighbours [south, sEast, east, nEast, north]) b
                  else if x == length xs
                         then newCell (neighbours [north, nWest, west, sWest, south]) b
                         else newCell (neighbours [north, nWest, west, sWest, south, sEast, east, nEast]) b

    where west = cellFinder (xs:xss) (x-1) y
          sWest = cellFinder (xs:xss) (x-1) (y+1)
          south = cellFinder (xs:xss) x (y+1)
          sEast = cellFinder (xs:xss) (x+1) (y+1)
          east = cellFinder (xs:xss) (x+1) y
          nEast = cellFinder (xs:xss) (x+1) (y-1)
          north = cellFinder (xs:xss) x (y-1)
          nWest = cellFinder (xs:xss) (x-1) (y-1)

-- Searches the cell by given coordinates and returns it's state 
cellFinder :: [[CellXY]] -> Int -> Int -> Bool
cellFinder xss x y = getBoolCellXY ((xss !! (y - 1)) !! (x - 1))

-- e)

-- Loads the grid from the input file
loadGridFile:: String -> IO [[Bool]]
loadGridFile xs =
  do
    content <- readFile xs
    return (readGrid (content))