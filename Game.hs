{-# LANGUAGE GADTs, KindSignatures #-}

module Game where

--import Graphics.UI.GLUT
--import Data.Vector
import Data.List
import Graphics.Blank
--import qualified Data.Set as Set

-- Check for valid characters to 16 character Sudoku
minibox :: Char -> Bool
minibox '1' = True
minibox '2' = True
minibox '3' = True
minibox '4' = True
minibox '5' = True
minibox '6' = True
minibox '7' = True
minibox '8' = True
minibox '9' = True
minibox '0' = True
minibox ' ' = True
minibox 'a' = True
minibox 'b' = True
minibox 'c' = True
minibox 'd' = True
minibox 'e' = True
minibox 'f' = True
minibox  _  = False

-- Couple helpful data types to hold our information
charList :: [[Char]]
charList = [
           ['0', '1', '2', '3', '4', '5', '6', '7','8','9','a','b','c','d','e','f'],
           ['0', '1', '2', '3', '4', '5', '6', '7','8','9','a','b','c','d','e','f'],
           ['0', '1', '2', '3', '4', '5', '6', '7','8','9','a','b','c','d','e','f'],
           ['0', '1', '2', '3', '4', '5', '6', '7','8','9','a','b','c','d','e','f'],
           ['0', '1', '2', '3', '4', '5', '6', '7','8','9','a','b','c','d','e','f'],
           ['0', '1', '2', '3', '4', '5', '6', '7','8','9','a','b','c','d','e','f'],
           ['0', '1', '2', '3', '4', '5', '6', '7','8','9','a','b','c','d','e','f'],
           ['0', '1', '2', '3', '4', '5', '6', '7','8','9','a','b','c','d','e','f'],
           ['0', '1', '2', '3', '4', '5', '6', '7','8','9','a','b','c','d','e','f'],
           ['0', '1', '2', '3', '4', '5', '6', '7','8','9','a','b','c','d','e','f'],
           ['0', '1', '2', '3', '4', '5', '6', '7','8','9','a','b','c','d','e','f'],
           ['0', '1', '2', '3', '4', '5', '6', '7','8','9','a','b','c','d','e','f'],
           ['0', '1', '2', '3', '4', '5', '6', '7','8','9','a','b','c','d','e','f'],
           ['0', '1', '2', '3', '4', '5', '6', '7','8','9','a','b','c','d','e','f'],
           ['0', '1', '2', '3', '4', '5', '6', '7','8','9','a','b','c','d','e','f'],
           ['0', '1', '2', '3', '4', '5', '6', '7','8','9','a','b','c','d','e','f']
         ]

-- Empty list for reference
emptyList :: [[Char]]
emptyList = [
            [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ',' ',' ',' ',' ',' ',' ',' ',' '],
            [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ',' ',' ',' ',' ',' ',' ',' ',' '],
            [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ',' ',' ',' ',' ',' ',' ',' ',' '],
            [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ',' ',' ',' ',' ',' ',' ',' ',' '],
            [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ',' ',' ',' ',' ',' ',' ',' ',' '],
            [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ',' ',' ',' ',' ',' ',' ',' ',' '],
            [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ',' ',' ',' ',' ',' ',' ',' ',' '],
            [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ',' ',' ',' ',' ',' ',' ',' ',' '],
            [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ',' ',' ',' ',' ',' ',' ',' ',' '],
            [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ',' ',' ',' ',' ',' ',' ',' ',' '],
            [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ',' ',' ',' ',' ',' ',' ',' ',' '],
            [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ',' ',' ',' ',' ',' ',' ',' ',' '],
            [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ',' ',' ',' ',' ',' ',' ',' ',' '],
            [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ',' ',' ',' ',' ',' ',' ',' ',' '],
            [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ',' ',' ',' ',' ',' ',' ',' ',' '],
            [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ',' ',' ',' ',' ',' ',' ',' ',' ']
            ]


puzzle1 :: [[Char]]
puzzle1 = [
           [' ', '6', '2', ' ',   ' ', ' ', ' ', 'e',   '0', ' ', ' ', ' ',   ' ', 'd', 'c', ' '],
           ['a', ' ', ' ', ' ',   ' ', ' ', ' ', '0',   '8', ' ', ' ', 'c',   ' ', '3', ' ', '4'],
           ['5', ' ', ' ', '1',   '6', 'c', ' ', ' ',   'f', ' ', '3', '4',   '9', ' ', 'e', '0'],
           [' ', ' ', ' ', '4',   ' ', '3', ' ', ' ',   ' ', '1', ' ', '6',   'f', '8', '2', ' '],
         
           [' ', '3', ' ', ' ',   '1', '7', ' ', '6',   '4', '0', '8', ' ',   '2', ' ', 'b', 'a'],
           ['2', ' ', '0', '7',   '8', ' ', ' ', ' ',   '9', ' ', '6', ' ',   ' ', ' ', ' ', ' '],
           ['8', ' ', ' ', ' ',   ' ', '4', '2', ' ',   ' ', 'd', '7', 'e',   '0', ' ', ' ', ' '],
           [' ', ' ', '5', 'e',   'b', ' ', ' ', ' ',   ' ', 'f', ' ', '1',   ' ', ' ', '8', ' '],
         
           [' ', '5', ' ', ' ',   '7', ' ', '8', ' ',   ' ', ' ', ' ', 'b',   '1', '0', ' ', ' '],
           [' ', ' ', ' ', '8',   '5', 'd', 'b', ' ',   ' ', 'e', '0', ' ',   ' ', ' ', ' ', '7'],
           [' ', ' ', ' ', ' ',   ' ', 'a', ' ', '3',   ' ', ' ', ' ', 'f',   'c', '5', ' ', 'e'],
           ['d', '9', ' ', '0',   ' ', 'e', 'f', 'c',   '3', ' ', '5', '7',   ' ', ' ', 'a', ' '],
         
           [' ', 'f', '6', '9',   'a', ' ', '1', ' ',   ' ', ' ', 'c', ' ',   '3', ' ', ' ', ' '],
           ['1', 'a', ' ', '3',   'e', '2', ' ', 'f',   ' ', ' ', 'b', '8',   '5', ' ', ' ', '9'],
           ['b', ' ', '7', ' ',   'c', ' ', ' ', '4',   'e', ' ', ' ', ' ',   ' ', ' ', ' ', '8'],
           [' ', 'd', '8', ' ',   ' ', ' ', ' ', '7',   '5', ' ', ' ', ' ',   ' ', 'f', '4', ' ']
         ]

test1 :: [Char]
test1 = [' ', ' ', ' ', '8',   '5', 'd', 'b', ' ',   ' ', 'e', '0', ' ',   ' ', ' ', ' ', '7']

empty :: [Char]
empty = []

-- Return True if puzzle is solved
isSolved :: [[Char]] -> Bool
isSolved [] = True
isSolved a = noSpaces (map spaceInRow a)

noSpaces :: [Bool] -> Bool
noSpaces [] = True
noSpaces a = if (a !! 0 )
                 then False
                 else (noSpaces ( tail a ) )

spaceInRow :: [Char] -> Bool
spaceInRow [] = False
spaceInRow a = if ( a !! 0 ) == ' '
               then True
               else spaceInRow (tail a )

isSpace :: Char -> Bool
isSpace ' ' = True
isSpace _   = False


check :: Char -> Bool
check a = if a == ' '
             then True
             else False

-- Check if it's in the row
checkRow :: Char -> [Char] -> Bool
checkRow _ [] = False
checkRow a b  = if a == ( b !! 0 )
                then True
                else checkRow a (tail b)

-- Check for the row number of a list
getRowNum :: [Char] -> [[Char]] -> Int
getRowNum _ [] = -17
getRowNum a b = if a == ( b !! 0 )
                then 0
                else (1 + getRowNum a (tail b))

-- Generates master list of what cannot be done
start :: [[Char]] -> [[[Char]]]
start a = strongerUnion ( strongerUnion (getAllRowUnits a ) (getAllColumns a ) ) (getAllBoxUnits a )

fst4(x,_,_,_) = x

-- Function returns all boxes
getBoxes :: [[Char]] -> [[Char]]
getBoxes []= []
getBoxes a =  [
                intersect ( (fst (splitAt 4 ( a !! 0 ))) ++ (fst (splitAt 4 ( a !! 1 ))) ++ (fst (splitAt 4 ( a !! 2 ))) ++  (fst (splitAt 4 ( a !! 3 ))) ) nonUsed, 
                intersect ( (fst (splitAt 4 (snd (splitAt 4 ( a !! 0 ))))) ++ (fst ( splitAt 4 (snd (splitAt 4 ( a !! 1 ))))) ++ (fst ( splitAt 4 (snd (splitAt 4 ( a !! 2 )))))  ++  (fst ( splitAt 4 (snd (splitAt 4 ( a !! 3 ))))) ) nonUsed,
                intersect ( (fst (splitAt 4 (snd (splitAt 8 ( a !! 0 ))))) ++ (fst ( splitAt 4 (snd (splitAt 8 ( a !! 1 ))))) ++ (fst ( splitAt 4 (snd (splitAt 8 ( a !! 2 )))))  ++  (fst ( splitAt 4 (snd (splitAt 8 ( a !! 3 ))))) ) nonUsed,
                intersect ( (snd (splitAt 12 ( a !! 0 ))) ++ (snd (splitAt 12 ( a !! 1 ))) ++ (snd (splitAt 12 ( a !! 2 )))  ++  (snd (splitAt 12 ( a !! 3 ))) ) nonUsed
              ] ++ getBoxes ( snd (splitAt 4 a) )

getAllBoxUnits :: [[Char]] -> [[[Char]]]
getAllBoxUnits [] = []
getAllBoxUnits b = 
    [copy2 4 ( (getBoxes b) !! 0 ) ++ copy2 4 ( (getBoxes b) !! 1  ) ++ copy2 4 ( ( getBoxes b) !! 2  ) ++ copy2 4 ( ( getBoxes b ) !! 3 )] ++
    [copy2 4 ( (getBoxes b) !! 0 ) ++ copy2 4 ( (getBoxes b) !! 1  ) ++ copy2 4 ( ( getBoxes b) !! 2  ) ++ copy2 4 ( ( getBoxes b ) !! 3 )] ++
    [copy2 4 ( (getBoxes b) !! 0 ) ++ copy2 4 ( (getBoxes b) !! 1  ) ++ copy2 4 ( ( getBoxes b) !! 2  ) ++ copy2 4 ( ( getBoxes b ) !! 3 )] ++
    [copy2 4 ( (getBoxes b) !! 0 ) ++ copy2 4 ( (getBoxes b) !! 1  ) ++ copy2 4 ( ( getBoxes b) !! 2  ) ++ copy2 4 ( ( getBoxes b ) !! 3 )] ++
    getAllBoxUnits (snd ( splitAt 4 b ) ) 


--Pass in a puzzle get a list of all possible answers that
--do not exist in the same row
getAllRowUnits :: [[Char]] -> [[[Char]]]
getAllRowUnits a = map getRowUnits a

getRowUnits :: [Char] -> [[Char]]
getRowUnits a = copy2 16 (intersect a nonUsed)

-- Return a list of all possiblities not in the same
-- columns
getAllColumns :: [[Char]] -> [[[Char]]]
getAllColumns a = transpose $ getAllRowUnits $ transpose a


-- Union all list of Char lists together
strongerUnion :: [[[Char]]] -> [[[Char]]] -> [[[Char]]]
strongerUnion [] [] = []
strongerUnion a b = [strongUnion ( a !! 0 ) ( b !! 0 ) ] ++ strongerUnion ( tail a ) ( tail b )

-- Union all inididual Char lists together
strongUnion :: [[Char]] -> [[Char]] -> [[Char]]
strongUnion [] [] = []
strongUnion a b = [union ( a !! 0 ) (b !! 0)] ++ strongUnion (tail a ) ( tail b )  

--checkColumn :: Char -> [Char] -> Bool
--checkColumn a b = if 

--listColumn :: Int -> [Char]
--listColumn x = 


copy2 number item = [item | _ <- [ 1..number ]]





--clearRow :: [Char] -> [Char]
--clearRow a = 

-- Contains all possible answers for all rows
possiblities :: [[[Char]]]
possiblities = [[[]]]



nonUsed :: [Char]
nonUsed = ['0', '1', '2', '3', '4', '5', '6', '7','8','9','a','b','c','d','e','f']

--removeElement :: Char -> [Char] -> [Char]
--removeElement a b = map checkElement a b

checkElement :: Char -> Char -> Char
checkElement a b = if a == b 
                   then ' '
                   else b



--rowSolver :: [Char] -> [Char]
--rowSolver a = map insertEle



--insertElement :: Char -> Char
--insertElement a = if a = ' ' do 
usedElements :: [Char]
usedElements = []

-- Take a row and return it's possiblities
--checkRow :: [Char] -> [Char]
--checkRow a = 

--
getPossibilities :: Char -> [Char] -> [Char]
getPossibilities a b = if a == ' '
                       then b
                       else delete a nonUsed
