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
--charList :: [[Char]]
--charList = [
--           ['0', '1', '2', '3', '4', '5', '6', '7','8','9','a','b','c','d','e','f'],
--           ['0', '1', '2', '3', '4', '5', '6', '7','8','9','a','b','c','d','e','f'],
--           ['0', '1', '2', '3', '4', '5', '6', '7','8','9','a','b','c','d','e','f'],
--           ['0', '1', '2', '3', '4', '5', '6', '7','8','9','a','b','c','d','e','f'],
--           ['0', '1', '2', '3', '4', '5', '6', '7','8','9','a','b','c','d','e','f'],
--           ['0', '1', '2', '3', '4', '5', '6', '7','8','9','a','b','c','d','e','f'],
--           ['0', '1', '2', '3', '4', '5', '6', '7','8','9','a','b','c','d','e','f'],
--           ['0', '1', '2', '3', '4', '5', '6', '7','8','9','a','b','c','d','e','f'],
--           ['0', '1', '2', '3', '4', '5', '6', '7','8','9','a','b','c','d','e','f'],
--           ['0', '1', '2', '3', '4', '5', '6', '7','8','9','a','b','c','d','e','f'],
--           ['0', '1', '2', '3', '4', '5', '6', '7','8','9','a','b','c','d','e','f'],
--           ['0', '1', '2', '3', '4', '5', '6', '7','8','9','a','b','c','d','e','f'],
--           ['0', '1', '2', '3', '4', '5', '6', '7','8','9','a','b','c','d','e','f'],
--           ['0', '1', '2', '3', '4', '5', '6', '7','8','9','a','b','c','d','e','f'],
--           ['0', '1', '2', '3', '4', '5', '6', '7','8','9','a','b','c','d','e','f'],
--           ['0', '1', '2', '3', '4', '5', '6', '7','8','9','a','b','c','d','e','f']
--         ]

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

puzzle3 :: [[Char]]
puzzle3 = [ 
           "745269f03a8deb1c",
           "06fbc54d197ea238",
           "c8ae31b264f095d7",
           "d91378ea25bc064f",

           "2c7d5b98f64130ea",
           "3fb4d7a602e9c185",
           "ea61 2c3d857b94f",
           "9085fe14a3cb2d76",

           "5b4a2c37e09 186d",
           "13e8a65b4cd27f09",
           "f20794d18ba653ce",
           "6d9ce08f57134a2b",

           "852f1a0cbe64d793",
           "bed08f79c13a6452",
           "4736bd2e9f058ca1",
           "a1c943657d28feb0"
          ]

test1 :: [Char]
test1 = [' ', ' ', ' ', '8',   '5', 'd', 'b', ' ',   ' ', 'e', '0', ' ',   ' ', ' ', ' ', '7']

empty :: [Char]
empty = []

-- This inverses the triple matrix to show the possible values
-- rather than the values that cannot be in the matrix
inverseList :: [[[Char]]] -> [[[Char]]]
inverseList [] = []
inverseList a =  [ inverseRow ( a !! 0 ) ] ++ ( inverseList ( tail a ) )

inverseRow :: [[Char]] -> [[Char]]
inverseRow [] = []
inverseRow b = [(nonUsed \\ ( b !! 0 ))] ++ ( inverseRow (tail b ) )





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


-- Take list of Chars and make a new puzzle with derived
newPuzzle :: [[[Char]]] -> [[Char]]
newPuzzle [] = []
newPuzzle a = map newLine a

newLine :: [[Char]] -> [Char]
newLine [] = []
newLine a = map newVariable a

newVariable :: [Char] -> Char
newVariable a = if ( (length a) > 1 )
                then ' '
                else ( a !! 0 )

-- Generates master list of what cannot be done
start :: [[Char]] -> [[[Char]]]
start a =  inverseList ( strongerUnion ( strongerUnion (getAllRowUnits a ) (getAllColumns a ) ) (getAllBoxUnits a ) ) 

-- Take in current puzzle and replace all string with single characters
-- where the original puzzle had answers
derived :: [[Char]] -> [[[Char]]] -> [[[Char]]]
derived [] [] = []
derived a  b  = [ deriveRow ( a !! 0 ) ( b !! 0 ) ] ++ derived (tail a) ( tail b )

deriveRow :: [Char] -> [[Char]] -> [[Char]]
deriveRow [] [] = []
deriveRow a b = if   ( a !! 0 ) == ' '
                then [( b !! 0 )] ++ deriveRow (tail a) (tail b)
                else [[ a !! 0 ]] ++ deriveRow (tail a) (tail b)

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


-- Take puzzle return puzzle with a guess
changePuz :: [[Char]] -> [[Char]]
changePuz cl = 
  newPuzzle (changePuzMore (derived cl (start cl)))

-- Take derived and return a new derived that has exactly 1 guess in it
changePuzMore :: [[[Char]]] -> [[[Char]]]
changePuzMore [] = []
changePuzMore cl = if (changePuzRow True (cl !! 0)) == ( cl !! 0 ) 
                   then [ cl !! 0 ] ++ changePuzMore (tail cl)
                   else [(changePuzRow True (cl !! 0))] ++ (tail cl)

-- Take in a row return changed row
changePuzRow :: Bool -> [[Char]] -> [[Char]]
changePuzRow _ [] = []
changePuzRow continue row = 
  if (continue)
  then if ((Prelude.length (row !! 0) ) > 2 )
       then [row !! 0] ++ changePuzRow continue ( Prelude.tail row )
       else if (isTwo ( row !! 0 ))
            then [ [(row !! 0) !! 0] ] ++ changePuzRow False ( Prelude.tail row )
            else [row !! 0] ++ changePuzRow continue (Prelude.tail row)
  else [row !! 0] ++ changePuzRow continue (Prelude.tail row)           

isTwo :: [Char] -> Bool
isTwo str = if( ( Prelude.length str ) == 2 )
            then True
            else False

