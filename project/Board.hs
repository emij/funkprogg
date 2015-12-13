module Board where

import Test.QuickCheck
import Data.Maybe(isNothing, isJust, fromMaybe, fromJust, catMaybes, listToMaybe)
import Numeric
import System.IO
import Data.Char(digitToInt, isDigit)
import Data.Ix(inRange)
import Data.List.Split
import Data.List
import Data.Tuple
import Control.Monad

-- Othello consists of blocks
data Othello = Othello { rows :: [Block] }
 deriving (Show)
-- Each block consists of cells
type Block = [Cell]
-- Each cell may contain a disk
type Cell = Maybe Disk
-- A disk can be Black or White
data Disk = Black | White
 deriving (Show, Eq)
-- A position on the Othello board
type Pos = (Int, Int)

-- The size of the Othello
oSize :: Int
oSize = 8

blankOthello :: Othello
blankOthello = Othello (replicate oSize (replicate oSize Nothing))

-- Creates a Othello with the initial configuration
createGameBoard :: Othello
createGameBoard = placeDisks blankOthello 
  [((c1,c1), White),
   ((c1,c2), Black),
   ((c2,c1), Black),
   ((c2,c2), White)
  ]
  where c1 = quot oSize 2 - 1
        c2 = quot oSize 2

-- Given a list containing tuples of position and disk places the disks on the board
placeDisks :: Othello -> [(Pos, Disk)] -> Othello
placeDisks oth []              = oth
placeDisks oth ((pos, d):xs) = placeDisks (placeDisk oth pos d) xs

-- Update a position of a disk with a new disk
placeDisk :: Othello -> Pos -> Disk -> Othello
placeDisk oth (x, y) d = Othello $ rows oth !!= (y, updatedRow)
      where updatedRow = row !!= (x, Just d)
            row        = rows oth !! y

-- Checks if there are any empty places on a board
isFinished :: Othello -> Bool
isFinished oth = (all.all) isJust $ rows oth

-------------------------------------------------------------------------

-- printOthello oth prints a representation of the othoku oth on the screen
printOthello :: Othello -> IO ()
printOthello oth = putStr $ unlines [concat (intersperse " " l) | l <- convToString oth]

-- Converts a Othello to a string representation
convToString :: Othello -> [[String]]
convToString oth = (map.map) convCellToString (rows oth)

-- Converts a cell to a string representation
convCellToString :: Cell -> String
convCellToString Nothing = "·"
convCellToString (Just i) = if i == Black then "□" else "■"
--
-------------------------------------------------------------------------

-- Returns the cell of a position in a othello
cell :: Othello -> Pos -> Cell
cell oth (x, y)= rows oth !! y !! x

-- Checks whether a position is valid or not
valid :: Pos -> Bool
valid (x, y) = inRange (0, oSize-1) x && inRange (0, oSize-1) y

-- Given a list of elements, replace the element with a new on given index
(!!=) :: [a] -> (Int,a) -> [a]
(!!=) al (i, val)
  | length al <= i = error "index too large"
  | i < 0          = error "negative index"
  | otherwise      = a ++ val : as
          where (a,_:as) = splitAt i al

