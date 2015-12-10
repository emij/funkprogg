module Othello where

import Test.QuickCheck
import Data.Maybe(isNothing, isJust, fromMaybe, fromJust, catMaybes, listToMaybe)
import Numeric
import System.IO
import Data.Char(digitToInt, isDigit)
import Data.Ix(inRange)
import Data.List.Split
import Data.List
import Data.Tuple
-------------------------------------------------------------------------

data Othello = Othello { rows :: [Block] }
 deriving ( Show, Eq )
type Block = [Cell]
-- We have chosen to call a Maybe Int a cell since its terms are more logical
-- in the sense of creating Othello
type Cell = Maybe Disk
data Disk = Black | White
type Pos = (Int, Int)


-- isSolved oth checks if oth is already solved, i.e. there are no blanks
isFinished :: Othello -> Bool
isFinished oth = (all.all) isJust $ rows oth

-------------------------------------------------------------------------

-- printOthello oth prints a representation of the othoku oth on the screen
printOthello :: Othello -> IO ()
printOthello oth = putStr $ unlines [concat l | l <- convToString oth]

-- Converts a Othello to a string representation
convToString :: Othello -> [[String]]
convToString oth = (map.map) convCellToString (rows oth)

-- Converts a cell to a string representation
convCellToString :: Cell -> String
convCellToString Nothing = "."
convCellToString (Just i) = show i

-------------------------------------------------------------------------

-- Creates all possible blocks from a Othello
blocks :: Othello -> Pos -> [Block]

-- Generates all squareBlocks as Blocks (Used to determine duplicates)
diagonals :: Othello -> Pos -> [Block]

horizontals :: Othello -> Pos -> [Block]

verticals :: Othello -> Pos -> [Block]
verticals oth pos = horizontals (transpose oth) (swap pos)

-------------------------------------------------------------------------

playable :: Othello -> Pos -> Bool

-- Update a position of a disk with a new disk
placeDisk :: Othello -> Pos -> Disk -> Othello
placeDisk oth (r, c) val = Othello $ rows oth !!= (r, updatedRow)
      where updatedRow = row !!= (c, val)
            row        = rows oth !! r

-- Given a list of elements, replace the element with a new on given index
(!!=) :: [a] -> (Int,a) -> [a]
(!!=) al (i, val)
  | length al <= i = error "index too large"
  | i < 0          = error "negative index"
  | otherwise      = a ++ val : as
          where (a,_:as) = splitAt i al


-------------------------------------------------------------------------
