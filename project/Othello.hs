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
 deriving ( Show)
type Block = [Cell]
-- We have chosen to call a Maybe Int a cell since its terms are more logical
-- in the sense of creating Othello
type Cell = Maybe Disk
data Disk = Black | White
 deriving (Show)
type Pos = (Int, Int)

createGameBoard :: Othello
createGameBoard = Othello (replicate 8 (replicate 8 Nothing))


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
convCellToString Nothing = "O"
convCellToString (Just i) = show i

-------------------------------------------------------------------------

-- Creates all possible blocks from a Pos i an Othello
blocks :: Othello -> Pos -> [Block]
blocks = undefined

diagonals :: Othello -> Pos -> [Block]
diagonals = undefined

horizontals :: Othello -> Pos -> [Block]
horizontals = undefined

verticals :: Othello -> Pos -> [Block]
verticals oth pos = undefined
-------------------------------------------------------------------------

playable :: Othello -> Pos -> Bool
playable = undefined

-- Update a position of a disk with a new disk
placeDisk :: Othello -> Pos -> Disk -> Othello
placeDisk oth (r, c) val = Othello $ rows oth !!= (r, updatedRow)
      where updatedRow = row !!= (c, Just val)
            row        = rows oth !! r

-- Given a list of elements, replace the element with a new on given index
(!!=) :: [a] -> (Int,a) -> [a]
(!!=) al (i, val)
  | length al <= i = error "index too large"
  | i < 0          = error "negative index"
  | otherwise      = a ++ val : as
          where (a,_:as) = splitAt i al


-------------------------------------------------------------------------
