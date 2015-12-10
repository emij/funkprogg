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
 deriving (Show)
type Block = [Cell]
-- We have chosen to call a Maybe Int a cell since its terms are more logical
-- in the sense of creating Othello
type Cell = Maybe Disk
type Name = String
data Player = Player { name :: Name, disk :: Disk }
 deriving (Show)
data Disk = Black | White
 deriving (Show, Eq)
type Pos = (Int, Int)

gameLoop :: Othello -> Player -> Player -> IO ()
gameLoop oth pl nextPl = do
  printOthello oth 
  -- Print and save possible moves
  -- TODO

  putStrLn $ name pl ++ " (" ++ show (disk pl) ++ "): "

  -- Player selects a move from the list.
  i <- getChar
  -- play game with input
  -- TODO
  --

  putStrLn ""
  -- If game is finished display winner else next player turn
  if isFinished oth then do
    putStrLn "Player won won" 
   else do
    putStrLn "Next player turn"
    gameLoop oth nextPl pl

blankOthello :: Othello
blankOthello = Othello (replicate 8 (replicate 8 Nothing))

createGameBoard :: Othello
createGameBoard = placeDisks blankOthello [((3,3), White),
                                           ((3,4), Black),
                                           ((4,3), Black),
                                           ((4,4), White)
                                          ]

printPlayerName :: Player -> IO ()
printPlayerName pl = putStrLn $ name pl

printPlayerColor :: Player -> IO ()
printPlayerColor pl = putStrLn $ show $ disk pl


placeDisks :: Othello -> [(Pos, Disk)] -> Othello
placeDisks oth []              = oth
placeDisks oth ((pos,disk):xs) = placeDisks (placeDisk oth pos disk) xs

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
convCellToString (Just i) = if i == Black then "□" else "■"

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
