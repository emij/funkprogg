module Sudoku where

import Test.QuickCheck
import Data.Maybe(isNothing, isJust, fromMaybe, fromJust)
import Numeric
import System.IO
import Data.Char(digitToInt, isDigit)
import Data.List(nub, transpose, concat)
import Data.Ix(inRange)
import Data.List.Split
-------------------------------------------------------------------------


example :: Sudoku
example =
    Sudoku
      [ [Just 3, Just 6, Nothing,Nothing,Just 7, Just 1, Just 2, Nothing,Nothing]
      , [Nothing,Just 5, Nothing,Nothing,Nothing,Nothing,Just 1, Just 8, Nothing]
      , [Nothing,Nothing,Just 9, Just 2, Nothing,Just 4, Just 7, Nothing,Nothing]
      , [Nothing,Nothing,Nothing,Nothing,Just 1, Just 3, Nothing,Just 2, Just 8]
      , [Just 4, Nothing,Nothing,Just 5, Nothing,Just 2, Nothing,Nothing,Just 9]
      , [Just 2, Just 7, Nothing,Just 4, Just 6, Nothing,Nothing,Nothing,Nothing]
      , [Nothing,Nothing,Just 5, Just 3, Nothing,Just 8, Just 9, Nothing,Nothing]
      , [Nothing,Just 8, Just 3, Nothing,Nothing,Nothing,Nothing,Just 6, Nothing]
      , [Nothing,Nothing,Just 7, Just 6, Just 9, Nothing,Nothing,Just 4, Just 3]
      ]

data Sudoku = Sudoku { rows :: [Block] }
 deriving ( Show, Eq )
type Block = [Cell]
-- We have chosen to call a Maybe Int a cell since its terms are more logical
-- in the sense of creating Sudoku
type Cell = Maybe Int
-- allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku (replicate 9 (replicate 9 Nothing))

-- isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle
isSudoku :: Sudoku -> Bool
isSudoku sud =  isCorrLen (rows sud) &&
                all isCorrLen (rows sud) &&
                isNums sud

-- Verifies that a list of blocks or cells are of length 9
isCorrLen :: [a] -> Bool
isCorrLen a = length a == 9

-- Verifies that all cells has valid values
isNums :: Sudoku -> Bool
isNums sud = (all.all) isCorNum $ rows sud

-- Takes a cell and determines if it has a valid value
isCorNum :: Cell -> Bool
isCorNum Nothing = True
isCorNum pos = inRange (1, 9) (fromJust pos)

-- isSolved sud checks if sud is already solved, i.e. there are no blanks
isSolved :: Sudoku -> Bool
isSolved sud = (all.all) isJust $ rows sud

-------------------------------------------------------------------------

-- printSudoku sud prints a representation of the sudoku sud on the screen
printSudoku :: Sudoku -> IO ()
printSudoku sud = putStr $ unlines [concat l | l <- convToString sud]

-- Converts a Sudoku to a string representation
convToString :: Sudoku -> [[String]]
convToString sud = (map.map) convCellToString (rows sud)

-- Converts a cell to a string representation
convCellToString :: Cell -> String
convCellToString Nothing = "."
convCellToString (Just i) = show i

-- readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku path = do
                    file <- readFile path
                    let allLines = lines file
                    let sudoku = createSudoku allLines
                    if isSudoku sudoku then return sudoku
                    else error "Bad Sudoku"

-- Creates a sudoku from a list of strings
createSudoku :: [String] -> Sudoku
createSudoku ll = Sudoku $ (map.map) createCell ll

-- Creates a Block from a list of strings
createCell :: Char -> Cell
createCell '.' = Nothing
createCell c   = Just $ if isDigit c then digitToInt c
                        else error "Bad Sudoku - Non digit in Sudoku"

-------------------------------------------------------------------------

-- cell generates an arbitrary cell in a Sudoku
cell :: Gen (Cell)
cell = frequency [ (8, return Nothing),
                   (2, choose (1,9) >>= return . Just)
                    ]

-- an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- sequence [ sequence [ cell | j <- [1..9] ] | i <- [1..9] ]
       return (Sudoku rows)

prop_Sudoku :: Sudoku -> Bool
prop_Sudoku = isSudoku

-------------------------------------------------------------------------

-- Verifies that a Sudoku is okay (No duplicates)
isOkay :: Sudoku -> Bool
isOkay sud = all isOkayBlock (blocks sud)

-- Verifies that a block is okay (No duplicates)
isOkayBlock :: Block -> Bool
isOkayBlock block = containsDuplicates [ c | c <- block, isJust c ]
    where containsDuplicates c =
            length c == length (nub c)

-- Creates all possible blocks from a Sudoku
blocks :: Sudoku -> [Block]
blocks sud = rows sud ++ transpose (rows sud) ++ squareBlocks sud

-- Generates all squareBlocks as Blocks (Used to determine duplicates)
squareBlocks :: Sudoku -> [Block]
squareBlocks sud = chunksOf 9
                    (concat $ concat
                      [transpose c | c <- (chunksOf 3 (rows sud))])


prop_Blocks :: Sudoku -> Bool
prop_Blocks sud = (length $ blocks sud) == (3*9)
-------------------------------------------------------------------------
