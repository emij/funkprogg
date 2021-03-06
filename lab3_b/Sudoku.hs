module Sudoku where

import Test.QuickCheck
import Data.Maybe(isNothing, isJust, fromMaybe, fromJust, catMaybes, listToMaybe)
import Numeric
import System.IO
import Data.Char(digitToInt, isDigit)
import Data.Ix(inRange)
import Data.List.Split
import Data.List
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
cell :: Gen Cell
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
                      [transpose c | c <- chunksOf 3 (rows sud)])


prop_Blocks :: Sudoku -> Bool
prop_Blocks sud = length (blocks sud) == (3*9)

-------------------------------------------------------------------------
type Pos = (Int, Int)

-- Generates a list of positions of all the blanks in the Sudoku
blanks :: Sudoku -> [Pos]
blanks sud = indexToPos [ elemIndices Nothing row | row <- rows sud]

-- Converts a list of lists with indexes to a list of positions
indexToPos :: [[Int]] -> [Pos]
indexToPos rr = iTP rr 0
  where iTP [] _ = []
        iTP (r:rx) rIndex = [ (rIndex,x) | x <- r] ++ iTP rx (rIndex+1)

-- That all of the blanks reported really are blanks
-- Note: Maybe should check that rest return false on isBlank?
prop_Blanks :: Sudoku -> Bool
prop_Blanks sud = all (isBlank sud) (blanks sud)

-- Given a sudoku and a position, return true if it is blank
isBlank :: Sudoku -> Pos -> Bool
isBlank sud (r, c) = isNothing $ rows sud !! r !! c

-------------------------------------------------------------------------

-- Given a list of elements, replace the element with a new on given index
(!!=) :: [a] -> (Int,a) -> [a]
(!!=) al (i, val)
  | length al <= i = error "index too large"
  | i < 0          = error "negative index"
  | otherwise      = a ++ val : as
          where (a,_:as) = splitAt i al

-- Update a position of a cell with a new Cell
update :: Sudoku -> Pos -> Cell -> Sudoku
update sud (r, c) val = Sudoku $ rows sud !!= (r, updatedRow)
      where updatedRow = row !!= (c, val)
            row        = rows sud !! r

-- Verify that if we change a Soduku that position really have changed
-- Note: Maybee should check that rest of the cells are the same aswell?
prop_ValueUpdated :: Sudoku -> Pos -> Cell -> Bool
prop_ValueUpdated sud p val = rows updatedSudoku !! r !! c == val
      where updatedSudoku = update sud (r, c) val
            -- We wanted to use do and choose (0, 8) for the position but
            -- didnt manage to get it right.
            (r, c) = (fst p `mod` 9, snd p `mod` 9)

-- Given a Sudoku and a position, return the possible candidates
candidates :: Sudoku -> Pos -> [Int]
candidates sud (r, c) = [1..9] \\ nonCandidates
  where nonCandidates = nub $ catMaybes allExistingValues
        allExistingValues = rows sud !! r
                            ++ transpose (rows sud) !! c
                            ++ squareBlocks sud !! indexOfSquare
        indexOfSquare = r `div` 3 * 3 + c `div` 3

-------------------------------------------------------------------------

-- Verifies that we have a correct Sudoku before starting the recursion step
-- to solve the sudoku
solve :: Sudoku -> Maybe Sudoku
solve sud
  | isSudoku sud && isOkay sud = solve' sud
  | otherwise          = Nothing

-- Recursivly solve the sudoku, backtrack if necessary.
solve' :: Sudoku -> Maybe Sudoku
solve' sud
  | isSolved sud = Just sud
  | null (blanks sud) = Nothing
  | null $ candidates sud (head $ blanks sud) = Nothing
  | otherwise = listToMaybe $ catMaybes
          [ solve' (update sud pos (Just val)) | val <- candidates sud pos ]
      where pos = head $ blanks sud

-- Simple function for reading and solving sudokus from file
-- Should propably use readSudoku.
readAndSolve :: FilePath -> IO ()
readAndSolve path = do
                    file <- readFile path
                    let allLines = lines file
                    let sud = createSudoku allLines
                    printSudoku $ fromJust (solve sud)
                    return ()


-- Checks that any Cell containing a number in sud2
-- are maintained in sud1
isSolutionOf :: Sudoku -> Sudoku -> Bool
isSolutionOf sud1 sud2
  | isSolved sud1 = compCell (concat $ rows sud1) (concat $ rows sud2)
  | otherwise = False
  where compCell [] _          = True
        compCell _ []          = True
        compCell (o:os) (c:cs) = (isNothing c || (o == c))
                                    && compCell os cs

-- Got short on time so we have not yet implemented this propery
--prop_SolveSound :: Sudoku -> Property
