module Sudoku where

import Test.QuickCheck
import Data.Maybe(isNothing, isJust, fromMaybe)
import Numeric
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

data Sudoku = Sudoku { rows :: [[Maybe Int]] }
 deriving ( Show, Eq )

-- allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku (replicate 9 (replicate 9 Nothing))

-- isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle
isSudoku :: Sudoku -> Bool
isSudoku sud =  isCorrLen (rows sud) &&
                and [ isCorrLen row | row <- rows sud] &&
                isNums sud

-- borde bryta ut listcomprehension till en egen funktion

isCorrLen :: [a] -> Bool
isCorrLen a = length a == 9

isNums :: Sudoku -> Bool
isNums sud = compareSoduku sud and isCorNum 

isCorNum :: Maybe Int -> Bool
isCorNum pos = (Just 0 < pos && pos <= Just 9) || isNothing pos


-- isSolved sud checks if sud is already solved, i.e. there are no blanks
isSolved :: Sudoku -> Bool
isSolved sud = not $ compareSoduku sud or isNothing 

compareSoduku sud fold func = fold [ fold [ func pos | pos <- row ] | row <- rows sud ]

-------------------------------------------------------------------------

-- printSudoku sud prints a representation of the sudoku sud on the screen
printSudoku :: Sudoku -> IO ()
printSudoku sud = putStr $ unlines [concat l | l <- convToString sud]

convToString :: Sudoku -> [[String]]
convToString sud = [[ convNumToString pos | pos <- row ] | row <- rows sud ]

convNumToString :: Maybe Int -> String
convNumToString i
    | isNothing i = "."
    | otherwise = show $ fromMaybe 0 i



-- readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku = undefined

-------------------------------------------------------------------------

-- cell generates an arbitrary cell in a Sudoku
cell :: Gen (Maybe Int)
cell = oneof [do val <- choose (1,9)
                 return (Just val),
                 return Nothing]

-- an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- sequence [ sequence [ cell | j <- [1..9] ] | i <- [1..9] ]
       return (Sudoku rows)

-------------------------------------------------------------------------
