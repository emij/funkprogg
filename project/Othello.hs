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
import Control.Monad
-------------------------------------------------------------------------

-- Othello consists of blocks
data Othello = Othello { rows :: [Block] }
 deriving (Show)
-- Each block consits of a cell
type Block = [Cell]
-- Each cell may contain a disk
type Cell = Maybe Disk
-- A disk can be Black or White
data Disk = Black | White
 deriving (Show, Eq)
-- A position on the Othello board
type Pos = (Int, Int)

type Direction = (Int, Int)
-- A player consists of a name and a disk (his color)
data Player = Player { name :: Name, disk :: Disk }
 deriving (Show)
type Name = String

oSize :: Int
oSize = 8

main :: IO ()
main = gameLoop createGameBoard (Player "Player1" White) (Player "Player2" Black)

gameLoop :: Othello -> Player -> Player -> IO ()
gameLoop oth p nP = do
  printOthello oth
  -- Print and save possible moves
  let playableMoves = playablePos oth p
  when (null playableMoves) (do 
          let opponentPlayableMoves = playablePos oth nP
          if null opponentPlayableMoves then 
            printWinner oth p nP
            -- How do we handle a early exit?
          else 
            gameLoop oth nP p
        )

  let iPlayableMoves = zip [1..] playableMoves
  let iPMString = intercalate ", " [ show i ++ ":" ++ show pos | (i, pos) <- iPlayableMoves ]
  putStrLn iPMString
  putStrLn $ name p ++ "'s (" ++ show (disk p) ++ ") turn!"

  -- Player selects a move from the list.
  index <- getPlay (length iPlayableMoves)

  -- We generate the new othello from the play
  let newoth = playDisk oth (playableMoves !! (index - 1)) p

  -- If game is finished display winner else next player turn
  if isFinished newoth then
    printWinner newoth p nP
  else do
    putStrLn "Next player turn"
    gameLoop newoth nP p

getPlay :: Int -> IO Int
getPlay maxI = do
  putStr $ "Please select valid index (1-" ++ show maxI ++ "):\n"
  input <- getLine

  if validNum input && inRange (1, maxI) (read input :: Int)
  then return $ read input
  else getPlay maxI

-- Need own function as isDigit on an empty list returns True
validNum :: String -> Bool
validNum []     = False
validNum str = all isDigit str

printWinner :: Othello -> Player -> Player -> IO ()
printWinner oth p1 p2 = do 
  putStrLn "#########################"
  putStrLn "####   Final board   ####"
  putStrLn "#########################"
  printOthello oth
  let winningPlayer = winner oth p1 p2
  putStrLn "#########################"
  if isNothing winningPlayer then
    putStrLn $ "It was a draw! " ++ scoreString oth p1 p2 
  else 
    putStrLn $ "Gratulations " 
              ++ name (fromJust winningPlayer) 
              ++ "! You won with score " 
              ++ scoreString oth p1 p2
  putStrLn "#########################"

-- Given a state of a Othello, determine who is the winner
winner :: Othello -> Player -> Player -> Maybe Player
winner oth p1 p2
  | score oth p1 > score oth p2 = Just p1
  | score oth p2 > score oth p1 = Just p2
  | otherwise                     = Nothing

-- Score of a player in a Othello state
score :: Othello -> Player -> Int
score oth p = length [ d | d <- catMaybes (concat (rows oth)), d == disk p ]

-- The current game score of the Othello
scoreString :: Othello -> Player -> Player -> String
scoreString oth p1 p2 = show (score oth p1) ++ " - " ++ show (score oth p2)

-------------------------------------------------------------------------

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

-------------------------------------------------------------------------

-- Creates all possible blocks from a Pos i an Othello
blocks :: Othello -> Pos -> [Block]
blocks oth pos = [ block oth pos dir | dir <- directions ]

block :: Othello -> Pos -> (Int, Int) -> Block
block oth pos dir
  | valid nextPos = cell oth nextPos : block oth nextPos dir
  | otherwise = []
  where nextPos = stepPos pos dir

directions :: [Direction]
directions = [ (dX, dY) | dX <- [-1,0,1], dY <- [-1,0,1] ]

cell :: Othello -> Pos -> Cell
cell oth (x, y)= rows oth !! y !! x

valid :: Pos -> Bool
valid (x, y) = inRange (0, oSize-1) x && inRange (0, oSize-1) y

-------------------------------------------------------------------------

playable :: Othello -> Pos -> Disk -> Bool
playable oth pos c
  | occupied oth pos = False
  | otherwise = or [ playableBlock block | block <- blocks oth pos]
    where playableBlock [] = False
          playableBlock (b:bs)
            | isNothing b || b == Just c = False
            | otherwise = myDisk (dropWhile (==b) bs)
          myDisk []    = False
          myDisk (q:_) = q == Just c

-- Determines if a cell is occupied or not
occupied :: Othello -> Pos -> Bool
occupied oth pos = isJust (cell oth pos)

-- Returns all playable positions for a player
playablePos :: Othello -> Player -> [Pos]
playablePos oth p = [ (x,y) | x <- [0..oSize-1],
                              y <- [0..oSize-1],
                              playable oth (x,y) (disk p)]

-------------------------------------------------------------------------

-- Player places a disk at a position
playDisk :: Othello -> Pos -> Player -> Othello
playDisk oth pos p = flipDirections (placeDisk oth pos d) pos d $ flippingDirections oth pos d 
  where d = disk p

-- Update a position of a disk with a new disk
placeDisk :: Othello -> Pos -> Disk -> Othello
placeDisk oth (x, y) d = Othello $ rows oth !!= (y, updatedRow)
      where updatedRow = row !!= (x, Just d)
            row        = rows oth !! y

flipDirections :: Othello -> Pos -> Disk -> [Direction] -> Othello
flipDirections oth _ _ [] = oth
flipDirections oth p d (dir:dirs) = flipDirections (flipLine oth p dir d) p d dirs

-- Flip in a direction until we hit the same disk type.
flipLine :: Othello -> Pos -> Direction -> Disk -> Othello
flipLine oth pos dir d
  | fromJust (cell oth nextPos) /= d = flipLine (flipPos oth nextPos) nextPos dir d
  | otherwise = oth
  where nextPos = stepPos pos dir

-- Check which directions should be flipped
flippingDirections :: Othello -> Pos -> Disk -> [Direction]
flippingDirections oth pos d = [ dir | dir <- directions, shouldFlipDir oth pos dir d ]

-- Returns true if the disks in a direction ends with your own color.
shouldFlipDir :: Othello -> Pos -> Direction -> Disk -> Bool
shouldFlipDir oth pos dir d 
  | not (valid nextPos) = False
  | isNothing nextDisk = False
  | fromJust nextDisk == flipD d = shouldFlipDir oth nextPos dir d
  | otherwise = True
  where nextPos = stepPos pos dir
        nextDisk = cell oth nextPos

stepPos :: Pos -> Direction -> Pos
stepPos (x, y) (dX, dY) = (x + dX, y + dY)

flipD :: Disk -> Disk
flipD White = Black
flipD Black = White

flipPos :: Othello -> Pos -> Othello
flipPos oth pos
  | isNothing (cell oth pos) = oth
  | otherwise = placeDisk oth pos flippedDisk
    where flippedDisk = flipD $ fromJust $ cell oth pos

-- Given a list of elements, replace the element with a new on given index
(!!=) :: [a] -> (Int,a) -> [a]
(!!=) al (i, val)
  | length al <= i = error "index too large"
  | i < 0          = error "negative index"
  | otherwise      = a ++ val : as
          where (a,_:as) = splitAt i al

-------------------------------------------------------------------------
