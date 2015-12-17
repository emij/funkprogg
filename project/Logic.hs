module Logic where

import Board
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


type Direction = (Int, Int)
-- A player consists of a name and a disk (his color)
data Player = Player { name :: Name, disk :: Disk }
 deriving (Show)
type Name = String


-- Given a state of a Othello, determine who is the winner
winner :: Othello -> Player -> Player -> Maybe Player
winner oth p1 p2
  | score oth p1 > score oth p2 = Just p1
  | score oth p2 > score oth p1 = Just p2
  | otherwise                     = Nothing

-- Score of a player in a Othello state
score :: Othello -> Player -> Int
score oth p = length [ d | d <- catMaybes (concat (rows oth)), d == disk p ]

-------------------------------------------------------------------------

-- Creates all possible blocks from a Pos i an Othello
blocks :: Othello -> Pos -> [Block]
blocks oth pos = [ block oth pos dir | dir <- directions ]

-- Extracts a block given a pos and a direction
block :: Othello -> Pos -> Direction -> Block
block oth pos dir
  | valid nextPos = cell oth nextPos : block oth nextPos dir
  | otherwise = []
  where nextPos = stepPos pos dir

-- Returns all possible directions
-- Non moving direction is removed (0,0), would work anyway because
-- it is only used when checking if a position should be flipped
-- or when flipping disks. Both these function would exit directly.
--
directions :: [Direction]
directions = [ (dX, dY) | dX <- [-1,0,1], dY <- [-1,0,1], (dX, dY) /= (0,0) ]

-------------------------------------------------------------------------

-- Returns all playable positions for a player
playablePos :: Othello -> Disk -> [Pos]
playablePos oth d = [ (x,y) | x <- [0..oSize-1],
                              y <- [0..oSize-1],
                              playable oth (x,y) d]

-- Determines if a position is playable or not
playable :: Othello -> Pos -> Disk -> Bool
playable oth pos c =  not (occupied oth pos)
                   && or [ playableBlock b | b <- blocks oth pos]
    where playableBlock []     = False
          playableBlock (b:bs) = b == Just (flipDisk c)
                              && myDisk (dropWhile (==b) bs)
          myDisk []    = False
          myDisk (q:_) = q == Just c

-- Determines if a cell is occupied or not
occupied :: Othello -> Pos -> Bool
occupied oth pos = isJust (cell oth pos)

-------------------------------------------------------------------------

-- Player places a disk at a position
playDisk :: Othello -> Pos -> Player -> Othello
playDisk oth pos p = flipDirections (placeDisk oth pos d) pos d flipDirs
  where d = disk p
        flipDirs = flippingDirections oth pos d

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
flippingDirections oth pos d = [ dir | dir <- directions,
                                      shouldFlipDir oth pos dir d ]

-- Returns true if the disks in a direction ends with your own color.
shouldFlipDir :: Othello -> Pos -> Direction -> Disk -> Bool
shouldFlipDir oth pos dir d = valid nextPos
                           && isJust nextDisk
                           && (fromJust nextDisk == d
                              || shouldFlipDir oth nextPos dir d)
  where nextPos = stepPos pos dir
        nextDisk = cell oth nextPos

prop_flipCorrect :: Othello -> Disk -> Int -> Bool
prop_flipCorrect oth d i = null pPos
                        || notElem  selPos (playablePos flippedOth d)
  where selIndex   = i `mod` length pPos
        pPos       = playablePos oth d
        selPos     = pPos !! selIndex
        flippedOth = flipDirections oth selPos d (flippingDirections oth selPos d)

stepPos :: Pos -> Direction -> Pos
stepPos (x, y) (dX, dY) = (x + dX, y + dY)

flipDisk :: Disk -> Disk
flipDisk White = Black
flipDisk Black = White

flipPos :: Othello -> Pos -> Othello
flipPos oth pos
  | isNothing (cell oth pos) = oth
  | otherwise = placeDisk oth pos flippedDisk
    where flippedDisk = flipDisk $ fromJust $ cell oth pos

prop_flipPos :: Othello -> Pos -> Bool
prop_flipPos oth (x,y) = isNothing (cell oth pos)
                      || (flipDisk (fromJust (cell oth pos))
                        == fromJust (cell (flipPos oth pos) pos))
  where pos = (x `mod` oSize, y `mod` oSize)

-----------------------------------------------------------------------

