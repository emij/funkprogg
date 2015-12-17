module Othello  where

import Logic
import Board
import Data.Maybe
import Data.Char(isDigit)
import Data.Ix(inRange)
import Data.List(intercalate)
import Control.Monad
-------------------------------------------------------------------------

main :: IO ()
main = gameLoop createGameBoard (Player "Player1" White) (Player "Player2" Black)

gameLoop :: Othello -> Player -> Player -> IO ()
gameLoop oth p nP = do
  putStrLn $ name p ++ "'s turn"
  printOthello oth
  -- Print and save possible moves
  let playableMoves = playablePos oth (disk p)
  when (null playableMoves) (do
          let opponentPlayableMoves = playablePos oth (disk nP)
          if null opponentPlayableMoves then
            printWinner oth p nP
          else do
            putStrLn $ "No possible moves for " ++ name p
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
  else gameLoop newoth nP p

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
  putStrLn "###############"
  putStrLn "# Final board #"
  putStrLn "###############"
  printOthello oth
  let winningPlayer = winner oth p1 p2
  putStrLn "###############"
  if isNothing winningPlayer then
    putStrLn $ "It was a draw! " ++ scoreString oth p1 p2
  else
    putStrLn $ "Gratulations "
              ++ name (fromJust winningPlayer)
              ++ "! You won with score "
              ++ scoreString oth p1 p2
  putStrLn "###############"

-- The current game score of the Othello
scoreString :: Othello -> Player -> Player -> String
scoreString oth p1 p2 = show (score oth p1) ++ " - " ++ show (score oth p2)
