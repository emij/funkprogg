module Blackjack where
import Cards
import Wrapper
import Test.QuickCheck

{-
size hand2
    = size (Add (Card (Numeric 2) Hearts)
        (Add (Card Jack Spades) Empty))
    = 1 + size (Add (Card Jack Spades) Empty)
    = 2 + size (Empty)
= 2
-}

-- Returns an Empty hand.
empty :: Hand
empty = Empty

-- The value of a hand.
value :: Hand -> Integer
value hand
  | value' hand <= 21 = value' hand
  | otherwise         = value' hand - (numberOfAces hand * 10)
      where value' Empty = 0
            value' (Add card hand') = valueCard card + value' hand'

-- The value of a card.
valueCard :: Card -> Integer
valueCard (Card r _) = valueRank r

-- The rank of a card.
valueRank :: Rank -> Integer
valueRank (Numeric n) = n
valueRank Ace = 11
valueRank _ = 10

-- Number of Aces in a hand.
numberOfAces :: Hand -> Integer
numberOfAces Empty = 0
numberOfAces (Add card hand)
    | rank card == Ace = 1 + numberOfAces hand
    | otherwise = numberOfAces hand

-- Check if hand-value is above 21.
gameOver :: Hand -> Bool
gameOver hand = value hand > 21

-- The winner.
winner :: Hand -> Hand -> Player
winner guest bank
  | gameOver guest = Bank
  | gameOver bank = Guest
  | value guest > value bank = Guest
  | otherwise = Bank
