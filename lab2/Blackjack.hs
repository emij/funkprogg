module Blackjack where
import Cards
import Wrapper
{-
size hand2
    = size (Add (Card (Numeric 2) Hearts)
        (Add (Card Jack Spades) Empty))
    = 1 + size (Add (Card Jack Spades) Empty)
    = 2 + size (Empty)
= 2
-}

empty :: Hand
empty = Empty

-- The value of a hand.

value :: Hand -> Integer
value Empty = 0
value (Add card hand) = valueCard card + value hand

-- The value of a card.

valueCard :: Card -> Integer
valueCard (Card r _) = valueRank r

-- The rank of a card.

valueRank :: Rank -> Integer
valueRank (Numeric n) = n
valueRank Ace = 11
valueRank _ = 10

-- Check if hand-value is above 21.

gameOver :: Hand -> Bool
gameOver hand
  | value hand > 21 = True
  | otherwise = False

-- The winner.

winner :: Hand -> Hand -> Player
winner gHand bHand
  | gameOver gHand = Bank
  | gameOver bHand = Guest
  | value gHand > value bHand = Guest
  | otherwise = Bank

-- Number of Aces in a hand.

numberOfAces :: Hand -> Integer
numberOfAces Empty = 0
numberOfAces (Add card hand)
    | valueCard card == 11 = 1 + numberOfAces hand
    | otherwise = numberOfAces hand

--(<+) :: Hand -> Hand -> Hand

--fullDeck :: Hand

--draw :: Hand -> Hand -> (Hand, Hand)

--playBank :: Hand -> Hand

--shuffle :: stdGen -> Hand -> Hand
