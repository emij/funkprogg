module Blackjack where 
import Cards
import Wrapper

empty :: Hand
empty = Empty

value :: Hand -> Integer
value Empty = 0
value (Add card hand) = valueCard card + value hand

valueCard :: Card -> Integer
valueCard (Card r _) = valueRank r

valueRank :: Rank -> Integer
valueRank (Numeric n) = n
valueRank Ace = 11
valueRank _ = 10

gameOver :: Hand -> Bool
gameOver hand
  | value hand > 21 = True
  | otherwise = False

winner :: Hand -> Hand -> Player
winner gHand bHand
  | gameOver gHand = Bank
  | gameOver bHand = Guest
  | value gHand > value bHand = Guest
  | otherwise = Bank

--(<+) :: Hand -> Hand -> Hand

--fullDeck :: Hand

--draw :: Hand -> Hand -> (Hand, Hand)

--playBank :: Hand -> Hand

--shuffle :: stdGen -> Hand -> Hand 
