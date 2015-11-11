module Blackjack where
import Cards
import Wrapper
import Test.QuickCheck()

{-
size hand2
    = size (Add (Card (Numeric 2) Hearts)
        (Add (Card Jack Spades) Empty))
    = 1 + size (Add (Card Jack Spades) Empty)
    = 2 + size (Empty)
= 2
-}

-- Temporary test hands used for debugging.
exH :: Hand
exH = (Add (Card (Numeric 2) Hearts) (Add (Card Jack Spades) Empty))
exH1 :: Hand
exH1 = (Add (Card Ace Hearts) (Add (Card Jack Spades) Empty))
exH2 :: Hand
exH2 = (Add (Card Ace Hearts) (Add (Card Ace Spades) Empty))
exH3 :: Hand
exH3 = (Add (Card King Hearts) (Add (Card Jack Spades) Empty))
exH4 :: Hand
exH4 = (Add (Card Ace Hearts) (Add (Card Ace Spades) (Add (Card Ace Diamonds) Empty)))

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

-- Check if hand-value is above 21.
gameOver :: Hand -> Bool
gameOver hand
  | value hand > 21 = True
  | otherwise = False

-- The winner.
winner :: Hand -> Hand -> Player
winner guest bank
  | gameOver guest = Bank
  | gameOver bank = Guest
  | value guest > value bank = Guest
  | otherwise = Bank

-- Number of Aces in a hand.
numberOfAces :: Hand -> Integer
numberOfAces Empty = 0
numberOfAces (Add card hand)
    | valueCard card == 11 = 1 + numberOfAces hand
    | otherwise = numberOfAces hand

-- <+ OnTopOf operator, adds on hand to another
(<+) :: Hand -> Hand -> Hand
Empty <+ h2 = h2
(Add card hand) <+ h2 = Add card (hand <+ h2)

-- Associative property of onTopOf (<+)
prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 = p1 <+ (p2 <+ p3) == (p1 <+ p2) <+ p3

-- Size property of onTopOf (<+)
prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf h1 h2 = size h1 + size h2 == size (h1 <+ h2)

-- Create a full deck
fullDeck :: Hand
fullDeck = fullSuit Spades
        <+ fullSuit Hearts
        <+ fullSuit Diamonds
        <+ fullSuit Clubs

-- Creates a full suit
fullSuit :: Suit -> Hand
fullSuit s = fullSuit' allRanks s
  where fullSuit' :: [Rank] -> Suit -> Hand
        fullSuit' [] _      = Empty
        fullSuit' (r:sr) s' = Add Card {rank=r, suit=s'} Empty <+ fullSuit' sr s'

        allRanks :: [Rank]
        allRanks = [Numeric n | n <- [2..10]] ++ [Jack, Queen, King, Ace]

-- Draws the first card from the deck and adds it to the hand
draw :: Hand -> Hand -> (Hand, Hand)
draw Empty _ = error "draw: The deck is empty"
draw (Add card deck) hand = (deck, (Add card hand))

-- Makes the banks play
playBank :: Hand -> Hand
playBank deck = playBank' (deck, Empty)

playBank' :: (Hand,Hand) -> Hand
playBank' (deck,bankHand)
    | value bankHand < 16 = playBank' (deck', bankHand')
    | otherwise = bankHand
    where (deck', bankHand') = draw deck bankHand

--shuffle :: stdGen -> Hand -> Hand
