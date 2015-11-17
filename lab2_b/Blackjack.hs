module Blackjack where
import Cards
import Wrapper
import Test.QuickCheck hiding (shuffle)
import System.Random

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
exH5 :: Hand
exH5 = Add (Card (Numeric 9) Hearts) $ Add (Card (Numeric 8) Spades) $ Add (Card (Numeric 5) Hearts) Empty

-- Returns an Empty hand.
empty :: Hand
empty = Empty

-- The value of a hand.
value :: Hand -> Integer
value hand
  | handValue <= 21 = handValue
  | otherwise       = handValue - (numberOfAces hand * 10)
      where value' Empty            = 0
            value' (Add card hand') = valueCard card + value' hand'
            handValue = value' hand

-- The value of a card.
valueCard :: Card -> Integer
valueCard (Card r _) = valueRank r

-- The rank of a card.
valueRank :: Rank -> Integer
valueRank (Numeric n) = n
valueRank Ace         = 11
valueRank _           = 10

-- Check if hand-value is above 21.
gameOver :: Hand -> Bool
gameOver hand = value hand > 21

-- The winner.
winner :: Hand -> Hand -> Player
winner guest bank
  | gameOver guest           = Bank
  | gameOver bank            = Guest
  | value guest > value bank = Guest
  | otherwise                = Bank

-- Number of Aces in a hand.
numberOfAces :: Hand -> Integer
numberOfAces Empty = 0
numberOfAces (Add card hand)
    | rank card == Ace = 1 + numberOfAces hand
    | otherwise        = numberOfAces hand

-- <+ OnTopOf operator, adds on hand to another
(<+) :: Hand -> Hand -> Hand
Empty <+ h2           = h2
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
    | otherwise           = bankHand
    where (deck', bankHand') = draw deck bankHand

-- Given a StdGen shuffles the hand
shuffle :: StdGen -> Hand -> Hand
shuffle gen hand = fst(shuffle' (Empty, hand) gen)

shuffle' :: (Hand, Hand) -> StdGen -> (Hand, Hand)
shuffle' (hand1, Empty) _ = (hand1, Empty)
shuffle' (hand1, hand2) g = shuffle' ((Add drawnCard hand1), restHand) nextGen
  where restHand  = fst(drawTuple)
        drawnCard = snd(drawTuple)
        drawTuple = drawCard hand2 index
        index     = fst(randTuple)
        nextGen   = snd(randTuple)
        randTuple = randomR (0, size hand2 - 1) g

prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards g c h =
  c `belongsTo` h == c `belongsTo` shuffle g h

-- Checks wether a card belongs to a hand
belongsTo :: Card -> Hand -> Bool
c `belongsTo` Empty      = False
c `belongsTo` (Add c' h) = c == c' || c `belongsTo` h

-- Draws the card of index I from the provided hand
drawCard :: Hand -> Int -> (Hand, Card)
drawCard Empty _  = error "drawCard: The deck is empty"
drawCard hand index
  | size hand <= index || index < 0 = error "drawCard: Index out of bounds"
  | otherwise                       = drawCard' (hand, Empty) index

drawCard' :: (Hand, Hand) -> Int -> (Hand, Card)
drawCard' ((Add card hand1), hand2) index
  | index == 0 = ((hand1 <+ hand2), card)
  | otherwise  = drawCard' (hand1, Add card hand2) (index - 1)

-- Implementation interface
implementation :: Interface 
implementation = Interface { 
    iEmpty     = empty
  , iFullDeck  = fullDeck
  , iValue     = value
  , iGameOver  = gameOver
  , iWinner    = winner
  , iDraw      = draw
  , iPlayBank  = playBank
  , iShuffle   = shuffle
}

main :: IO ()
main = runGame implementation
