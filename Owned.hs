module Owned where

data Owner = Us | Opponent | Neutral deriving (Eq, Ord, Show)

other :: Owner -> Owner
other Us = Opponent
other Opponent = Us
other Neutral = error "Neutral doesn't have an 'other'"

class Owned a where
    owner :: a -> Owner
