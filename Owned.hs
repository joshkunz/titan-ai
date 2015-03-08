module Owned where

data Owner = Us | Opponent | Neutral deriving (Eq, Ord, Show)

class Owned a where
    owner :: a -> Owner
