module Common where

not_implemented = error "Not Implemented"
unreachable = error "unreachable"

joinWithSep :: [a] -> [[a]] -> [a] 
joinWithSep sep [] = []
joinWithSep sep (x:xs) = x ++ sep ++ (joinWithSep sep xs)

(|>) :: a -> (a -> b) -> b
(|>) x f = f x

infixl 1 |>
