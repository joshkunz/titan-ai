module Common where

not_implemented s = error $ "not implemented: " ++ s
unreachable s = error $ "unreachable: " ++ s

joinWithSep :: [a] -> [[a]] -> [a] 
joinWithSep sep [] = []
joinWithSep sep (x:xs) = x ++ sep ++ (joinWithSep sep xs)

(|>) :: a -> (a -> b) -> b
(|>) x f = f x

infixl 1 |>
