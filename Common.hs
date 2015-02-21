module Common where

-- Lower precedence reverse application operator
(|>) :: a -> (a -> b) -> b
(|>) x f = f x
infixl 0 |>

-- Higher precidence right associative application operator
($|) :: (a -> b) -> a -> b
($|) f x = f x
infixr 1 $|

not_implemented s = error $ "not implemented: " ++ s
unreachable s = error $ "unreachable: " ++ s

joinWithSep :: [a] -> [[a]] -> [a] 
joinWithSep sep [] = []
joinWithSep sep (x:xs) = x ++ sep ++ (joinWithSep sep xs)

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn with l = 
    let fsplit (x:xs) i = 
            if i == with then (reverse x : xs) 
            else ((i : x) : xs)
    in foldl fsplit [[]] l |> reverse
