module Common where
import qualified Data.List as List

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

checkListProperty :: (b -> b -> Bool) -> [b] -> Bool
checkListProperty f [] = True
checkListProperty f [i] = True
checkListProperty f (first : second : rest) =
    (f first second) && (checkListProperty f rest)

joinWithSep :: [a] -> [[a]] -> [a] 
joinWithSep sep [] = []
joinWithSep sep (x:xs) = x ++ sep ++ (joinWithSep sep xs)

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn with l = 
    let fsplit [[]] i = if i == with then [[]] else [[i]]
        fsplit (x : xs) i = if i == with then [] : ((reverse x) : xs)
                            else (i : x) : xs
        head : rest = foldl fsplit [[]] l
    in reverse ((reverse head) : rest)

-- Group items of the given list into non-overlapping pairs
group2 :: [a] -> [(a, a)]
group2 [] = []
group2 (i1 : (i2 : xs)) = (i1, i2) : group2 xs

group3 :: [a] -> [(a, a, a)]
group3 [] = []
group3 (i1 : (i2 : (i3 : xs))) = (i1, i2, i3) : group3 xs

-- Fold a function of a list using non-overlapping pairs from that list
foldlWith2 :: (a -> b -> b -> a) -> a -> [b] -> a
foldlWith2 f acc l = group2 l |> foldl (\acc (a, b) -> f acc a b) acc

-- Given an even length list of items and a functions that maps two items
-- to another item, run f on each pair
mapWith2 :: (a -> a -> b) -> [a] -> [b]
mapWith2 f l = group2 l |> map (\(a, b) -> f a b) 

mapWith3 :: (a -> a -> a -> b) -> [a] -> [b]
mapWith3 f l = group3 l |> map (\(a, b, c) -> f a b c)

-- Rotate the arguments of functions of three arguments. Somewhat
-- equivalent to `flip`, but for 3 argument functions
rargs3 :: (a -> b -> c -> d) -> c -> a -> b -> d
rargs3 f a3 a1 a2 = f a1 a2 a3

rargs4 :: (a -> b -> c -> d -> e) -> d -> a -> b -> c -> e
rargs4 f a4 a1 a2 a3 = f a1 a2 a3 a4

sortOn :: (Ord a) => (b -> a) -> [b] -> [b]
sortOn f = List.sortBy (\a b -> (compare (f a) (f b)))
