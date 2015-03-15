module AI.Common where
import qualified Data.List as List

onlyJust :: [Maybe a] -> [a]
onlyJust [] = []
onlyJust ((Just x) : xs) = x : (onlyJust xs)
onlyJust (Nothing : xs) = onlyJust xs

-- Note '%' is *not* modulus
divApprox :: (Real a, Fractional b) => a -> a -> b
divApprox a b = (realToFrac a) / (realToFrac b)

average :: (Real n, Fractional b) => [n] -> b
average ns = (realToFrac (sum ns)) / (List.genericLength ns)

enumerate :: [a] -> [(Integer, a)]
enumerate as = zip [0..(toInteger (length as))] as

