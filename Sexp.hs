module Sexp where
import qualified Data.Set as Set 
import Common ((|>))

fromList :: [String] -> String
fromList l = "(" ++ (unwords l) ++ ")"

namedList :: String -> [String] -> String
namedList n l = n : l |> fromList 

fromPair :: (String, String) -> String
fromPair (a, b) = fromList [a, b]

fromSet :: Set.Set String -> String
fromSet s = Set.elems s |> fromList

fromShow :: (Show a) => a -> String
fromShow s = "(" ++ (show s) ++ ")"
