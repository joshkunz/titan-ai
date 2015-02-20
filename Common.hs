module Common where

to_sexp :: [String] -> String
to_sexp l = "(" ++ (unwords l) ++ ")"

(|>) :: a -> (a -> b) -> b
(|>) x f = f x

infixl 1 |>
