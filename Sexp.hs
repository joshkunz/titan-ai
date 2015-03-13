module Sexp where
import qualified Data.Set as Set 
import qualified Text.PrettyPrint as PP
import Text.PrettyPrint ((<>))
import Common ((|>))

class Sexp a where
    sexp :: a -> PP.Doc

fromList :: [PP.Doc] -> PP.Doc
fromList l = PP.hsep l |> PP.parens

fromString :: String -> PP.Doc
fromString = PP.text

namedList :: String -> [PP.Doc] -> PP.Doc
namedList n l = (PP.text n) : l |> fromList

fromPair :: (PP.Doc, PP.Doc) -> PP.Doc 
fromPair (a, b) = fromList [a, b]

fromSet :: Set.Set PP.Doc -> PP.Doc 
fromSet s = Set.elems s |> fromList

namedSet :: String -> Set.Set PP.Doc -> PP.Doc
namedSet n s = Set.elems s |> namedList n

c_namedSet :: (Sexp a) => String -> Set.Set a -> PP.Doc
c_namedSet n s = Set.elems s |> map sexp |> namedList n

fromStringList :: [String] -> PP.Doc 
fromStringList l = map PP.text l |> fromList

namedStringList :: String -> [String] -> PP.Doc
namedStringList n l = n : l |> fromStringList

fromStringPair :: (String, String) -> PP.Doc
fromStringPair (a, b) = fromStringList [a, b] 

fromStringSet :: Set.Set String -> PP.Doc
fromStringSet s = Set.elems s |> fromStringList

render :: PP.Doc -> String
render = PP.renderStyle (PP.Style PP.PageMode 80 1.5)

-- Standard Instances

instance (Sexp a) => Sexp [a] where
    sexp l = fromList $ map sexp l

instance (Sexp a, Sexp b) => Sexp (a, b) where
    sexp (a, b) = fromList [sexp a, sexp b]
