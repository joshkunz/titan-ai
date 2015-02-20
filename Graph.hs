module Graph (Graph, empty, insertEdge, removeEdge, 
                     neighbors, adjacent, connected) where 
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Sexp as Sexp
import Common ((|>))

-- Mapping of a region to a list of its neighbors
data Graph a = Graph (Map.Map a (Set.Set a))

empty :: Graph a
empty = Graph Map.empty

-- Symetrically insert an edge between "from" and "to"
insertEdge :: (Ord a) => a -> a -> Graph a -> Graph a
insertEdge from to (Graph adj) =
    let insertDir from to adj =
            case Map.lookup from adj of
                Just ns -> Map.insert from (Set.insert to ns) adj
                Nothing -> Map.insert from (Set.fromList [to]) adj
    in
        insertDir from to adj |> insertDir to from |> Graph

removeEdge :: (Ord a) => a -> a -> Graph a -> Graph a
removeEdge from to (Graph adj) = 
    let removeDir from to adj =
            case Map.lookup from adj of
                Just ns -> if Set.member to ns then 
                               Map.insert from (Set.delete to ns) adj
                           else error "Cannot remove an edge that doesn't exist"
                Nothing -> error "Cannot remove an edge that doesn't exist"
     in removeDir from to adj |> removeDir to from |> Graph

fromList :: (Ord a) => [(a, a)] -> Graph a
fromList = foldl (\g (a, b) -> insertEdge a b g) empty

edgeSet :: (Ord a) => Graph a -> Set.Set (a, a)
edgeSet (Graph adj) = 
    let neighbor_set e b = Set.map (\x -> (e, x)) b
        neighbor_fold a k v = Set.union a $ neighbor_set k v 
    in
        Map.foldlWithKey neighbor_fold Set.empty adj

edges :: (Ord a) => Graph a -> [(a, a)]
edges g = Set.toList $ edgeSet g

neighbors :: (Ord a) => a -> Graph a -> Maybe (Set.Set a)
neighbors v (Graph adj) = Map.lookup v adj

adjacent :: (Ord a) => a -> a -> Graph a -> Bool 
adjacent a b g =
    case neighbors a g of
        Just ns -> Set.member b ns
        Nothing -> False 

connected :: (Ord a) => a -> a -> Graph a -> Bool
connected what to g =
    let connected_ what to seen = 
            if what == to then True else
            if Set.member what seen then False else
            let nseen = Set.insert what seen in
            case neighbors what g of
                Just ns -> any (\x -> connected_ x to nseen) 
                               (Set.toList ns)
                Nothing -> False
    in connected_ what to Set.empty 

edgeString :: (Show a) => (a, a) -> String
edgeString (a, b) = Sexp.fromList ["Edge", (show a), (show b)]

instance (Ord a, Show a) => Show (Graph a) where
    show g = "(Graph " ++ (unwords $ map edgeString $ edges g) ++ ")"
