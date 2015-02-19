module Graph (Graph, empty, insertEdge, neighbors, adjacent, connected) where 
import qualified Data.Map as Map
import qualified Data.Set as Set
import Common (to_sexp, (|>))

-- Mapping of a region to a list of its neighbors
data Graph a = Graph (Map.Map a (Set.Set a))

empty :: Graph a
empty = Graph Map.empty

-- Performs one half of the symmetric insertion
insertEdge_ from to adj = 
    case Map.lookup from adj of
        Just ns -> Map.insert from (Set.insert to ns) adj
        Nothing -> Map.insert from (Set.fromList [to]) adj

-- Symetrically insert an edge between "from" and "to"
insertEdge :: (Ord a) => a -> a -> Graph a -> Graph a
insertEdge from to (Graph adj) =
    insertEdge_ from to adj |> insertEdge_ to from |> Graph

fromList :: (Ord a) => [(a, a)] -> Graph a
fromList = foldl (\g (a, b) -> insertEdge a b g) empty

edge_set :: (Ord a) => Graph a -> Set.Set (a, a)
edge_set (Graph adj) = 
    let neighbor_set e b = Set.map (\x -> (e, x)) b
        neighbor_fold a k v = Set.union a $ neighbor_set k v 
    in
        Map.foldlWithKey neighbor_fold Set.empty adj

edges :: (Ord a) => Graph a -> [(a, a)]
edges g = Set.toList $ edge_set g

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

edge_string :: (Show a) => (a, a) -> String
edge_string (a, b) = to_sexp ["Edge", (show a), (show b)]

instance (Ord a, Show a) => Show (Graph a) where
    show g = unlines $ map edge_string $ edges g
