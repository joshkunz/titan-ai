module Graph (Graph, empty, insertEdge, adjacent) where 
import qualified Data.Map as Map
import qualified Data.Set as Set
import Common (to_sexp)

-- Mapping of a region to a list of its neighbors
data Graph a = Graph (Map.Map a (Set.Set a))

empty :: Graph a
empty = Graph Map.empty

edge_set :: (Ord a) => Graph a -> Set.Set (a, a)
edge_set (Graph adj) = 
    let neighbor_set e b = Set.map (\x -> (e, x)) b
        neighbor_fold a k v = Set.union a $ neighbor_set k v 
    in
        Map.foldlWithKey neighbor_fold Set.empty adj

edges :: (Ord a) => Graph a -> [(a, a)]
edges g = Set.toList $ edge_set g

-- Symetrically insert an edge between "from" and "to"
insertEdge :: (Ord a) => Graph a -> a -> a -> Graph a
insertEdge (Graph adj) from to =
    Graph $ insertEdge_ (insertEdge_ adj from to) to from

-- Performs one half of the symmetric insertion
insertEdge_ adj from to = 
    if Map.member from adj then
        let (Just neighbors) = Map.lookup from adj in
            Map.insert from (Set.insert to neighbors) adj
    else
        Map.insert from (Set.fromList [to]) adj

adjacent :: (Ord a) => Graph a -> a -> a -> Bool 
adjacent (Graph adj) a b =
    case Map.lookup a adj of
        Just ns -> Set.member b ns
        Nothing -> False 

edge_string :: (Show a) => (a, a) -> String
edge_string (a, b) = to_sexp ["Edge", (show a), (show b)]

instance (Ord a, Show a) => Show (Graph a) where
    show g = unlines $ map edge_string $ edges g
