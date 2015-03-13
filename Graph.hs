module Graph where 
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Sexp as Sexp
import Sexp (Sexp, sexp)
import Common ((|>), ($|))

-- Mapping of a region to a list of its neighbors
data Graph a = Graph (Map.Map a (Set.Set a))

-- External representation of a graph edge
data Edge a = Edge a a

-- External Representation of a path
type Path a = [Edge a]

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


sortableEdge :: (Ord a) => Edge a -> Edge a
sortableEdge (Edge a b) = Edge (min a b) (max a b)

instance (Ord a) => Eq (Edge a) where
    (==) a b = (a1, b1) == (a2, b2)
               where (Edge a1 b1) = sortableEdge a
                     (Edge a2 b2) = sortableEdge b

instance (Ord a) => Ord (Edge a) where
    compare a b = compare (a1, b1) (a2, b2)
                  where (Edge a1 b1) = sortableEdge a
                        (Edge a2 b2) = sortableEdge b

edgeSet :: (Ord a) => Graph a -> Set.Set (Edge a)
edgeSet (Graph adj) = 
    let neighbor_set e b = Set.map (\x -> Edge e x) b
        neighbor_join a k v = neighbor_set k v |> Set.union a
    in Map.foldlWithKey neighbor_join Set.empty adj

edges :: (Ord a) => Graph a -> [(Edge a)]
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


-- Yields the closest vertex to the start vertex for which the given
-- function yields true.
closest :: (Ord a) => a -> (a -> Bool) -> Graph a -> Maybe (a, Path a)
closest start f g =
    let traverse found [] = Nothing
        traverse found ((h, path) : tail) =
            if f h then Just (h, reverse path)
            else traverse nextFound nextTail
            where nextFound = (Set.insert h found)
                  extendPath n = (n, (Edge h n) : path)
                  extendTail ns = Set.filter ((flip Set.notMember) found) ns 
                                    |> Set.elems |> map extendPath |> (tail ++)
                  nextTail = maybe tail extendTail $ neighbors h g
    in traverse Set.empty [(start, [])]

instance (Ord a, Sexp a) => Sexp (Edge a) where
    sexp (Edge a b) = Sexp.namedList "Edge" [(sexp a), (sexp b)]

instance (Ord a, Sexp a) => Sexp (Graph a) where
    sexp g = Sexp.namedList "Graph" $| map sexp $| edges g
