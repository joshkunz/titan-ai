module AI.Always where
import qualified Graph
import Graph (Graph(..))
import Data.Set (Set)
import qualified Game
import Game (Region, GameMap)

neighbors :: (Ord a, Show a) => a -> Graph a -> Set a
neighbors n g = 
    case Graph.neighbors n g of
        Just ns -> ns
        Nothing -> error $ "Region: " ++ (show n) ++ " has no neighbors"

units :: Region -> GameMap -> Integer
units r gm =
    case Game.regionUnits r gm of
        Just u -> u
        Nothing -> error $ ("No units in region " ++ (show r))
