module Main where
import qualified Data.Map as Map
import qualified Graph as Graph
import Common (to_sexp,(|>))

data SuperRegion = SuperRegion { srid :: Int
                               , bounty :: Int }

data Region = Region { rid :: Int
                     , super_region :: SuperRegion }

type RegionGraph = Graph.Graph Region

data GameMap = GameMap { super_regions :: [SuperRegion]
                       , regions :: [Region]
                       , graph :: RegionGraph }

type GameSettings = Map.Map String String
data Game = Game { settings :: GameSettings
                 , map :: GameMap }

instance Show SuperRegion where
    show (SuperRegion s b) = to_sexp ["SuperRegion", (show s), (show b)]

instance Show Region where
    show (Region r sr) = to_sexp ["Region", (show r), (show sr)]

instance Eq Region where
    (==) (Region ra _) (Region rb _) = ra == rb

instance Ord Region where
    compare (Region ra _) (Region rb _) = compare ra rb

main = Graph.empty |> \x -> Graph.insertEdge x 1 2 
                   |> \x -> Graph.insertEdge x 2 3
                   |> \x -> Graph.insertEdge x 3 4
                   |> \x -> Graph.connected x 4 0
                   |> show |> putStrLn
