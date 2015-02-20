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

main = 
    let sr_a = (SuperRegion 1 10)
        r_foo = (Region 0 sr_a)
        r_baz = (Region 1 sr_a)
        r_qux = (Region 2 sr_a)
        g = Graph.empty |> Graph.insertEdge r_foo r_baz
                        |> Graph.insertEdge r_baz r_qux
    in do
        "  baz -- foo? " ++ (Graph.adjacent r_baz r_foo g |> show) |> putStrLn
        "  qux -- foo? " ++ (Graph.connected r_qux r_foo g |> show ) |> putStrLn
        "A baz -- foo? " ++ (Graph.removeEdge r_baz r_foo g
                                |> Graph.connected r_baz r_foo
                                |> show) |> putStrLn
