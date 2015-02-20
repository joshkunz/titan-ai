module Main where
import qualified Game as Game
import qualified Graph as Graph
import Common (to_sexp,(|>))

main = 
    let sr_a = (Game.SuperRegion 1 10)
        r_foo = (Game.Region 0 sr_a)
        r_baz = (Game.Region 1 sr_a)
        r_qux = (Game.Region 2 sr_a)
        g = Graph.empty |> Graph.insertEdge r_foo r_baz
                        |> Graph.insertEdge r_baz r_qux
    in do
        "  baz -- foo? " ++ (Graph.adjacent r_baz r_foo g |> show) |> putStrLn
        "  qux -- foo? " ++ (Graph.connected r_qux r_foo g |> show ) |> putStrLn
        "A baz -- foo? " ++ (Graph.removeEdge r_baz r_foo g
                                |> Graph.connected r_baz r_foo
                                |> show) |> putStrLn
