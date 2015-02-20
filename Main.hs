module Main where
import qualified Game as Game
import qualified Graph as Graph
import qualified Parse as Parse
import Common ((|>))

main = 
    "setup_map super_regions 1 10 2 20 3 30" 
        |> (flip Parse.parseStartupLine) Game.empty
        |> show |> putStrLn
