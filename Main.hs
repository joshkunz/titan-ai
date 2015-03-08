module Main where
import qualified Game as Game
import qualified Graph as Graph
import qualified Parse as Parse
import Common ((|>))

startupLines = [ "settings timebank 10000"
               , "settings time_per_move 500"
               , "settings max_rounds 50"
               , "settings your_bot player1"
               , "settings opponent_bot player2"
               , "setup_map super_regions 1 2 2 5"
               , "setup_map regions 1 1 2 1 3 2 4 2 5 2"
               , "setup_map neighbors 1 2,3,4 2 3 4 5"
               , "setup_map wastelands 3"
               , "settings starting_regions 2 4"
               ]

parseLines :: [String] -> Game.Game -> Game.Game
parseLines ls g = foldl (flip Parse.parseStartupLine) g ls

main = Game.empty
        |> parseLines startupLines
        |> show |> putStrLn
