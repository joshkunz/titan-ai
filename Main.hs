module Main where
import qualified Game as Game
import Game (Game(..))
import qualified Engine as Engine
import qualified AI as AI
import System.IO
import Engine (Engine(..))
import Common ((|>))

exampleGame = [ "settings timebank 10000"
              , "settings time_per_move 500"
              , "settings max_rounds 50"
              , "settings your_bot player1"
              , "settings opponent_bot player2"
              , "setup_map super_regions 1 2 2 5"
              , "setup_map regions 1 1 2 1 3 2 4 2 5 2"
              , "setup_map neighbors 1 2,3,4 2 3 4 5"
              , "setup_map wastelands 3"
              , "settings starting_regions 2 4"
              , "settings starting_pick_amount 1"
              , "pick_starting_region 10000 2 4"
              , "settings starting_armies 7"
              , "update_map 1 player1 2 2 player1 4 3 neutral 10 4 player2 5"
              , "go place_armies 10000"
              , "go attack/transfer 10000"
              , "update_map 1 neutral 2 4 player2 5 5 neutral 2"
              , "opponent_moves player2 place_armies 1 10, player2 attack/transfer 1 2 5"
              ]

log_output = False
log_input = False

noop = return () :: IO ()

doErr = hPutStr stderr

doLog :: Bool -> String -> String -> IO String
doLog pred msg s = 
    (if pred then 
        doErr (msg ++ " " ++ (show s) ++ "\n") 
     else return ()) >> return s

doLogOutput = doLog log_output "[Log/Output]"
doLogInput = doLog log_input "[Log/Input]"

doOut s = doLogOutput s >> hPutStr stdout s >> hFlush stdout

runner :: IO Engine -> String -> IO Engine
runner e s =
    e >>= \e -> return (Engine.nextLine s e)
      >>= (\(out, err, ne) -> (maybe noop doErr err)
                           >> (maybe noop doOut out)
                           >> return ne )

runLines :: [String] -> Engine -> IO Engine
runLines ls e = foldl runner (return e) ls

runStdin :: Engine -> IO Game 
runStdin e = 
    hGetLine stdin >>= doLogInput
                   >>= \s -> runner (return e) s 
--                   >>= \e -> (hPutStrLn stdout (Engine.game e |> show) >> return e)
                   >>= \e -> 
                        hIsEOF stdin 
                            >>= \eof -> if eof then return $ Engine.game e 
                                               else runStdin e

--main = (Engine.fromFuncs startPicker armyPlacer Main.mover
--         |> runStdin) >>= \g -> (show g |> hPutStrLn stderr)
main = Engine.fromFuncs AI.startPicker AI.armyPlacer AI.mover |> runStdin
