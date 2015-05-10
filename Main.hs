module Main where
import qualified Game as Game
import Game (Game(..))
import qualified Engine as Engine
import qualified AI as AI
import System.IO
import Engine (Engine(..))
import Common ((|>))

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
