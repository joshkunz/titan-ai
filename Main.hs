module Main where
import qualified Game as Game
import Game (Game(..), SuperRegion(..), Region(..), Placement(..), Move(..))
import qualified Graph as Graph
import qualified Data.Set as Set
import qualified Parse as Parse
import qualified Engine as Engine
import System.IO
import Engine (Engine(..))
import Common ((|>))

maxSRBounty :: Set.Set Game.Region -> Game.Game -> Integer
maxSRBounty rs g =
    let next_max v (Game.Region _ (SuperRegion _ b)) = if b > v then b else v
    in Set.foldl next_max 0 rs

highestRegions :: Set.Set Region -> Game.Game -> Set.Set Region
highestRegions rs g =
    Set.filter (\(Region _ (SuperRegion _ b)) -> b >= maxBounty) rs
    where maxBounty = maxSRBounty rs g

-- General strategy: 
--   First, pick starting territories in a high-value super region
--   Second, pick close to other territories we own
--   Second, pick away from wastelands
--   Third, pick close to enemy territory
startPicker :: Integer -> Set.Set Region -> Game.Game -> Region
startPicker i rs g =
    highestRegions rs g |> Set.elems |> head

armyPlacer :: Integer -> Game.Game -> [Placement]
armyPlacer i g = []

mover :: Integer -> Game.Game -> [Move]
mover i g = []

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

runner :: IO Engine -> String -> IO Engine
runner e s =
    e >>= \e -> case Engine.nextLine s e of 
                    (Just s, ne) -> putStr s >> return ne
                    (Nothing, ne) -> return ne

runLines :: [String] -> Engine -> IO Engine
runLines ls e = foldl runner (return e) ls

runStdin :: Engine -> IO Game 
runStdin e = 
    hGetLine stdin >>= \s -> runner (return e) s 
                   >>= \e -> 
                        hIsEOF stdin 
                            >>= \eof -> if eof then return $ Engine.game e 
                                               else runStdin e

main = (Engine.fromFuncs startPicker armyPlacer Main.mover
         |> runStdin) >>= \g -> (show g |> hPutStrLn stderr)
