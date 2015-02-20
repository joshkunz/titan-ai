module Parse where
import qualified Game as Game
import Common ((|>), not_implemented)

-- Group items of the given list into non-overlapping pairs
group2 :: [a] -> [(a, a)]
group2 [] = []
group2 (i1 : (i2 : xs)) = (i1, i2) : group2 xs

-- Given an even length list of items and a functions that maps two items
-- to another item, run f on each pair
mapWith2 :: (a -> a -> b) -> [a] -> [b]
mapWith2 f l = group2 l |> map (\(a, b) -> f a b) 

parseSetupMap :: [String] -> Game.Game -> Game.Game
parseSetupMap ws g =
    case opt of
        "super_regions" -> 
            let strSR id b = Game.SuperRegion (read id :: Int) (read b :: Int)
            in mapWith2 strSR rest 
                   |> foldl (flip Game.insertSuperRegion) (Game.map g)
                   |> \gm -> g { Game.map = gm }
        "regions" -> not_implemented
        "neighbors" -> not_implemented
        "wastelands" -> not_implemented 
        "opponent_starting_regions" -> not_implemented
        x -> "Unrecognized setup_map sub command \"" ++ x ++ "\"" |> error
    where opt : rest = ws

parseSettings :: [String] -> Game.Game -> Game.Game
parseSettings s g = not_implemented

parseGo :: [String] -> Game.Game -> Game.Game
parseGo = not_implemented

parseStartupLine :: String -> Game.Game -> Game.Game
parseStartupLine l g =
    case opt of 
        "setup_map" -> parseSetupMap rest g
        "settings" -> parseSettings rest g
        "go" -> parseGo rest g
        x -> "Unrecognized startup command \"" ++ x ++ "\"" |> error
    where opt : rest = words l
