module Parse where
import qualified Game as Game
import Common ((|>), not_implemented, unreachable)

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
        "regions" -> 
            let strR id sid gm = 
                 case Game.getSuperRegionById (read sid :: Int) gm of
                    Just sr -> (Game.Region (read id :: Int) sr)
                    Nothing -> error $ "No super region with id \"" ++ sid ++ "\""
            in mapWith2 (\id sid -> strR id sid (Game.map g)) rest
                    |> foldl (flip Game.insertRegion) (Game.map g)
                    |> \gm -> g { Game.map = gm }
        "neighbors" -> not_implemented opt
        "wastelands" -> not_implemented opt
        "opponent_starting_regions" -> not_implemented opt
        x -> error $ "Unrecognized setup_map sub command \"" ++ x ++ "\""
    where opt : rest = ws

parseSettings :: [String] -> Game.Game -> Game.Game
parseSettings s g = 
    case s of 
        opt : vs -> Game.setStringSetting opt (unwords vs) g 
                        |> (flip Game.setSettings) g
        _ -> unreachable "parseSettings"

parseGo :: [String] -> Game.Game -> Game.Game
parseGo = not_implemented "go"

parseStartupLine :: String -> Game.Game -> Game.Game
parseStartupLine l g =
    case opt of 
        "setup_map" -> parseSetupMap rest g
        "settings" -> parseSettings rest g
        "go" -> parseGo rest g
        x -> "Unrecognized startup command \"" ++ x ++ "\"" |> error
    where opt : rest = words l
