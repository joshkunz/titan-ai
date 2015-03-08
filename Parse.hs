module Parse where
import qualified Data.Maybe as Maybe
import qualified Game as Game
import qualified Graph as Graph
import Common ((|>), splitOn, not_implemented, unreachable)

-- Group items of the given list into non-overlapping pairs
group2 :: [a] -> [(a, a)]
group2 [] = []
group2 (i1 : (i2 : xs)) = (i1, i2) : group2 xs

foldlWith2 :: (a -> b -> b -> a) -> a -> [b] -> a
foldlWith2 f acc l = group2 l |> foldl (\acc (a, b) -> f acc a b) acc

-- Given an even length list of items and a functions that maps two items
-- to another item, run f on each pair
mapWith2 :: (a -> a -> b) -> [a] -> [b]
mapWith2 f l = group2 l |> map (\(a, b) -> f a b) 

-- Rotate the arguments of functions of three arguments. Somewhat
-- equivalent to `flip`, but for 3 argument functions
rargs3 :: (a -> b -> c -> d) -> c -> a -> b -> d
rargs3 f a3 a1 a2 = f a1 a2 a3

superRegionForStrings :: String -> String -> Game.SuperRegion
superRegionForStrings id bounty = Game.SuperRegion (read id :: Integer) 
                                                   (read bounty :: Integer)

regionForStringId :: String -> Game.SuperRegion -> Game.Region
regionForStringId id sr = Game.Region (read id :: Integer) sr

parseSuperRegion :: String -> String -> Game.Game -> Game.Game
parseSuperRegion id bounty g = 
    Game.SuperRegion (read id :: Integer) (read bounty :: Integer)
        |> (flip Game.insertSuperRegion) (Game.map g)
        |> \gm -> g { Game.map = gm }

parseRegion :: String -> String -> Game.Game -> Game.Game
parseRegion id sid g =
    let gm = Game.map g
        r = case Game.getSuperRegionById (read sid :: Integer) gm of
                Just sr -> Game.Region (read id :: Integer) sr
                Nothing -> error $ "No super region with id \"" ++ sid ++ "\""
    in Game.insertRegion r gm |> \gm -> g { Game.map = gm }

getRegionFromString :: String -> Game.Game -> Game.Region
getRegionFromString id (Game.Game _ gm) = 
    case Game.getRegionById (read id :: Integer) gm of
        Just r -> r
        Nothing -> error $ "Region \"" ++ id ++ "\" not found"

parseNeighbors :: String -> String -> Game.Game -> Game.Game
parseNeighbors rid ns g =
    let gm = Game.map g
        r = getRegionFromString rid g
    in splitOn ',' ns |> map ((flip getRegionFromString) g)
                      |> foldl (\g n -> Graph.insertEdge r n g) (Game.graph gm)
                      |> \gr -> gm { Game.graph = gr } 
                      |> \gm -> g { Game.map = gm }

parseWastelands :: String -> Game.Game -> Game.Game
parseWastelands rid g =
    let gm = Game.map g
        r = getRegionFromString rid g
    in (Game.RegionState Game.Neutral Game.wasteland_units)
          |> \s -> Game.setRegionState r s gm
          |> (flip Game.setMap) g

parseSetupMap :: [String] -> Game.Game -> Game.Game
parseSetupMap ws g =
    case opt of
        "super_regions" -> foldlWith2 (rargs3 parseSuperRegion) g rest
        "regions" ->  foldlWith2 (rargs3 parseRegion) g rest
        "neighbors" -> foldlWith2 (rargs3 parseNeighbors) g rest
        "wastelands" -> foldl (flip parseWastelands) g rest
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
