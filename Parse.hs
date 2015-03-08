module Parse where
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Owned as Owned
import qualified Sexp as Sexp
import Sexp (sexp)
import qualified Game as Game
import qualified Graph as Graph
import Common ((|>), splitOn, not_implemented, unreachable, 
               mapWith2, mapWith3, rargs3, rargs4)

type NeighborPair = (Game.Region, Set.Set Game.Region)
type MapUpdate = (Game.Region, Owned.Owner, Integer)
type OpponentMove = Either Game.Placement Game.Move

data Command = Setting String String
             | SuperRegions (Set.Set Game.SuperRegion)
             | Regions (Set.Set Game.Region)
             | Neighbors (Set.Set NeighborPair)
             | Wastelands (Set.Set Game.Region)
             | OpponentStartingRegions (Set.Set Game.Region)
             | PickStartingRegion Integer (Set.Set Game.Region)
             | UpdateMap (Set.Set Game.Placement)
             | OpponentMoves (Set.Set OpponentMove)
             | PlaceArmies Integer
             | AttackOrTransfer Integer

sexp_for_neighbor_pair (r, rs) = 
    ( sexp r
    , Set.elems rs |> map sexp |> Sexp.fromList )

instance (Sexp.Sexp a, Sexp.Sexp b) => Sexp.Sexp (Either a b) where
    sexp (Left a) = sexp a
    sexp (Right a) = sexp a

instance Sexp.Sexp Command where
    sexp (Setting s v) = Sexp.namedStringList "Setting" [s, v]
    sexp (SuperRegions srs) = Sexp.c_namedSet "SuperRegions" srs
    sexp (Regions rs) = Sexp.c_namedSet "Regions" rs
    sexp (Neighbors ns) = Set.elems ns |> map sexp_for_neighbor_pair
                                       |> map Sexp.fromPair
                                       |> Sexp.namedList "Neighbors"
    sexp (Wastelands rs) = Sexp.c_namedSet "Wastelands" rs
    sexp (OpponentStartingRegions rs) = Sexp.c_namedSet "OpponentStartingRegions" rs
    sexp (PickStartingRegion i rs) = ((Sexp.fromString (show i)) : (Set.elems rs |> map sexp))
                                        |> Sexp.namedList "PickStartingRegion"
    sexp (UpdateMap ps) = Sexp.c_namedSet "UpdateMap" ps
    sexp (OpponentMoves ms) = Sexp.c_namedSet "OpponentMoves" ms
    sexp (PlaceArmies i) = Sexp.namedStringList "PlaceArmies" [(show i)]
    sexp (AttackOrTransfer i) = Sexp.namedStringList "AttackOrTransfer" [(show i)]

parseSuperRegion :: String -> String -> Game.SuperRegion 
parseSuperRegion id bounty = 
    Game.SuperRegion (read id :: Integer) (read bounty :: Integer)

parseRegion :: String -> String -> Game.Game -> Game.Region
parseRegion id sid g =
    case Game.getSuperRegionById (read sid :: Integer) (Game.map g) of
        Just sr -> Game.Region (read id :: Integer) sr
        Nothing -> error $ "No super region with id \"" ++ sid ++ "\""

parseExistingRegion :: String -> Game.Game -> Game.Region
parseExistingRegion id (Game.Game _ gm) = 
    case Game.getRegionById (read id :: Integer) gm of
        Just r -> r
        Nothing -> error $ "Region \"" ++ id ++ "\" not found"

parseNeighbor :: String -> String -> Game.Game -> NeighborPair
parseNeighbor rid ns g =
    ( parseExistingRegion rid g
    , splitOn ',' ns |> map ((flip parseExistingRegion) g) 
                     |> Set.fromList )

parseSetupMap :: [String] -> Game.Game -> Command 
parseSetupMap ws g =
    case opt of
        "super_regions" -> mapWith2 parseSuperRegion rest
                            |> Set.fromList |> SuperRegions
        "regions" ->  mapWith2 ((rargs3 parseRegion) g) rest
                            |> Set.fromList |> Regions
        "neighbors" -> mapWith2 ((rargs3 parseNeighbor) g) rest
                            |> Set.fromList |> Neighbors
        "wastelands" -> map ((flip parseExistingRegion) g) rest
                            |> Set.fromList |> Wastelands
        "opponent_starting_regions" -> 
            map ((flip parseExistingRegion) g) rest
                |> Set.fromList |> OpponentStartingRegions
        x -> error $ "Unrecognized setup_map sub command \"" ++ x ++ "\""
    where opt : rest = ws

parseSettings :: [String] -> Game.Game -> Command 
parseSettings s g = 
    case s of
        opt : vs -> Setting opt (unwords vs)
        _ -> unreachable "parseSettings"

ownerForPlayer :: String -> Game.Game -> Owned.Owner
ownerForPlayer p g
    | p == us = Owned.Us
    | p == them = Owned.Opponent
    | p == "neutral" = Owned.Neutral
    | otherwise = unreachable ("playerForOwner " ++ p)
    where gs = Game.settings g
          us = case (Game.your_bot gs) of
                    Just s -> s
                    Nothing -> unreachable "ownerForPlayer: No your_bot"
          them = case (Game.opponent_bot gs) of
                    Just s -> s
                    Nothing -> unreachable "ownerForPlayer: you opponent_bot"

parseUpdate :: String -> String -> String -> Game.Game -> Game.Placement 
parseUpdate rid who count g =
    Game.Placement owner region (read count :: Integer)
    where region = parseExistingRegion rid g
          owner = ownerForPlayer who g

parseUpdateMap :: [String] -> Game.Game -> Command
parseUpdateMap ps g = 
    mapWith3 ((rargs4 parseUpdate) g) ps 
        |> Set.fromList |> UpdateMap

parseOpponentPlacement :: Owned.Owner -> String -> String -> Game.Game -> Game.Placement
parseOpponentPlacement o id units g =
    Game.Placement o r (read units :: Integer)
    where r = parseExistingRegion id g

parseOpponentMove :: Owned.Owner -> String -> String -> String -> Game.Game -> Game.Move
parseOpponentMove o sid tid units g =
    Game.Move o sr tr (read units :: Integer)
    where sr = parseExistingRegion sid g
          tr = parseExistingRegion tid g

parseMoveList :: [String] -> Game.Game -> [OpponentMove]
parseMoveList [] g = []
parseMoveList ms g = 
    case extra of
        "place_armies" : id : count : rest ->
            parseOpponentPlacement o id count g 
                |> Left |> \x -> x : parseMoveList rest g
        "attack/transfer" : sid : tid : count : rest ->
            parseOpponentMove o sid tid count g
                |> Right |> \x -> x : parseMoveList rest g
        _ -> unreachable (Sexp.namedStringList "parseMoveList" ms |> Sexp.render)
    where name : extra = ms
          o = (ownerForPlayer name g)

parseOpponentMoves :: [String] -> Game.Game -> Command
parseOpponentMoves ms g = parseMoveList ms g |> Set.fromList |> OpponentMoves

parseGo :: [String] -> Game.Game -> Command 
parseGo cs g =
    case cs of 
        ["place_armies", i] -> PlaceArmies (read i :: Integer)
        ["attack/transfer", i] -> AttackOrTransfer (read i :: Integer)
        _ -> unreachable ("Bad go: " ++ (unwords cs))

parseLine :: String -> Game.Game -> Command
parseLine l g =
    case opt of 
        "setup_map" -> parseSetupMap rest g
        "settings" -> parseSettings rest g
        "pick_starting_region" -> 
            map ((flip parseExistingRegion) g) regions
                |> Set.fromList 
                |> PickStartingRegion (read time :: Integer)
            where time : regions = rest
        "update_map" -> parseUpdateMap rest g
        "opponent_moves" -> parseOpponentMoves rest g
        "go" -> parseGo rest g
        x -> "Unrecognized startup command \"" ++ x ++ "\"" |> error
    where opt : rest = words l
