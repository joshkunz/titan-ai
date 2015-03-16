module Engine where
import qualified Game as Game
import Game (RegionState(RegionState), Placement(..)) 
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Parse as Parse
import qualified Owned as Owned
import Owned (Owned, Owner(..), owner)
import Parse (Command(..))
import Common ((|>), not_implemented, checkListProperty)

data Result a = Result a (Maybe String)

type StartPicker = 
    Integer -> Set.Set Game.Region -> Game.Game -> Result Game.Region

type ArmyPlacer = 
    Integer -> Game.Game -> Result [Game.Placement]

type Mover = 
    Integer -> Game.Game -> Result [Game.Move]

data Engine = Engine { start_picker :: StartPicker
                     , army_placer :: ArmyPlacer
                     , mover :: Mover 
                     , game :: Game.Game }

pickString :: Game.Region -> String
pickString r = (Game.rid r |> show) ++ "\n"

noLog :: a -> Result a
noLog v = Result v Nothing

playerForOwner :: Owner -> Game.Game -> String
playerForOwner o g =
    case o of
        Us -> name (Game.your_bot st)
        Opponent -> name (Game.opponent_bot st)
        Neutral -> "neutral"
    where st = Game.settings g
          name n = case n of 
                    Just name -> name
                    Nothing -> error ("No name for owner " ++ (show o))

haveSameOwner :: (Owned a) => a -> a -> Bool
haveSameOwner ow1 ow2 = (owner ow1) == (owner ow2)

noMoves = "No moves\n"

placementString :: Game.Placement -> Game.Game -> String
placementString (Game.Placement o r i) g = 
    (playerForOwner o g) 
        ++ " place_armies " 
        ++ (show (Game.rid r)) 
        ++ " " ++ (show i) ++ ","

placementsString :: [Game.Placement] -> Game.Game -> String
placementsString [] g = noMoves
placementsString ps g =
    if not (checkListProperty haveSameOwner ps) then 
        error "Placements have differing owners"
    else
        (map ((flip placementString) g) ps |> unwords) ++ "\n"

moveString :: Game.Move -> Game.Game -> String
moveString (Game.Move o sr tr i) g =
    (playerForOwner o g) 
        ++ " attack/transfer "
        ++ (show (Game.rid sr)) 
        ++ " " ++ (show (Game.rid tr))
        ++ " " ++ (show i) ++ ","

movesString :: [Game.Move] -> Game.Game -> String
movesString [] g = noMoves
movesString ms g =
    if not (checkListProperty haveSameOwner ms) then
        error "Moves have different owners"
    else
        (map ((flip moveString) g) ms |> unwords) ++ "\n"

placementForMove :: Game.Move -> Game.Placement
placementForMove (Game.Move o sr tr i) = Game.Placement o tr i

empty :: Engine
empty = Engine { start_picker = \a b c -> not_implemented "start_picker"
               , army_placer = \a b -> not_implemented "army_placer"
               , mover = \a b -> not_implemented "mover"
               , game = Game.empty }

fromFuncs :: StartPicker -> ArmyPlacer -> Mover -> Engine
fromFuncs s a m = Engine { start_picker = s
                         , army_placer = a
                         , mover = m
                         , game = Game.empty }

-- The default wasteland unit count is 6
wasteland = Game.RegionState Neutral 6
starting_region = Game.RegionState Opponent 2
-- Default unit count is 2
unclaimed = Game.RegionState Neutral 2

nextLine :: String -> Engine -> (Maybe String, Maybe String, Engine)
nextLine l e =
    case (Parse.parseLine l g) of
        Setting s v -> g |> Game.setStringSetting s v 
                         |> (flip Game.setSettings) g
                         |> newE |> emptyR
        SuperRegions srs -> Set.elems srs |> foldl (flip Game.insertSuperRegion) gm
                                          |> newG |> emptyR
        Regions rs -> 
            let newRegion gm r = Game.insertRegion r gm 
                                    |> Game.setRegionState r unclaimed
            in Set.elems rs |> foldl newRegion gm |> newG |> emptyR
        Neighbors ns -> Set.elems ns |> foldl (\gm (r, rs) -> Game.setNeighbors r rs gm) gm
                                     |> newG |> emptyR
        Wastelands rs -> 
            Set.elems rs |> foldl (\gm r -> Game.setRegionState r wasteland gm) gm
                         |> newG |> emptyR
        OpponentStartingRegions rs ->
            Set.elems rs |> foldl (\gm r -> Game.setRegionState r starting_region gm) gm
                         |> newG |> emptyR
        PickStartingRegion i rs ->
            ( pickString sr |> Just
            , log
            , Game.setRegionState sr (Game.RegionState Us 2) gm |> newG )
            where Result sr log = (start_picker e) i rs g
        UpdateMap ps -> 
            -- Just trust whatever update_map is telling us
            Set.elems ps |> map (\(Placement o r c) -> (r, (RegionState o c)))
                         |> Map.fromList
                         |> \x -> gm { Game.states = x }
                         |> newG |> emptyR
        OpponentMoves ms -> e |> emptyR
        PlaceArmies i -> 
            ( Just (placementsString ps g)
            , log
            , foldl (flip Game.applyPlacement) gm ps |> newG )
            where Result ps log = (army_placer e) i g
        AttackOrTransfer i ->
            ( Just (movesString ms g)
            , log
            -- No move effects, because update_map will tell us
            , g |> newE )
            where ownedByUs x = (owner x) == Us
                  Result ms log = (mover e) i g
    where g = (game e)
          gm = (Game.map g)
          newE g = e { game = g }
          newG gm = ((flip Game.setMap) g) gm |> newE
          emptyR r = (Nothing, Nothing, r)
