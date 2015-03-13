module Game where
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Foldable as Foldable
import qualified Data.Maybe as Maybe
import qualified Graph as Graph
import qualified Sexp as Sexp
import qualified Owned as Owned
import Owned (Owner(..)) 
import Sexp (Sexp, sexp)
import Common ((|>), unreachable)

-- The number of units on wastelands. Not specifically in
-- the rules, but it appears to be this count from the
-- games I've seen.
wasteland_units = 6

data SuperRegion = SuperRegion { srid :: Integer 
                               , bounty :: Integer }

data Region = Region { rid :: Integer
                     , super_region :: SuperRegion }

data RegionState = RegionState { owner :: Owned.Owner 
                               , units :: Integer }

type RegionGraph = Graph.Graph Region

data GameSettings =
    GameSettings { timebank :: Maybe Integer        -- max game time in ms
                 , time_per_move :: Maybe Integer   -- max turn time in ms
                 , max_rounds :: Maybe Integer      -- max round count
                 , your_bot :: Maybe String     -- name of our bot
                 , opponent_bot :: Maybe String -- name of opponent bot
                 , starting_armies :: Maybe Integer -- Number of armies we can place
                 , starting_regions :: Set.Set Region -- Set of regions we can pick
                 , starting_pick_amount :: Maybe Integer -- The number of regions we can pick from the starting set
                 }

data GameMap = GameMap { super_regions :: Set.Set SuperRegion
                       , regions :: Set.Set Region
                       , states :: Map.Map Region RegionState
                       , graph :: RegionGraph }

data Game = Game { settings :: GameSettings
                 , map :: GameMap }

-- (Placement region count)
data Placement = 
    Placement Owned.Owner Game.Region Integer 
        deriving (Eq, Ord)

instance Owned.Owned Placement where
    owner (Placement o _ _) = o

-- (Move source-region dest-region count)
data Move = 
    Move Owned.Owner Region Region Integer 
        deriving (Eq, Ord)

instance Owned.Owned Move where
    owner (Move o _ _ _) = o

instance Eq SuperRegion where
    (==) (SuperRegion ra ba) (SuperRegion rb bb) = (ra == rb) && (ba == bb)

instance Ord SuperRegion where
    compare (SuperRegion ra ba) (SuperRegion rb bb) = 
        case compare ra rb of
            EQ -> compare ba bb
            x -> x

instance Eq Region where
    (==) (Region ra sra) (Region rb srb) = (ra == rb) && (sra == srb)

instance Ord Region where
    compare (Region ra sra) (Region rb srb) =
        case compare ra rb of
            EQ -> compare sra srb
            x -> x

instance Sexp Owned.Owner where
    sexp o = Sexp.fromString (show o)

instance Sexp RegionState where
    sexp (RegionState owner units) = 
        Sexp.namedStringList "RegionState" [(show owner), (show units)]

instance Sexp SuperRegion where
    sexp (SuperRegion s b) = 
        Sexp.namedStringList "SuperRegion" [(show s), (show b)]

instance Sexp Region where
    sexp (Region r sr) = 
        Sexp.namedList "Region" [(Sexp.fromString $ show r), (sexp sr)]

instance Show Region where
    show r = sexp r |> Sexp.render

instance Sexp GameMap where
    sexp (GameMap srs rs st g) = 
        Sexp.namedList "GameMap"
            [ (Set.elems srs |> Prelude.map sexp 
                             |> Sexp.namedList "SuperRegions")
            , (Set.elems rs |> Prelude.map sexp 
                            |> Sexp.namedList "Regions")
            , (Map.assocs st |> Prelude.map (\(k, v) -> (sexp k, sexp v))
                             |> Prelude.map Sexp.fromPair
                             |> Sexp.namedList "States")
            , (sexp g) ]

showMaybe :: (Show a) => Maybe a -> String
showMaybe (Just a) = show a
showMaybe Nothing = "'None"

instance Sexp GameSettings where
    sexp s = [ ["timebank", showMaybe $ timebank s]
             , ["time-per-move", showMaybe $ time_per_move s]
             , ["max-rounds", showMaybe $ max_rounds s]
             , ["your-bot", showMaybe $ your_bot s]
             , ["opponent-bot", showMaybe $ opponent_bot s]
             , ["starting-armies", showMaybe $ starting_armies s]
             , ["starting-pick-amount", showMaybe $ starting_pick_amount s]  ]
             |> Prelude.map Sexp.fromStringList 
             |> \x -> x ++ [(starting_regions s 
                                |> Set.toList |> Prelude.map sexp 
                                |> Sexp.namedList "starting-regions")]
             |> Sexp.namedList "GameSettings"

instance Sexp Game where
    sexp (Game s m) = Sexp.namedList "Game" [(sexp s), (sexp m)]

instance Sexp Placement where
    sexp (Placement o r i) =
        Sexp.namedList "Placement" [ sexp o , sexp r 
                                   , Sexp.fromString (show i) ]

instance Show Placement where
    show p = sexp p |> Sexp.render

instance Sexp Move where
    sexp (Move o s d i) =
        Sexp.namedList "Move" [ sexp o , sexp s , sexp d
                              , Sexp.fromString (show i) ]

instance Show Move where
    show m = sexp m |> Sexp.render

instance Show Game where
    show g = sexp g |> Sexp.render

emptySettings :: GameSettings
emptySettings = GameSettings { timebank = Nothing                 
                             , time_per_move = Nothing
                             , max_rounds = Nothing
                             , your_bot = Nothing 
                             , opponent_bot = Nothing 
                             , starting_armies = Nothing 
                             , starting_regions = Set.empty 
                             , starting_pick_amount = Nothing } 
emptyMap :: GameMap
emptyMap = GameMap { super_regions = Set.empty
                   , regions = Set.empty
                   , states = Map.empty
                   , graph = Graph.empty }

empty :: Game
empty = Game { settings = emptySettings
             , Game.map = emptyMap }

setStringSetting :: String -> String -> Game -> GameSettings
setStringSetting s v (Game gs gm) =
    case s of
        "timebank" -> gs { timebank = Just (read v :: Integer) }
        "time_per_move" -> gs { time_per_move = Just (read v :: Integer) }
        "max_rounds" -> gs { max_rounds = Just (read v :: Integer) }
        "your_bot" -> gs { your_bot = Just v }
        "opponent_bot" -> gs { opponent_bot = Just v }
        "starting_armies" -> gs { starting_armies = Just (read v :: Integer) }
        "starting_regions" -> 
            words v |> Prelude.map (\x -> (read x :: Integer))
                    |> Prelude.map ((flip getRegionById) gm)
                    |> Prelude.map Maybe.fromJust
                    |> Set.fromList 
                    |> \srs -> gs { starting_regions = srs }
        "starting_pick_amount" -> 
            gs { starting_pick_amount = Just (read v :: Integer) }
        _ -> unreachable s

setMap :: GameMap -> Game -> Game
setMap gm g = g { Game.map = gm }

setSettings :: GameSettings -> Game -> Game
setSettings gs g = g { settings = gs }

insertSuperRegion :: SuperRegion -> GameMap -> GameMap
insertSuperRegion s gm = gm { super_regions = super_regions gm |> Set.insert s }

insertRegion :: Region -> GameMap -> GameMap
insertRegion r gm = gm { regions = regions gm |> Set.insert r }

setNeighbors :: Region -> Set.Set Region -> GameMap -> GameMap
setNeighbors r rs gm = 
    Set.elems rs |> foldl (flip (Graph.insertEdge r)) (graph gm) 
                 |> \x -> gm { graph = x }

setRegionState :: Region -> RegionState -> GameMap -> GameMap
setRegionState r s gm =
    states gm |> Map.insert r s
              |> \s -> gm { states = s }

getRegionState :: Region -> GameMap -> Maybe RegionState
getRegionState r gm =
    states gm |> Map.lookup r

getSuperRegionById :: Integer -> GameMap -> Maybe SuperRegion
getSuperRegionById id gm = 
    super_regions gm 
        |> Foldable.find (\(SuperRegion id_ _) -> id_ == id)

regionsInSuperRegion :: SuperRegion -> GameMap -> Set.Set Region 
regionsInSuperRegion sr gm =
    Set.filter (regionIn sr) (regions gm)
    where regionIn sr (Region id su) = sr == su

getRegionById :: Integer -> GameMap -> Maybe Region
getRegionById id gm =
    regions gm
        |> Foldable.find (\(Region id_ _) -> id_ == id)

stateForRegion :: Region -> GameMap -> Maybe RegionState
stateForRegion r gm = Map.lookup r (states gm) 

unitsInRegion :: Region -> GameMap -> Maybe Integer
unitsInRegion r gm = stateForRegion r gm |> fmap units 

regionOwner :: Region -> GameMap -> Maybe Owner
regionOwner r gm = stateForRegion r gm |> fmap owner

regionOwnedBy :: Region -> Owner -> GameMap -> Bool
regionOwnedBy r o gm = 
    case regionOwner r gm of
        Just o_ -> o_ == o
        Nothing -> error ("No owner for region: " ++ (show r))

regionsOwnedBy :: Owner -> GameMap -> Set.Set Region
regionsOwnedBy o gm  = 
    Game.regions gm |> Set.filter (\r -> regionOwnedBy r o gm)

applyPlacement :: Placement -> GameMap -> GameMap
applyPlacement (Placement o region count) gm =
    RegionState o count |> \rs -> setRegionState region rs gm

applyPlacementGame :: Placement -> Game -> Game
applyPlacementGame p g = Game.map g |> applyPlacement p |> (flip setMap) g
