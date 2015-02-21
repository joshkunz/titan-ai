module Game where
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Foldable as Foldable
import qualified Data.Maybe as Maybe
import qualified Graph as Graph
import qualified Sexp as Sexp
import Common ((|>), unreachable)

data SuperRegion = SuperRegion { srid :: Int
                               , bounty :: Int }

data Region = Region { rid :: Int
                     , super_region :: SuperRegion }

type RegionGraph = Graph.Graph Region

data GameSettings =
    GameSettings { timebank :: Maybe Int        -- max game time in ms
                 , time_per_move :: Maybe Int   -- max turn time in ms
                 , max_rounds :: Maybe Int      -- max round count
                 , your_bot :: Maybe String     -- name of our bot
                 , opponent_bot :: Maybe String -- name of opponent bot
                 , starting_armies :: Maybe Int -- Number of armies we can place
                 , starting_regions :: Set.Set Region -- Set of regions we can pick
                 } deriving (Show)

data GameMap = GameMap { super_regions :: Set.Set SuperRegion
                       , regions :: Set.Set Region
                       , graph :: RegionGraph }

data Game = Game { settings :: GameSettings
                 , map :: GameMap }

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

instance Show SuperRegion where
    show (SuperRegion s b) = Sexp.fromList ["SuperRegion", (show s), (show b)]

instance Show Region where
    show (Region r sr) = Sexp.fromList ["Region", (show r), (show sr)]

instance Show GameMap where
    show (GameMap srs rs g) = 
        Sexp.fromList [ "GameMap"
                      , (Set.elems srs |> Prelude.map show 
                                       |> Sexp.namedList "SuperRegions")
                      , (Set.elems rs |> Prelude.map show 
                                      |> Sexp.namedList "Regions")
                      , (show g) ]

instance Show Game where
    show (Game s m) = Sexp.fromList ["Game", (Sexp.fromShow s), (show m)]

emptySettings :: GameSettings
emptySettings = GameSettings { timebank = Nothing                 
                             , time_per_move = Nothing
                             , max_rounds = Nothing
                             , your_bot = Nothing 
                             , opponent_bot = Nothing 
                             , starting_armies = Nothing 
                             , starting_regions = Set.empty } 
emptyMap :: GameMap
emptyMap = GameMap { super_regions = Set.empty
                   , regions = Set.empty
                   , graph = Graph.empty }

empty :: Game
empty = Game { settings = emptySettings
             , Game.map = emptyMap }

setStringSetting :: String -> String -> Game -> GameSettings
setStringSetting s v (Game gs gm) =
    case s of
        "timebank" -> gs { timebank = Just (read v :: Int) }
        "time_per_move" -> gs { time_per_move = Just (read v :: Int) }
        "max_rounds" -> gs { max_rounds = Just (read v :: Int) }
        "your_bot" -> gs { your_bot = Just v }
        "opponent_bot" -> gs { opponent_bot = Just v }
        "starting_armies" -> gs { starting_armies = Just (read v :: Int) }
        "starting_regions" -> 
            words v |> Prelude.map (\x -> (read x :: Int))
                    |> Prelude.map ((flip getRegionById) gm)
                    |> Prelude.map Maybe.fromJust
                    |> Set.fromList 
                    |> \srs -> gs { starting_regions = srs }
        _ -> unreachable s

setMap :: GameMap -> Game -> Game
setMap gm g = g { Game.map = gm }

setSettings :: GameSettings -> Game -> Game
setSettings gs g = g { settings = gs }

insertSuperRegion :: SuperRegion -> GameMap -> GameMap
insertSuperRegion s gm = gm { super_regions = super_regions gm |> Set.insert s }

insertRegion :: Region -> GameMap -> GameMap
insertRegion r gm = gm { regions = regions gm |> Set.insert r }

getSuperRegionById :: Int -> GameMap -> Maybe SuperRegion
getSuperRegionById id gm = 
    super_regions gm 
        |> Foldable.find (\(SuperRegion id_ _) -> id_ == id)

getRegionById :: Int -> GameMap -> Maybe Region
getRegionById id gm =
    regions gm
        |> Foldable.find (\(Region id_ _) -> id_ == id)
