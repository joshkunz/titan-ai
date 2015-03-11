module AI where
import qualified Data.Set as Set
import Data.Set (Set(..))
import qualified Data.List as List
import Data.Map (Map(..))
import qualified Data.Map as Map 
import Data.Ratio ((%))
import Game ( Game, SuperRegion(..), Region(..), Placement(..), Move(..)
            , RegionState(..), GameMap(..))
import qualified Game as Game
import Owned (Owner(..))
import qualified Engine as Engine
import Engine (Result(..))
import qualified Sexp as Sexp
import Common ((|>))

-- Note '%' is *not* modulus
divApprox :: (Real a, Fractional b) => a -> a -> b
divApprox a b = (realToFrac a) / (realToFrac b)

average :: (Real n, Fractional b) => [n] -> b
average ns = (realToFrac (sum ns)) / (List.genericLength ns)

-- Constants --
minArmies = 5

-- The number of armies it's possible to achieve on this map
possibleArmies :: GameMap -> Integer
possibleArmies gm =
    Game.super_regions gm |> Set.elems 
                          |> map Game.bounty 
                          |> sum |> (+) minArmies

coversSuperRegion :: SuperRegion -> [Region] -> GameMap -> Bool
coversSuperRegion sr rs gm =
    Set.difference (Game.regionsInSuperRegion sr gm)
                   (Set.fromList rs) |> (==) Set.empty

groupBySuperRegion :: [Region] -> [[Region]]
groupBySuperRegion rs = 
    List.sortBy bySRId rs |> List.groupBy sameId
    where bySRId (Region _ (SuperRegion i1 _)) 
                 (Region _ (SuperRegion i2 _)) = compare i1 i2
          sameId r1 r2 = (bySRId r1 r2) == EQ

-- The set of super regions owned by the given owner
superRegionsOwnedBy :: Owner -> GameMap -> Set SuperRegion
superRegionsOwnedBy o gm = 
    Game.regionsOwnedBy o gm 
        |> Set.elems |> groupBySuperRegion
        |> map groupCoversSR 
        |> filter (\(sr, c) -> c)     -- Collect only covered regions
        |> map (\(sr, True) -> sr)    -- Strip coverage status
        |> Set.fromList
    where groupCoversSR rs@((Region _ sr) : rest) = 
            (sr, coversSuperRegion sr rs gm)
    
armiesOf :: Owner -> GameMap -> Integer
armiesOf o gm =
    superRegionsOwnedBy o gm 
        |> Set.elems
        |> map (\(SuperRegion i b) -> b)
        |> sum |> (+) minArmies

totalPopulation :: GameMap -> Integer
totalPopulation gm =
    Map.elems (Game.states gm) 
        |> map Game.units |> sum

populationOf :: Owner -> GameMap -> Integer
populationOf o gm =
    Map.elems (Game.states gm)
        |> filter (\(RegionState o_ u) -> o_ == o)
        |> map Game.units |> sum

-- Rank how well the particular owner is performing in the game
--  The rank is the average of a number of ratios between the player's
--  performance and the best possible performance.
rankGame :: (Fractional n) => Owner -> Game -> n
rankGame o g =
    average [incomeRatio, regionRatio, populationRatio]
    where gm = Game.map g
          incomeRatio = (armiesOf o gm) `divApprox` (possibleArmies gm)
          regionRatio = (Set.size (Game.regionsOwnedBy o gm)) 
            `divApprox` (Set.size (Game.regions gm)) 
          populationRatio = (populationOf o gm)
                `divApprox` (totalPopulation gm)

maxSRBounty :: Set Region -> Game -> Integer
maxSRBounty rs g =
    let next_max v (Region _ (SuperRegion _ b)) = if b > v then b else v
    in Set.foldl next_max 0 rs

highestRegions :: Set Region -> Game -> Set Region
highestRegions rs g =
    Set.filter (\(Region _ (SuperRegion _ b)) -> b >= maxBounty) rs
    where maxBounty = maxSRBounty rs g

-- General strategy: 
--   First, pick starting territories in a high-value super region
--   Second, pick close to other territories we own
--   Second, pick away from wastelands
--   Third, pick close to enemy territory
startPicker :: Integer -> Set Region -> Game -> Result Region
startPicker i rs g =
    Result pick (Just (given_log ++ pick_log))
    where pick = highestRegions rs g |> Set.elems |> head 
          pick_log = "[Log] Picking Region: " ++ (show pick) ++ "\n"
          given_log = ( "[Log] Given Regions: " 
                     ++ (Sexp.c_namedSet "Regions" rs |> Sexp.render)
                     ++ "\n")

armyPlacer :: Integer -> Game -> Result [Placement]
armyPlacer i g = 
    Result [Placement Us lucky_r armies]
           (Just ( armies_log ++ r_log ))
    where gm = Game.map g
          armies = case Game.settings g |> Game.starting_armies of
                        Just x -> x
                        Nothing -> error "No starting armies set"
          lucky_r = case Game.regionsOwnedBy Us gm |> Set.elems of
                        [] -> error "No territory owned by us"
                        r : rest -> r
          armies_log = "[Log] Have: " ++ (show armies) ++ " armies to place.\n"
          r_log = "[Log] Lucky Region: " ++ (show lucky_r) ++ "\n"

mover :: Integer -> Game -> Result [Move]
mover i g = [] |> Engine.noLog

