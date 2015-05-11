module AI.Rank where
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Game as Game
import qualified Owned as Owned
import Owned(Owner(..)) 
import Game (Region(..), SuperRegion(..), GameMap, Game)
import Common ((|>), not_implemented)

import AI.Common (divApprox, average)
import qualified AI.Constant as Constant
import qualified AI.Always as Always
import qualified AI.Game

type RegionRank = Region -> Region -> Game -> Double
type RegionOrder = Region -> Region -> Region -> Region -> Game -> Ordering 

-- The number of armies it's possible to achieve on this map
possibleArmies :: GameMap -> Integer
possibleArmies gm =
    Game.super_regions gm |> Set.elems 
                          |> map Game.bounty 
                          |> sum |> (+) Constant.minIncome

coversSuperRegion :: SuperRegion -> [Region] -> GameMap -> Bool
coversSuperRegion sr rs gm =
    Set.difference (Game.superRegionRegions sr gm)
                   (Set.fromList rs) |> (==) Set.empty

groupBySuperRegion :: [Region] -> [[Region]]
groupBySuperRegion rs = 
    List.sortBy bySRId rs |> List.groupBy sameId
    where bySRId (Region _ (SuperRegion i1 _)) 
                 (Region _ (SuperRegion i2 _)) = compare i1 i2
          sameId r1 r2 = (bySRId r1 r2) == EQ

targetCapture :: Region -> Region -> Game -> Double -- [0..1]
targetCapture _ r@(Region _ sr) g = 
    friendlyFraction + superRegionCoverFraction
    where gm = Game.map g
          graph = Game.graph gm
          -- How much territory do we have bordering the region?
          friendlyFraction = (Set.size (AI.Game.friendlyNeighbors r gm)) 
                 `divApprox` (Set.size (Always.neighbors r graph))
          allInSr = Game.superRegionRegions sr gm
          oursInSr = Set.filter (\r -> Game.regionOwnedBy r Us gm) allInSr
          -- How much of the target super region do we own?
          superRegionCoverFraction = (Set.size oursInSr)
                         `divApprox` (Set.size allInSr)

targetPlacement :: Region -> Region -> Game -> Double
targetPlacement src t g = 
    destUnits `divApprox` (srcUnits + destUnits)
    where gm = Game.map g
          srcUnits = Always.units src gm
          destUnits = Always.units t gm

byRank :: (Region, Double) -> (Region, Double) -> Ordering
byRank (_, ra) (_, rb) = compare ra rb

groupSingle :: RegionRank -> Region -> [Region] -> Game -> Double
groupSingle f h rs g = map (\r -> f r h g) rs |> average

groupMap :: RegionRank -> Map Region [Region] -> Game -> [(Region, Double)]
groupMap f m g = 
    Map.foldlWithKey nextRank [] m |> List.sortBy byRank |> reverse
    where nextRank l hostile attks = (hostile, groupSingle f hostile attks g) : l

naive :: RegionRank -> Set Region -> Region -> Game -> [(Region, Double)]
naive f rs r g =
    Set.foldl rankFold Set.empty rs
        |> Set.elems
        |> List.sortBy byRank
        |> reverse
    where rankFold acc target = (Set.insert (target, (f r target g)) acc)

ordered :: RegionOrder -> Set Region -> Region -> Game -> [(Region, Double)]
ordered = not_implemented "AI.Rank.ordered"
