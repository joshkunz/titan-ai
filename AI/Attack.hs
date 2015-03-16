module AI.Attack where 
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map, (!))
import qualified Data.List as List

import Owned (Owner(..))
import Game (GameMap, Region, Move(..))
import Common ((|>))

import qualified AI.Probability as Prob
import qualified AI.Constant as Constant
import qualified AI.Always as Always
import qualified AI.Game

-- A mapping showing how many units are in each region
data UnitMap = Hostile (Map Region Integer) | Friendly (Map Region Integer) 
-- Mapping from friendly regions to the number of units we'll use from that
-- region in this attack plan
type MovePlan = Map Region Integer
-- Mapping from hostile regions to their move plans, used between move rounds
type MovePlanSet = Map Region MovePlan
-- A mapping from hostile regions to the list of friendly regions that can attack them
type AttackMap = Map Region [Region]

-- Hostile Region -> [Friendly Region] 
--                -> Hostile Unit Map -> Friendly UnitMap 
--                -> Current Move Plan -> (New Move Plan, New Friendly Unit Map)
type Planner = Region -> Integer -> [Region] -> UnitMap -> (MovePlan, UnitMap)

hostileUnitMap :: GameMap -> UnitMap
hostileUnitMap gm =
    AI.Game.attackableRegions gm 
        |> Set.foldl (\m r -> Map.insert r (Always.units r gm) m) Map.empty
        |> Hostile

friendlyUnitMap :: GameMap -> UnitMap 
friendlyUnitMap gm = 
    AI.Game.unsafeRegions gm 
        |> Set.foldl (\m r -> Map.insert r (usableRegionUnits r) m) Map.empty
        |> Friendly
    where usableRegionUnits r = (Always.units r gm) - Constant.minReserveForce

coopAttackCount :: Region -> Map Region (Set Region) -> Integer
coopAttackCount r m =
    Map.foldl (\v rs -> v + (intForBool (Set.member r rs))) 0 m
    where intForBool b = if b then 1 else 0

attackCountMap :: Set Region -> Map Region (Set Region) -> Map Region Integer
attackCountMap rs partMap =
    Set.foldl updateACMap Map.empty rs
    where updateACMap accMap r = Map.insert r (coopAttackCount r partMap) accMap

sortByAttackCount :: Map Region (Set Region) -> Map Region Integer -> Map Region [Region]
sortByAttackCount partMap countMap =
    Map.map sortRegions partMap
    where sortRegions rs = Set.elems rs |> List.sortBy cmpRegion
          cmpRegion ra rb = (countMap ! ra) `compare` (countMap ! rb)

-- Takes a game map, and returns a mapping from attackable region to the set
-- of regions that is capable of attacking the attackable region
attackableRegionMap :: GameMap -> Map Region [Region]
attackableRegionMap gm =
    let partMap = Set.foldl updateRMap Map.empty participatingRegions
        acMap = attackCountMap participatingRegions partMap
    in sortByAttackCount partMap acMap
    where participatingRegions = AI.Game.unsafeRegions gm
          updateRMap acc r = Set.foldl (addRToMap r) acc (AI.Game.hostileNeighbors r gm)
          addRToMap region map hostile = 
            Map.insert hostile (Set.insert region current) map
            where current = if Map.member hostile map then map ! hostile else Set.empty


emptyPlan = Map.empty :: MovePlan
emptyPlanSet = Map.empty :: MovePlanSet

initialPlans :: Planner -> [Region] -> AttackMap -> UnitMap -> UnitMap -> (MovePlanSet, UnitMap)
initialPlans planner ranked attackMap (Hostile hUnitMap) fUnitMap =
    foldl nextPlan (emptyPlanSet, fUnitMap) ranked
    where nextPlan (planSet, unitMap) h =
            planner h (hUnitMap ! h) (attackMap ! h) unitMap 
                |> \(plan, newMap) -> (addPlan h plan planSet, newMap)
          addPlan h plan planSet = 
            if plan /= emptyPlan then Map.insert h plan planSet else planSet

incrementExistingPlans :: Planner -> [Region] -> AttackMap -> UnitMap -> UnitMap -> MovePlanSet -> (MovePlanSet, UnitMap)
incrementExistingPlans planner ranked attackMap (Hostile hUnitMap) fUnitMap plans =
    foldl addPlan (plans, fUnitMap) ranked
    where addPlan (planSet, unitMap) h =
            case Map.lookup h planSet of 
                Just plan | plan /= emptyPlan -> planner h (hUnitMap ! h) (attackMap ! h) unitMap 
                                                    |> \(newPlan, newMap) -> (incPlan h plan newPlan planSet, newMap)
                          | otherwise -> (planSet, unitMap)
                Nothing -> (planSet, unitMap)
          incPlan h plan newPlan planSet 
            | newPlan == emptyPlan = planSet
            | otherwise = Map.insert h (Map.unionWith (+) plan newPlan) planSet

capPlanner :: Region -> Integer -> [Region] -> UnitMap -> (MovePlan, UnitMap) 
capPlanner h hForces attacking orig@(Friendly uMap) =
    foldl picker (0, emptyPlan) attacking |> updateuMap
    where minCap = Prob.forceOnlyMinimumCaptureForce hForces Constant.minCaptureConfidence
          picker pass@(committed, plan) r =
            let avail = uMap ! r
                needed = minCap - committed
                remaining = if needed > avail then 0 else avail - needed
                used = avail - remaining
            in if needed == 0 then pass
               else if avail == 0 then pass
               else (committed + used, Map.insert r used plan)
          updateuMap (committed, plan) = 
            if committed < minCap then (emptyPlan, orig)
            -- Subtract the units we committed in the plan, from the given uMap
            else (plan, Friendly (Map.unionWith (-) uMap plan))

-- Generate a plan that uses the rest of the units in the attacking territory
fillPlanner :: Region -> Integer -> [Region] -> UnitMap -> (MovePlan, UnitMap)
fillPlanner h hForces attacking (Friendly uMap) =
    Map.filterWithKey (\r units -> r `elem` attacking && units > 0) uMap
        |> \plan -> (plan, Friendly (Map.unionWith (-) uMap plan))

movesForPlans :: MovePlanSet -> [Move]
movesForPlans ps = 
    Map.foldlWithKey outerFolder [] ps
    where outerFolder moves hostile movePlan = 
                (Map.foldlWithKey (innerFolder hostile) [] movePlan) ++ moves
          innerFolder hostile moves ours units = 
            if units == 0 then moves else (Move Us ours hostile units) : moves
