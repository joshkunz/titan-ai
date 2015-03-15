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

hostileUnitMap :: GameMap -> Map Region Integer
hostileUnitMap gm =
    AI.Game.attackableRegions gm 
        |> Set.foldl (\m r -> Map.insert r (Always.units r gm) m) Map.empty

friendlyUnitMap :: GameMap -> Map Region Integer
friendlyUnitMap gm = 
    AI.Game.unsafeRegions gm 
        |> Set.foldl (\m r -> Map.insert r (usableRegionUnits r) m) Map.empty
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

type MovePlan = Map Region Integer
type AttackMap = Map Region [Region]
type UnitMap = Map Region Integer
type Planner = Region -> [Region] -> UnitMap -> UnitMap -> MovePlan -> (MovePlan, UnitMap)
type MoveState = (Integer, MovePlan, UnitMap)
type MovePlanSet = Map Region MovePlan
type CAPMover = Region -> Integer -> MoveState -> Region -> MoveState

emptyPlanSet = Map.empty :: MovePlanSet
emptyPlan = Map.empty :: MovePlan

-- 1. meet cap
-- 2. try to double cap
-- 3. commit any remaining units

basicCAPMover :: Region -> Integer -> MoveState -> Region -> MoveState
basicCAPMover hostile minCap pass@(units, curPlan, unitMap) r =
    let avail = unitMap ! r
        needed = minCap - units
        remaining = if needed > avail then 0 else avail - needed
        used = avail - remaining
    in if needed == 0 then pass
       else if avail == 0 then pass
       else ( units + used
            , Map.insert r used curPlan
            , Map.insert r remaining unitMap )

orderedCAPPlanner :: CAPMover -> Region -> [Region] -> UnitMap -> UnitMap -> MovePlan -> (MovePlan, UnitMap) 
orderedCAPPlanner f hostile attacking hostileMap friendlyMap curPlan =
    let hostileForces = hostileMap ! hostile
        minCap = Prob.forceOnlyMinimumCaptureForce hostileForces Constant.minCaptureConfidence
        availableForces = map (\r -> friendlyMap ! r) attacking |> sum
    in if availableForces < minCap then (curPlan, friendlyMap)
       else foldl (f hostile minCap) (0, emptyPlan, friendlyMap) attacking
                |> \(_, plan, unitMap) -> (Map.unionWith (+) curPlan plan, unitMap)

stuffingMover :: Region -> (MovePlan, UnitMap) -> (MovePlan, UnitMap)
stuffingMover r (plan, map) =
    let avail = map ! r in
        if avail > 0 then 
            ( Map.insert r avail plan
            , Map.insert r 0 map ) 
        else (plan, map)

type BasicMover = Region -> (MovePlan, UnitMap) -> (MovePlan, UnitMap)
basicPlanner :: BasicMover -> Region -> [Region] -> UnitMap -> UnitMap -> MovePlan -> (MovePlan, UnitMap)
basicPlanner f hostile attacking hostileMap friendlyMap curPlan =
    foldl (flip f) (emptyPlan, friendlyMap) attacking 
        |> \(plan, map) -> ((Map.unionWith (+) curPlan plan), map)

applyRound :: Planner -> [Region] -> AttackMap -> UnitMap -> UnitMap -> MovePlanSet -> (MovePlanSet, UnitMap)
applyRound planner ranked attackMap hostileMap friendlyMap plans =
    foldl nextPlan (plans, friendlyMap) ranked
    where nextPlan (cPlans, unitMap) r = 
            let currentPlan = if Map.member r cPlans then cPlans ! r else emptyPlan
                (nextPlan, nextUnitMap) = planner r (attackMap ! r) hostileMap friendlyMap currentPlan
            in ((Map.insert r nextPlan cPlans), nextUnitMap)

assignAttacks :: MovePlanSet -> [Move]
assignAttacks ps = 
    Map.foldlWithKey outerFolder [] ps
    where outerFolder moves hostile movePlan = 
                (Map.foldlWithKey (innerFolder hostile) [] movePlan) ++ moves
          innerFolder hostile moves ours units = 
            if units == 0 then moves else (Move Us ours hostile units) : moves
