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

type MoveState_ = (Integer, [Move], Map Region Integer)
nextMove :: Region -> Integer -> MoveState_ -> Region -> MoveState_
nextMove hostile minCap pass@(units, moves, unitMap) r =
    let avail = unitMap ! r
        needed = minCap - units
        remaining = if needed > avail then 0 else avail - needed
        used = avail - remaining
    in if needed == 0 then pass
       else if avail == 0 then pass
       else ( units + used
            , ((Move Us r hostile used) : moves)
            , Map.insert r remaining unitMap )

-- Takes a region to attack, a list of regions that can attack the region (in rank order)
-- And a mapping that reflects the currently available armies for each region.
-- This function returns a list of moves that realize this action, or none if the
-- action couldn't be completed, as well as a new mapping reflecting the units
-- used to realize the action.
movesForRegion :: Region -> [Region] -> Map Region Integer -> Map Region Integer -> ([Move], Map Region Integer)
movesForRegion h rs hUnitMap fUnitMap =
    let hostileForces = hUnitMap ! h
        minCap = Prob.forceOnlyMinimumCaptureForce hostileForces Constant.minCaptureConfidence
        availForces = map (\r -> fUnitMap ! r) rs |> sum
    in if availForces < minCap then ([], fUnitMap) 
       else foldl (nextMove h minCap) (0, [], fUnitMap) rs 
                |> \(_, ms, unitMap) -> (ms, unitMap)

hostileUnitMap :: GameMap -> Map Region Integer
hostileUnitMap gm =
    AI.Game.attackableRegions gm 
        |> Set.foldl (\m r -> Map.insert r (Always.units r gm) m) Map.empty

friendlyUnitMap :: GameMap -> Map Region Integer
friendlyUnitMap gm = 
    AI.Game.unsafeRegions gm 
        |> Set.foldl (\m r -> Map.insert r (Always.units r gm) m) Map.empty

-- Takes a ranked list of hostile regions, a mapping from hostile regions to
-- the set of friendly regions that are capable of attacking them and a mapping
-- from friendly regions to the number of units they can allocate to any attack
-- this round. It returns a list of moves for this round to accomplish as many
-- attacks as possible in rank order.
assignAttacks :: [Region] -> Map Region [Region] -> Map Region Integer -> Map Region Integer -> [Move]
assignAttacks ranked attackMap hUnitMap fUnitMap =
    foldl nextAttacks ([], fUnitMap) ranked |> \(moves, _) -> moves
    where nextAttacks (cMoves, unitMap) r = 
            let (nextMoves, nextunitMap) = movesForRegion r (attackMap ! r) hUnitMap unitMap
            in ((cMoves ++ nextMoves), nextunitMap)
                        
