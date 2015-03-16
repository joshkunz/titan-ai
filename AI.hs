module AI where
import qualified Data.Set as Set
import Data.Set (Set(..), (\\))
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Game ( Game, SuperRegion(..), Region(..), Placement(..), Move(..)
            , RegionState(..), GameMap(..))
import qualified Game as Game
import Engine (Result(..))
import qualified Sexp as Sexp
import Owned (Owner(..))
import Common ((|>))
import qualified Graph as Graph
import Graph (Edge(..), Graph(..)) 
import Text.Printf (printf)

import AI.Common (onlyJust, enumerate)
import qualified AI.Constant as Constant
import qualified AI.Always as Always
import qualified AI.Attack as Attack
import qualified AI.Rank as Rank
import qualified AI.Game

--import Debug.Trace (trace)

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
          pick_log = "[Log/Picker] Picking Region: " ++ (show pick) ++ "\n"
          given_log = ( "[Log/Picker] Given Regions: " 
                     ++ (Sexp.c_namedSet "Regions" rs |> Sexp.render)
                     ++ "\n")

-- Assume the regions are in ranked order, try to allocate them in rank order
-- with higher ranked regions getting a larger share of the pie
weightedPlacement :: Integer -> [Region] -> [Placement]
weightedPlacement armies rs =
    foldl nextPlacement (armies, []) (enumerate rs) |> \(_, pls) -> pls
    where totalRs = (length rs |> toInteger)
          nextPlacement pass@(avail, pls) (i, r) =
            let placed = avail - (avail `div` Constant.allocationFactor)
                rest = avail - placed
            in if avail == 0 then pass
               else if i == (totalRs - 1) then (0, (Placement Us r avail) : pls)
               else (rest, (Placement Us r placed) : pls)

-- try to place an equal number on all of the given regions
rotatingPlacement :: Integer -> [Region] -> [Placement]
rotatingPlacement i rs = 
    foldl pFold [] (enumerate rs)
    where perRegion = i `div` (toInteger (length rs))
          extra = i `mod` (toInteger (length rs))
          regionPlacement index r = 
            let placedUnits = (perRegion + (if index < extra then 1 else 0)) in
                if placedUnits /= 0 then Just (Placement Us r placedUnits) else Nothing
          pFold accum (i, r) = 
              case regionPlacement i r of
                Just p -> p : accum 
                Nothing -> accum

sortCandidatesByHighestRank :: Rank.RegionRank -> Set Region -> Game -> [Region]
sortCandidatesByHighestRank f rs g =
    Set.elems rs |> map (\c -> (c, hostileNeighborRanks c))
                 |> map extractRank |> onlyJust
                 |> List.sortBy Rank.byRank |> reverse
                 |> map (\(c, _) -> c)
    where gm = Game.map g
          hostileNeighborRanks r = AI.Game.hostileNeighbors r gm |> \rs -> Rank.naive f rs r g
          extractRank (c, []) = Nothing
          extractRank (c, (_, rank) : _) = Just (c, rank)

armyPlacer :: Integer -> Game -> Result [Placement]
armyPlacer i g = 
    let placements = 
            candidates |> \cs -> sortCandidatesByHighestRank Rank.targetCapture cs g
                       |> weightedPlacement totalArmies
        armiesLog = printf "[Log/Placer] Income: %d \n" totalArmies 
        candidateLog = 
            printf "[Log/Placer] %s\n" 
                   (Set.elems candidates |> map Sexp.sexp
                                         |> Sexp.namedList "Candidates" 
                                         |> Sexp.render)
        placementLog = printf "[Log/Placer] %s\n" 
                        (map Sexp.sexp placements |> Sexp.namedList "Placements"
                                                  |> Sexp.render)
    in Result placements
              (Just (armiesLog ++ candidateLog ++ placementLog))
    where gm = Game.map g
          totalArmies = case Game.settings g |> Game.starting_armies of
                            Just x -> x
                            Nothing -> error "No starting armies set"
          candidates = AI.Game.unsafeRegions gm

moveForSafeRegion :: Region -> Game -> Maybe Move
moveForSafeRegion r g =
    case Game.unitsInRegion r gm of
        Just u -> if u > Constant.minReserveForce then
                    case Graph.closest r isUnsafe graph of
                        Just (frontline, (Edge _ nr) : _) -> Just (Move Us r nr (u - 1))
                        Just (frontline, []) -> Nothing
                        Nothing -> Nothing 
                  else Nothing
        Nothing -> error $ "Region: " ++ (show r) ++ " has no units"
    where gm = Game.map g
          graph = Game.graph gm
          isUnsafe r = not (AI.Game.isSafeRegion r gm)

safeMoves :: Game -> [Move]
safeMoves g = 
    AI.Game.safeRegions gm |> Set.elems
                           |> map ((flip moveForSafeRegion) g) 
                           |> onlyJust
    where gm = Game.map g

-- 1. meet cap
-- 2. try to double cap
-- 3. commit any remaining units

attackMoves :: Game -> [Move]
attackMoves g = 
    let gm = Game.map g
        hostileUnitMap = Attack.hostileUnitMap gm
        friendlyUnitMap = Attack.friendlyUnitMap gm
        attackMap = Attack.attackableRegionMap gm
        rankedRegions = 
            Rank.groupMap Rank.targetCapture attackMap g |> map (\(r, _) -> r)
        incRound planner (plans, newuMap) = 
            Attack.incrementExistingPlans planner rankedRegions attackMap hostileUnitMap newuMap plans
     in 
        Attack.initialPlans (Attack.capPlanner 1) rankedRegions attackMap hostileUnitMap friendlyUnitMap
            |> incRound (Attack.capPlanner 1)
            |> incRound (Attack.capPlanner 2)
            |> incRound (Attack.capPlanner 2)
            |> \(plans, _) -> Attack.movesForPlans plans

-- General strategy:
--  Attack any territories we have a chance of defeating.
--  Move units bordered completely by friendly territory towards the frontline
mover :: Integer -> Game -> Result [Move]
mover i g = 
    let moves = (safeMoves g) ++ (attackMoves g)
        log = moves |> map Sexp.sexp |> Sexp.namedList "Moves" |> Sexp.render
    in Result moves (Just ("[Log/Mover] " ++ log ++ "\n" ))
