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
import AI.Constant as Constant
import qualified AI.Always as Always
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
    let placements = if captureCandidates == [] 
                        then rotatingPlacement totalArmies tideCandidates
                        else (rotatingPlacement captureArmies captureCandidates)
                          ++ (rotatingPlacement tideArmies tideCandidates)
        armiesLog = printf "[Log/Placer] total: %d cap: %d tide: %d\n"
                            totalArmies captureArmies tideArmies
        candidateLog = let capture = (captureCandidates |> map Sexp.sexp |> Sexp.namedList "CaptureCandidates")
                           tide = tideCandidates |> map Sexp.sexp |> Sexp.namedList "TideCandidates"
                       in printf "[Log/Placer] %s\n" (Sexp.fromList [capture, tide] |> Sexp.render)
        placementLog = printf "[Log/Placer] %s\n" 
                        (map Sexp.sexp placements |> Sexp.namedList "Placements"
                                                  |> Sexp.render)
    in Result placements
              (Just (armiesLog ++ candidateLog ++ placementLog))
    where gm = Game.map g
          totalArmies = case Game.settings g |> Game.starting_armies of
                            Just x -> x
                            Nothing -> error "No starting armies set"
          captureArmies = round ((realToFrac totalArmies) * captureFraction)
          tideArmies = totalArmies - captureArmies
          captureCandidates = AI.Game.regionsWithCapturableNeighbors minCaptureConfidence gm
                                |> \cs -> sortCandidatesByHighestRank Rank.targetCapture cs g
          tideCandidates = AI.Game.unsafeRegions gm \\ (Set.fromList captureCandidates)
                            |> \cs -> sortCandidatesByHighestRank Rank.targetPlacement cs g

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


moveAttackBasic :: Region -> Game -> Maybe Move
moveAttackBasic r g =
    let possibleTargets = 
            AI.Game.hostileNeighbors r gm |> Set.filter capturableAtConfidence 
    in if possibleTargets /= Set.empty then
        Rank.naive Rank.targetCapture possibleTargets r g |> pickAttack
       else Nothing
    where gm = Game.map g
          units = Always.units r gm
          capturableAtConfidence r = AI.Game.isCapturable r units Constant.minCaptureConfidence gm
          pickAttack [] = Nothing
          pickAttack ((hr,_) : _) = Just (Move Us r hr (units - 1))

-- General strategy:
--  Attack any territories we have a chance of defeating.
--  Move units bordered completely by friendly territory towards the frontline
mover :: Integer -> Game -> Result [Move]
mover i g = 
    let safeMoves = Set.elems safe 
                        |> map ((flip moveForSafeRegion) g) |> onlyJust
        attackMoves = Set.elems unsafe 
                        |> map ((flip moveAttackBasic) g) |> onlyJust
        moves = safeMoves ++ attackMoves
        log = moves |> map Sexp.sexp |> Sexp.namedList "Moves" |> Sexp.render
    in Result moves (Just ("[Log/Mover] " ++ log ++ "\n" ))
    where gm = Game.map g
          safe = AI.Game.safeRegions gm
          unsafe = AI.Game.unsafeRegions gm
