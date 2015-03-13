module AI where
import qualified Data.Set as Set
import Data.Set (Set(..), (\\))
import qualified Data.List as List
import Data.Map (Map(..))
import qualified Data.Map as Map 
import qualified Data.Maybe as Maybe
import qualified Data.Array.IArray as Array
import Data.Array.IArray (IArray(..), (!))
import Data.Array (Array)
import Data.Ratio ((%))
import Game ( Game, SuperRegion(..), Region(..), Placement(..), Move(..)
            , RegionState(..), GameMap(..))
import qualified Game as Game
import Owned (Owner(..))
import qualified Engine as Engine
import Engine (Result(..))
import qualified Sexp as Sexp
import Common ((|>), choose)
import qualified Graph as Graph
import Graph (Edge(..), Graph(..)) 

--import Debug.Trace (trace)

-- Constants --
minIncome = 5

minDefenceForce = 1

-- Probability an attacker destroys a defender and vice versa
attackerProb = 0.6
defenderProb = 0.7
-- Weight of lucky kills, assume det. kills are weighted 1 - <this value>
luckFactor = 0.16

minCaptureConfidence = 0.90

onlyJust :: [Maybe a] -> [a]
onlyJust [] = []
onlyJust ((Just x) : xs) = x : (onlyJust xs)
onlyJust (Nothing : xs) = onlyJust xs

-- Note '%' is *not* modulus
divApprox :: (Real a, Fractional b) => a -> a -> b
divApprox a b = (realToFrac a) / (realToFrac b)

average :: (Real n, Fractional b) => [n] -> b
average ns = (realToFrac (sum ns)) / (List.genericLength ns)

pBinom :: (RealFloat a) => a -> a -> a -> a
pBinom p n k = (n `choose` k) * (p ** k) * ((1 - p) ** (n - k))

cdfBinom :: (RealFloat a) => a -> a -> a -> a
cdfBinom p n k = [0..(floor k)] |> map realToFrac
                                |> map (pBinom p n) |> sum 

-- The number of armies it's possible to achieve on this map
possibleArmies :: GameMap -> Integer
possibleArmies gm =
    Game.super_regions gm |> Set.elems 
                          |> map Game.bounty 
                          |> sum |> (+) minIncome

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
        |> sum |> (+) minIncome

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

-- The probability that a given number of units (ourUnits) with a given
-- attack success probability (prob) will kill all of the enemy units
-- (theirUnits). The constant `luckFactor` is used to describe the weight
-- of the lucky vs. deterministic attacks.
killsAllProbability :: Double -> Double -> Double -> Double
killsAllProbability prob ourUnits theirUnits = 
    if extraKillsNeeded <= 0 then 1.0
    else if adjustedExtra > ourUnits then 0.0
    else (1 - (cdfBinom prob ourUnits adjustedExtra))
    where guaranteedKills = prob * ourUnits * (1 - luckFactor)
          extraKillsNeeded = theirUnits - guaranteedKills
          adjustedExtra = extraKillsNeeded * (100 * luckFactor)

forceOnlyCaptureProbability :: Integer -> Integer -> Double
forceOnlyCaptureProbability aUnits dUnits =
    -- A killing all and D killing all are independent so P(A \cap B) = P(A)P(B)
    aKillsAllp * (1 - dKillsAllp)
    where aUnits_ = realToFrac aUnits
          dUnits_ = realToFrac dUnits
          aKillsAllp = killsAllProbability attackerProb aUnits_ dUnits_
          dKillsAllp = killsAllProbability defenderProb dUnits_ aUnits_

-- The probability that we'll capture the given region 'r' with 
-- the given number 'a_units' units according to the region state
-- in the game map. Specifically the probability that we'll destroy all
-- of the occupying forces and still have at least one unit remaining
captureProbability :: Region -> Integer -> GameMap -> Double
captureProbability r aUnits gm =
    forceOnlyCaptureProbability aUnits dUnits
    where dUnits = case Game.unitsInRegion r gm of 
                        Just u -> u
                        Nothing -> error $ "No such region: " ++ (show r)

-- Garunteed capture with a force of this size or larger assuming attacks
-- have 'p' probability of succeeding.
maxRequiredCaptureForce :: Double -> Integer -> Integer
maxRequiredCaptureForce p f = 
    (realToFrac f) / ((1 - luckFactor) * p) |> ceiling |> (+) 1

-- Find the index of the element closest to 'e' in array. Prefers the higher
-- index if the value falls on a boundary.
approxBinarySearch :: (Ord e, Integral i, Array.Ix i) => e -> Array i e -> i
approxBinarySearch elem arr =
    let bsearch low high = if (high - low) <= 1 then high
                           else let pivot = (low + high) `div` 2
                                    item = arr ! pivot
                                in case compare item elem of
                                       EQ -> pivot
                                       LT -> bsearch pivot high
                                       GT -> bsearch low pivot
    in (\(low, high) -> bsearch low high) $ Array.bounds arr

forceOnlyMinimumCaptureForce :: Integer -> Double -> Integer
forceOnlyMinimumCaptureForce dUnits con =
    approxBinarySearch con chanceArray
    where maxForce = maxRequiredCaptureForce attackerProb dUnits 
          forceCapProb i = forceOnlyCaptureProbability i dUnits
          chanceArray = 
            (Array.array (1, maxForce) 
                        [ (i, forceCapProb i) | i <- [1..maxForce]])

minimumCaptureForce :: Region -> Double -> GameMap -> Integer
minimumCaptureForce r con gm =
    forceOnlyMinimumCaptureForce dUnits con
    where dUnits = case Game.unitsInRegion r gm of
                        Just u -> u
                        Nothing -> error $ "No such region: " ++ (show r)

aNeighbors :: (Ord a, Show a) => a -> Graph a -> Set a
aNeighbors n g = 
    case Graph.neighbors n g of
        Just ns -> ns
        Nothing -> error $ "Region: " ++ (show n) ++ " has no neighbors"

hostileNeighbors :: Region -> GameMap -> Set Region
hostileNeighbors r gm =
    Game.graph gm |> aNeighbors r
                  |> Set.filter (\r -> not (Game.regionOwnedBy r Us gm))

friendlyNeighbors :: Region -> GameMap -> Set Region
friendlyNeighbors r gm = 
    (Game.graph gm |> aNeighbors r) \\ hostileNeighbors r gm

-- True if all of 'r's neighbors are friendly
isSafeRegion :: Region -> GameMap -> Bool
isSafeRegion r gm = hostileNeighbors r gm == Set.empty

safeRegions :: Game -> Set Region
safeRegions g =
    let gm = Game.map g in
        Game.regionsOwnedBy Us gm |> Set.filter ((flip isSafeRegion) gm)

unsafeRegions :: Game -> Set Region
unsafeRegions g = 
    let gm = Game.map g in Game.regionsOwnedBy Us gm \\ (safeRegions g)

moveForSafeRegion :: Region -> Game -> Maybe Move
moveForSafeRegion r g =
    case Game.unitsInRegion r gm of
        Just u -> if u > minDefenceForce then
                    case Graph.closest r isUnsafe graph of
                        Just (frontline, (Edge _ nr) : []) -> Just (Move Us r nr (u - 1))
                        Nothing -> Nothing 
                  else Nothing
        Nothing -> error $ "Region: " ++ (show r) ++ " has no units"
    where gm = Game.map g
          graph = Game.graph gm
          isUnsafe r = not (isSafeRegion r gm)

rankTarget :: Region -> Game -> Double -- [0..1]
rankTarget r@(Region _ sr) g = 
    friendlyFraction + superRegionCoverFraction
    where gm = Game.map g
          graph = Game.graph gm
          -- How much territory do we have bordering the region?
          friendlyFraction = (Set.size (friendlyNeighbors r gm)) 
                 `divApprox` (Set.size (aNeighbors r graph))
          allInSr = Game.regionsInSuperRegion sr gm
          oursInSr = Set.filter (\r -> Game.regionOwnedBy r Us gm) allInSr
          superRegionCoverFraction = (Set.size oursInSr)
                         `divApprox` (Set.size allInSr)

byRank :: (Region, Double) -> (Region, Double) -> Ordering
byRank (_, ra) (_, rb) = compare ra rb

isCapturable :: Region -> Integer -> Double -> GameMap -> Bool
isCapturable r units confidence gm =
    (captureProbability r units gm) >= confidence

regionTargets :: Region -> Game -> [(Region, Double)]
regionTargets r g = possibleTargets
        |> Set.filter (\r -> isCapturable r units minCaptureConfidence gm)
        |> Set.elems
        |> map (\r -> (r, rankTarget r g))
        |> List.sortBy byRank 
        |> reverse
    where gm = Game.map g
          units = Game.unitsInRegion r gm 
                    |> Maybe.fromMaybe (error $ "No units in regionTargets")
          possibleTargets = hostileNeighbors r gm

moveAttackBasic :: Region -> Game -> Maybe Move
moveAttackBasic r g =
    regionTargets r g |> pickAttack
    where units = Game.unitsInRegion r (Game.map g)
                    |> Maybe.fromMaybe (error $ "No units in regionTargets")
          pickAttack [] = Nothing
          pickAttack ((hr,_) : _) = Just (Move Us r hr (units - 1))

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

enumerate :: [a] -> [(Integer, a)]
enumerate as = zip [0..(toInteger (length as))] as

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


someRegion :: Game -> Region
someRegion g = Game.map g |> Game.regionsOwnedBy Us |> Set.elems |> head

armyPlacer :: Integer -> Game -> Result [Placement]
armyPlacer i g = 
    Result placements 
           (Just ( armiesLog ++ candidateLog ++ placementLog))
    where gm = Game.map g
          armies = case Game.settings g |> Game.starting_armies of
                        Just x -> x
                        Nothing -> error "No starting armies set"
          extractRank (c, []) = Nothing
          extractRank (c, (_, rank) : _) = Just (c, rank)
          sortedCandidates =
            unsafeRegions g
                |> Set.elems
                |> map (\c -> (c, regionTargets c g))
                |> map extractRank |> onlyJust
                |> List.sortBy byRank |> reverse 
                |> map (\(c, _) -> c)
          checkedCandidates = if sortedCandidates == [] then [someRegion g]
                              else sortedCandidates
          placements = rotatingPlacement armies checkedCandidates 
          armiesLog = "[Log/Placer] Have: " 
                   ++ (show armies) ++ " armies to place.\n"
          candidateLog = "[Log/Placer] " 
                      ++ (checkedCandidates |> map Sexp.sexp 
                                            |> Sexp.namedList "Candidates"
                                            |> Sexp.render)
                      ++ "\n"
          placementLog = "[Log/Placer] " 
                      ++ (map Sexp.sexp placements |> Sexp.namedList "Placements"
                                                   |> Sexp.render)
                      ++ "\n"

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
    in Result moves (Just ("[Log/Mover] " ++ log))
    where safe = safeRegions g
          unsafe = unsafeRegions g
