module AI.Game where
import qualified Data.Set as Set
import Data.Set (Set, (\\))
import qualified Game as Game
import Game (Region, GameMap)
import Common ((|>))
import Owned (Owner(..))

import qualified AI.Always as Always
import qualified AI.Probability as Prob

hostileNeighbors :: Region -> GameMap -> Set Region
hostileNeighbors r gm =
    Game.graph gm |> Always.neighbors r
                  |> Set.filter (\r -> not (Game.regionOwnedBy r Us gm))

friendlyNeighbors :: Region -> GameMap -> Set Region
friendlyNeighbors r gm = 
    (Game.graph gm |> Always.neighbors r) \\ hostileNeighbors r gm

-- True if all of 'r's neighbors are friendly
isSafeRegion :: Region -> GameMap -> Bool
isSafeRegion r gm = hostileNeighbors r gm == Set.empty

safeRegions :: GameMap -> Set Region
safeRegions gm = Game.regionsOwnedBy Us gm |> Set.filter ((flip isSafeRegion) gm)

unsafeRegions :: GameMap -> Set Region
unsafeRegions gm = Game.regionsOwnedBy Us gm \\ (safeRegions gm)

isCapturable :: Region -> Integer -> Double -> GameMap -> Bool
isCapturable r units confidence gm = (Prob.capture r units gm) >= confidence

regionsWithCapturableNeighbors :: Double -> GameMap -> Set Region
regionsWithCapturableNeighbors confidence gm =
    unsafeRegions gm |> Set.filter neighborCapturable
    where neighborCapturable r = 
            let units = Always.units r gm in
                hostileNeighbors r gm 
                    |> Set.elems
                    |> any (\r -> isCapturable r units confidence gm)
