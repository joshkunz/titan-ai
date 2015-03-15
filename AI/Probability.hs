module AI.Probability where 
import qualified Data.Array.IArray as Array
import Data.Array.IArray (IArray(..), (!))
import Data.Array (Array)

import Common ((|>))

import qualified Game
import Game(Region, GameMap)

import AI.Always as Always
import AI.Constant as Constant

choose :: (RealFrac a) => a -> a -> a
choose n k = 
    let choose_rec acc n k i 
            | i > k = acc 
            | otherwise = (acc * (n - (k - i))) / i 
                            |> \a -> choose_rec a n k (i + 1)
    in choose_rec 1 n k 1

pBinom :: (RealFloat a) => a -> a -> a -> a
pBinom p n k = (n `choose` k) * (p ** k) * ((1 - p) ** (n - k))

cdfBinom :: (RealFloat a) => a -> a -> a -> a
cdfBinom p n k = [0..(floor k)] |> map realToFrac
                                |> map (pBinom p n) |> sum 

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


-- The probability that a given number of units (ourUnits) with a given
-- attack success probability (prob) will kill all of the enemy units
-- (theirUnits). The `luckFactor` is used to describe the weight
-- of the lucky vs. deterministic attacks.
killsAll :: Double -> Double -> Double -> Double -> Double
killsAll prob ourUnits theirUnits luckFactor = 
    if extraKillsNeeded <= 0 then 1.0
    else if adjustedExtra > ourUnits then 0.0
    else (1 - (cdfBinom prob ourUnits adjustedExtra))
    where guaranteedKills = prob * ourUnits * (1 - luckFactor)
          extraKillsNeeded = theirUnits - guaranteedKills
          adjustedExtra = extraKillsNeeded * (100 * luckFactor)

forceOnlyCapture :: Integer -> Integer -> Double
forceOnlyCapture aUnits dUnits =
    -- A killing all and D killing all are independent so P(A \cap B) = P(A)P(B)
    aKillsAllp * (1 - dKillsAllp)
    where aUnits_ = realToFrac aUnits
          dUnits_ = realToFrac dUnits
          aKillsAllp = killsAll Constant.attackerProb aUnits_ dUnits_ Constant.luckFactor
          dKillsAllp = killsAll Constant.defenderProb dUnits_ aUnits_ Constant.luckFactor

-- The probability that we'll capture the given region 'r' with 
-- the given number 'a_units' units according to the region state
-- in the game map. Specifically the probability that we'll destroy all
-- of the occupying forces and still have at least one unit remaining
capture :: Region -> Integer -> GameMap -> Double
capture r aUnits gm =
    forceOnlyCapture aUnits (Always.units r gm)

-- Garunteed capture with a force of this size or larger assuming attacks
-- have 'p' probability of succeeding.
maxRequiredCaptureForce :: Double -> Integer -> Integer
maxRequiredCaptureForce p f = 
    (realToFrac f) / ((1 - luckFactor) * p) |> ceiling |> (+) 1

forceOnlyMinimumCaptureForce :: Integer -> Double -> Integer
forceOnlyMinimumCaptureForce dUnits con =
    approxBinarySearch con chanceArray
    where maxForce = maxRequiredCaptureForce Constant.attackerProb dUnits 
          forceCapProb i = forceOnlyCapture i dUnits
          chanceArray = 
            (Array.array (1, maxForce) 
                        [ (i, forceCapProb i) | i <- [1..maxForce]])

minimumCaptureForce :: Region -> Double -> GameMap -> Integer
minimumCaptureForce r con gm =
    forceOnlyMinimumCaptureForce (Always.units r gm) con
