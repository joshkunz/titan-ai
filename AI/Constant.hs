module AI.Constant where

-- The fraction we allocate at, 2 = 1/2 of the armies each weight
allocationFactor = 2

-- The minimum income we can obtain
minIncome = 5

-- The minimum number of units that must always remain in a territory
minReserveForce = 1

-- The fraction of the income that goes towards territories that can capture
-- their neighboring territories
captureFraction = 0.8

-- Probability an attacker destroys a defender and vice versa
attackerProb = 0.6
defenderProb = 0.7

-- Weight of lucky kills, assume det. kills are weighted 1 - <this value>
luckFactor = 0.16

-- Default minimum confidence required for us to beleive that we'll succeed
-- in capturing a territory.
minCaptureConfidence = 0.90
