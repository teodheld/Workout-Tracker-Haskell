module Workout.Calculations where

import Workout.Domain

setVolume :: Set -> Double
setVolume s = fromIntegral (repetitions s) * weight s

totalVolume :: Workout -> Double
totalVolume (Workout sets) = sum[setVolume s| s <- sets]



