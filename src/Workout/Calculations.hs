module Workout.Calculations where

import Workout.Domain

class HasVolume a where
    volume :: a -> Double

instance HasVolume Set where 
    volume s = fromIntegral (repetitions s) * weight s

instance HasVolume Workout where
    volume (Workout sets) = sum(map volume sets)



