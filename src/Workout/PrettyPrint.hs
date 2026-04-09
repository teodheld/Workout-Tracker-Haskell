module Workout.PrettyPrint where

import Workout.Domain
import qualified Data.Text as T

prettyExercise :: Exercise -> String 
prettyExercise (Exercise name) = T.unpack name

prettySet :: Set -> String 
prettySet s = prettyExercise (exercise s)
            ++ " "
            ++ show (repetitions s)
            ++ "x"
            ++ show (weight s)

prettyWorkout :: Workout -> String 
prettyWorkout (Workout sets) = unlines (map prettySet sets)

