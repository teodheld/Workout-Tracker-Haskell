module Workout.PrettyPrint where

import Workout.Domain
import Data.List(groupBy)
import qualified Data.Text as T
import qualified Workout.Calculations as Calc

prettyExercise :: Exercise -> String 
prettyExercise (Exercise name) = T.unpack name

prettyRepsWeight :: Set -> String 
prettyRepsWeight s = show (repetitions s) ++ "x" ++ show(weight s)

prettySet :: [Set] -> String 
prettySet [] = ""
prettySet (s:ss) = prettyExercise (exercise s)
                ++ " "
                ++ prettyRepsWeight s
                ++ concatMap (\x -> ", " ++ prettyRepsWeight x) ss

prettyWorkout :: Workout -> String 
prettyWorkout (Workout sets) = 
    unlines $ map prettySet (groupBy sameExercise sets)
    where
        sameExercise a b = exercise a == exercise b

