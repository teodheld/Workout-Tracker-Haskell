-- test/Main.hs 
module Main where

import Test.QuickCheck
import Workout.Domain
import Workout.Parser
import Workout.Calculations
import Text.Megaparsec (parse)
import Data.Either (isRight)

-- Arbitrary instance for Exercise, kun i test
instance Arbitrary Exercise where
  arbitrary = do 
    first <- elements ['A' .. 'Z']
    rest <- listOf (elements(['a'..'z'] ++ ['A'..'Z']))
    return $ Exercise (T.pack (first : rest))

-- Arbitrary instance for Set, kun i test
instance Arbitrary Set where
  arbitrary = do
    ex <- arbitrary
    reps <- choose (1, 20)
    weight <- choose (20, 200)
    return $ Set ex reps weight

-- Arbitrary instance for Workout
instance Arbitrary Workout where
  arbitrary = do
    sets <- listOf arbitrary
    return $ Workout sets

-- Property: show Exercise is not empty
prop_exerciseShow :: Exercise -> Bool
prop_exerciseShow ex = show ex /= ""

-- Property: totalVolume of Workout is never negative
prop_totalVolumeNonNegative :: Workout -> Bool
prop_totalVolumeNonNegative w = totalVolume w >= 0

-- Property: exerciseParser parses valid exercises
prop_exerciseParser :: Bool
prop_exerciseParser =
  all (\s -> isRight $ parse exerciseParser "" s) ["Squat","Bench","Deadlift"]

-- Property: setParser parses a valid set string
prop_setParser :: Bool
prop_setParser =
  isRight $ parse setParser "" "Squat 8x100"

-- Property: workoutParser parses a multi-set workout string
prop_workoutParser :: Bool
prop_workoutParser =
  let input = "Squat 8x100\nBench 12x50\nDeadlift 6x150"
  in isRight (parse workoutParser "" input)

-- Property: workoutParser gives correct total volume
prop_totalVolumeParser :: Bool
prop_totalVolumeParser =
  let input = "Squat 8x100\nBench 12x50\nDeadlift 6x150"
      parsed = parse workoutParser "" input
  in case parsed of
       Right w -> totalVolume w == (8*100 + 12*50 + 6*150)
       Left _  -> False

-- Main som kjører alle QuickCheck tests
main :: IO ()
main = do
  putStrLn "=== Testing Exercise ==="
  quickCheck prop_exerciseShow
  quickCheck prop_exerciseParser

  putStrLn "\n=== Testing Set ==="
  quickCheck prop_setParser

  putStrLn "\n=== Testing Workout ==="
  quickCheck prop_totalVolumeNonNegative
  quickCheck prop_workoutParser
  quickCheck prop_totalVolumeParser