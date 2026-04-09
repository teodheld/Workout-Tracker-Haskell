-- test/Main.hs 
module Main where

import Test.QuickCheck
import Workout.Domain
import Workout.Parser
import Workout.Calculations
import Workout.PrettyPrint
import Text.Megaparsec (parse)
import Data.Either (isRight)
import qualified Data.Text as T

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

instance Arbitrary Workout where 
  arbitrary = Workout <$> listOf1 arbitrary

-- Exercise show is never empty
prop_exerciseShow :: Exercise -> Bool
prop_exerciseShow ex = show ex /= ""

-- Total volume is never negative
prop_totalVolumeNonNegative :: Workout -> Bool
prop_totalVolumeNonNegative w = totalVolume w >= 0

-- Single set volume equals reps * weight
prop_setVolume :: Set -> Bool
prop_setVolume s = setVolume s == fromIntegral (repetitions s) * weight s

-- Total workout volume equals sum of set volumes
prop_totalVolumeIsSum :: Workout -> Bool
prop_totalVolumeIsSum (Workout sets) = totalVolume (Workout sets) == sum (map setVolume sets)

-- Parser accepts all known exercise names
prop_exerciseParser :: Bool
prop_exerciseParser =
  all (\s -> isRight $ parse exerciseParser "" s) ["Squat", "Bench", "Deadlift"]

-- Parser accepts a valid set string
prop_setParser :: Bool
prop_setParser = isRight $ parse setParser "" "Squat 8x100.0"

-- Parser accepts a multi-set workout string
prop_workoutParser :: Bool
prop_workoutParser =
  let input = "Squat 8x100.0\nBench 12x50.0\nDeadlift 6x150.0"
  in isRight (parse workoutParser "" input)

-- Parser computes correct total volume
prop_totalVolumeParser :: Bool
prop_totalVolumeParser =
  let input = "Squat 8x100.0\nBench 12x50.0\nDeadlift 6x150.0"
  in case parse workoutParser "" input of
       Right w -> totalVolume w == (8*100.0 + 12*50.0 + 6*150.0)
       Left _  -> False

-- Roundtrip: pretty printing a workout and parsing it gives back the same workout
prop_roundtrip :: Workout -> Bool
prop_roundtrip w = parse workoutParser "" (prettyWorkout w) == Right w


-- Main som kjører alle QuickCheck tests
main :: IO ()
main = do
  putStrLn "=== Testing Exercise ==="
  quickCheck prop_exerciseShow
  quickCheck prop_exerciseParser

  putStrLn "\n=== Testing Set ==="
  quickCheck prop_setParser
  quickCheck prop_setVolume

  putStrLn "\n=== Testing Workout ==="
  quickCheck prop_totalVolumeNonNegative
  quickCheck prop_totalVolumeIsSum
  quickCheck prop_workoutParser
  quickCheck prop_totalVolumeParser

  putStrLn "\n=== Roundtrip ==="
  quickCheck prop_roundtrip