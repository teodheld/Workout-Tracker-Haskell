{-# LANGUAGE DeriveGeneric #-}

module Workout.Domain where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)

data Exercise = Exercise Text 
  deriving (Show, Eq, Generic)

instance ToJSON Exercise
instance FromJSON Exercise

data Set = Set
  { exercise    :: Exercise
  , repetitions :: Int     
  , weight      :: Double
  } deriving (Eq, Show, Generic)

instance ToJSON Set
instance FromJSON Set

newtype Workout = Workout [Set]
  deriving (Eq, Show, Generic)

instance ToJSON Workout
instance FromJSON Workout