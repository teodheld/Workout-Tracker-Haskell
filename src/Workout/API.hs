{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Workout.API where

import Data.Proxy (Proxy(..))
import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import Servant

import Workout.Domain (Workout)

-- Response type for POST /workouts
data WorkoutResponse = WorkoutResponse
  { workout :: Workout
  , volume  :: Double
  } deriving (Show, Generic)

instance ToJSON WorkoutResponse

-- API, encoded at type level 
type API
  =    "workouts" :> Get '[JSON] [Workout]
  :<|> "workouts" :> ReqBody '[PlainText] String :> Post '[JSON] WorkoutResponse

api :: Proxy API
api = Proxy
