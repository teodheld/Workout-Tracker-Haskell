{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Workout.API where

import Data.Proxy (Proxy(..))
import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import Data.Text (Text)
import Servant
import Servant.HTML.Blaze (HTML)
import Text.Blaze.Html5 (Html)

import Workout.Domain (Workout)

-- Response type for POST /workouts
data WorkoutResponse = WorkoutResponse
  { workout :: Workout
  , volume  :: Double
  } deriving (Show, Generic)

instance ToJSON WorkoutResponse

data ProgressPoint = ProgressPoint
  { sessionIndex :: Int
  , vol          :: Double
  } deriving (Show, Generic)

instance ToJSON ProgressPoint

data ProgressResponse = ProgressResponse
  { exerciseName  :: Text 
  , progressPoint :: [ProgressPoint]
  } deriving(Show, Generic)

instance ToJSON ProgressResponse

-- API, encoded at type level 
type API
  =    Get '[HTML] Html
  :<|> "workouts" :> Get '[JSON] [Workout]
  :<|> "workouts" :> ReqBody '[PlainText] String :> Post '[JSON] WorkoutResponse
  :<|> "progress" :> Get '[JSON] [ProgressResponse]

api :: Proxy API
api = Proxy
