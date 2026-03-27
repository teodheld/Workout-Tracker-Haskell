{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Workout.API (runServer) where

import Servant
import Network.Wai.Handler.Warp (run)
import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import Text.Megaparsec (parse, errorBundlePretty)
import qualified Data.ByteString.Lazy.Char8 as BL

import Workout.Domain (Workout)
import Workout.Parser (workoutParser)
import Workout.Calculations (totalVolume)

-- ---------------------------------------------------------------------------
-- Response types
-- ---------------------------------------------------------------------------

data WorkoutResponse = WorkoutResponse
  { workout :: Workout
  , volume  :: Double
  } deriving (Show, Generic)

instance ToJSON WorkoutResponse

-- ---------------------------------------------------------------------------
-- API type
-- ---------------------------------------------------------------------------

type API
  =    "workouts" :> Get '[JSON] [Workout]
  :<|> "workouts" :> ReqBody '[PlainText] String :> Post '[JSON] WorkoutResponse

-- ---------------------------------------------------------------------------
-- In-memory store
-- ---------------------------------------------------------------------------

type Store = IORef [Workout]

-- ---------------------------------------------------------------------------
-- Handlers
-- ---------------------------------------------------------------------------

getWorkouts :: Store -> Handler [Workout]
getWorkouts store = liftIO $ readIORef store

postWorkout :: Store -> String -> Handler WorkoutResponse
postWorkout store input =
  case parse workoutParser "<request body>" input of
    Left err ->
      throwError err400 { errBody = BL.pack (errorBundlePretty err) }
    Right w -> do
      liftIO $ modifyIORef store (w :)
      return $ WorkoutResponse
        { workout = w
        , volume  = totalVolume w
        }

-- ---------------------------------------------------------------------------
-- Wiring
-- ---------------------------------------------------------------------------

api :: Proxy API
api = Proxy

server :: Store -> Server API
server store = getWorkouts store :<|> postWorkout store

runServer :: IO ()
runServer = do
  store <- newIORef []
  putStrLn "Server running on http://localhost:8080"
  run 8080 (serve api (server store))