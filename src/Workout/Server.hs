{-# LANGUAGE DataKinds #-}

module Workout.Server (runServer) where 

import Control.Monad.Reader (asks, runReaderT)
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (readIORef, modifyIORef)
import Text.Megaparsec (parse, errorBundlePretty)
import Network.Wai.Handler.Warp (run)
import Servant

import Workout.Domain (Workout)
import Workout.Parser (workoutParser)
import Workout.Calculations (totalVolume)
import Workout.API (API, WorkoutResponse(..), api)
import Workout.App (App(..), AppEnv(..), AppError(..), newAppEnv, toServerError)

-- Handlers
getWorkouts :: App [Workout]
getWorkouts = do 
    ref <- asks store 
    liftIO $ readIORef ref

postWorkout :: String -> App WorkoutResponse
postWorkout input = 
    case parse workoutParser "<request body>" input of
        Left err -> 
            throwError $ ParseError (errorBundlePretty err)
        Right w -> do 
            ref <- asks store 
            liftIO $ modifyIORef ref (w : )
            return $ WorkoutResponse
                { workout = w
                , volume = totalVolume w
                }

-- App –> Handler 
appToHandler :: AppEnv -> App a -> Handler a 
appToHandler env app = 
    runApp app 
        & (`runReaderT` env)
        & runExceptT
        & liftIO 
        >>= either (throwError . toServerError) return
    where (&) = flip ($)


--Wiring 

server :: AppEnv -> Server API 
server env = hoistServer api (appToHandler env) appServer
    where 
        appServer :: ServerT API App 
        appServer = getWorkouts :<|> postWorkout

runServer :: IO() 
runServer = do 
    env <- newAppEnv
    putStrLn "Server running on http://localhost:8080"
    run 8080 (serve api (server env))