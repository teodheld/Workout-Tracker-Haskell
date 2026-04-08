{-# LANGUAGE DataKinds #-}

module Workout.Server (runServer) where 

import Control.Monad.Reader (asks, runReaderT)
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (readIORef, modifyIORef)
import Text.Megaparsec (parse, errorBundlePretty)
import Network.Wai.Handler.Warp (run)
import Servant
import Text.Blaze.Html5 (Html, preEscapedToHtml) 
import qualified Data.Text.IO as TIO               
import qualified Data.Text as T  

import Workout.Domain (Workout)
import Workout.Parser (workoutParser)
import Workout.Calculations (totalVolume)
import Workout.API (API, WorkoutResponse(..), api)
import Workout.App (App(..), AppEnv(..), AppError(..), newAppEnv, toServerError)

-- Handlers

getIndex :: App Html
getIndex = do
    contents <- liftIO $ readFile "static/index.html"
    liftIO $ putStrLn $ "Read " ++ show(length contents) ++ " chars from index.html" 
    return $ preEscapedToHtml (T.pack contents) 

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
        appServer = getIndex :<|> getWorkouts :<|> postWorkout

runServer :: IO() 
runServer = do 
    env <- newAppEnv
    putStrLn "Server running on http://localhost:8080"
    run 8080 (serve api (server env))