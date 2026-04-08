{-# LANGUAGE DataKinds #-}

module Workout.Server (runServer) where 

import Control.Monad.Reader (asks, runReaderT)
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (readIORef, modifyIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map 
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec (parse, errorBundlePretty)
import Network.Wai.Handler.Warp (run)
import Servant
import Text.Blaze.Html5 (Html, preEscapedToHtml) 
import qualified Data.Text.IO as TIO               
import qualified Data.Text as T  

import Workout.Domain (Workout(..), Exercise(..), Set(..))
import Workout.Parser (workoutParser)
import Workout.Calculations (totalVolume, setVolume)
import Workout.API (API, WorkoutResponse(..), ProgressResponse(..), ProgressPoint(..), api)
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

getProgress :: App [ProgressResponse]
getProgress = do 
    ref <- asks store
    workouts <- liftIO $ readIORef ref
    let chronological = reverse workouts
        byExercise :: Map Text [Double]
        byExercise = foldr addWorkout Map.empty (zip[1..] chronological)
    return $ map toResponse (Map.toAscList byExercise)
    where 
        addWorkout (_, Workout sets) acc = 
            foldr addSet acc sets

        addSet s acc = 
            let Exercise name = exercise s
                vol           = setVolume s 
            in Map.insertWith (++) name [vol] acc
        
        toResponse (name, vols) = ProgressResponse
            { exerciseName   = name 
            , progressPoint = zipWith ProgressPoint [1..] vols
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
        appServer = getIndex :<|> getWorkouts :<|> postWorkout :<|> getProgress

runServer :: IO() 
runServer = do 
    env <- newAppEnv
    putStrLn "Server running on http://localhost:8080"
    run 8080 (serve api (server env))