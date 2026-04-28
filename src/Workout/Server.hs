{-# LANGUAGE DataKinds #-}

module Workout.Server (runServer) where 

import Control.Monad.Reader (asks, runReaderT)
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (readIORef, modifyIORef, writeIORef)
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
import qualified Workout.Calculations as Calc
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
                , volume = Calc.volume w
                }

getProgress :: App [ProgressResponse]
getProgress = do 
    ref <- asks store
    workouts <- liftIO $ readIORef ref
    let byExercise :: Map Text [Double]
        byExercise = foldr addWorkout Map.empty (zip[1..] workouts)
    return $ map toResponse (Map.toAscList byExercise)
    where 
        addWorkout (_, Workout sets) acc = 
            let sessionTotals :: Map Text Double 
                sessionTotals = foldr addSet Map.empty sets
            in Map.unionWith (++) acc (Map.map (:[]) sessionTotals)

        addSet s acc = 
            let Exercise name = exercise s
                vol           = Calc.volume s 
            in Map.insertWith (+) name vol acc
        
        toResponse (name, vols) = ProgressResponse
            { exerciseName   = name 
            , progressPoint = zipWith ProgressPoint [1..] vols
            }

deleteWorkout :: Int -> App NoContent
deleteWorkout idx = do 
    ref <- asks store 
    workouts <- liftIO $ readIORef ref
    if idx < 0 || idx >= length workouts 
        then throwError NotFound 
        else do 
            let updated = take idx workouts ++ drop (idx + 1) workouts 
            liftIO $ writeIORef ref updated
            return NoContent
            
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
        appServer = getIndex :<|> getWorkouts :<|> postWorkout :<|> deleteWorkout :<|> getProgress

runServer :: IO() 
runServer = do 
    env <- newAppEnv
    putStrLn "Server running on http://localhost:8080"
    run 8080 (serve api (server env))