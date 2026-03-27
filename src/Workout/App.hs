{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Workout.App where 

import Control.Monad.Reader (ReaderT, MonadReader, runReaderT)
import Control.Monad.Except (ExceptT, MonadError, runExceptT)
import Control.Monad.IO.Class (MonadIO)
import Data.IORef (IORef, newIORef)
import Servant (ServerError, err400, err500, errBody)
import qualified Data.ByteString.Lazy.Char8 as BL

import Workout.Domain (Workout)

--Environment
data AppEnv = AppEnv 
    { store :: IORef [Workout]}

newAppEnv :: IO AppEnv 
newAppEnv = AppEnv <$> newIORef [] 


--Error type 
data AppError 
    = ParseError String 
    | NotFound 
    | InternalError String

toServerError :: AppError -> ServerError
toServerError (ParseError msg)    = err400 {errBody = BL.pack msg}
toServerError NotFound            = err400 {errBody = BL.pack "Not found"}
toServerError (InternalError msg) = err500 {errBody = BL.pack msg}

--App Monad 

newtype App a = App 
    { runApp :: ReaderT AppEnv (ExceptT AppError IO) a}   
        deriving 
            ( Functor
            , Applicative
            , Monad
            , MonadReader AppEnv
            , MonadError AppError
            , MonadIO 
            )
