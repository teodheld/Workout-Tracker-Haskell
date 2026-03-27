-- app/Main.hs
module Main where

import Workout.Server (runServer)

main :: IO ()
main = runServer