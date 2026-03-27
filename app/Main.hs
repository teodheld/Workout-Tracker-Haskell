-- app/Main.hs
module Main where

import Workout.API (runServer)

main :: IO ()
main = runServer