module Workout.Parser where 

import Workout.Domain
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Text as T

import Data.Void

type Parser = Parsec Void String

exerciseParser :: Parser Exercise
exerciseParser = do 
    first <- upperChar 
    rest <- many alphaNumChar
    return $ Exercise(T.pack (first : rest))

setParser :: Parser Set
setParser = do
    exercise <- exerciseParser
    space1 
    reps <- L.decimal
    char 'x'
    weight <- L.float
    return (Set exercise reps weight)

workoutParser :: Parser Workout
workoutParser = do 
    sets <- some (setParser <* optional eol)
    eof 
    return (Workout sets)


