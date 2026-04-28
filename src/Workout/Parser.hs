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

repsWeightParser :: Parser (Int, Double)
repsWeightParser = do 
    reps <- L.decimal
    char 'x'
    weight <- L.float 
    return (reps, weight)

setLineParser :: Parser [Set]
setLineParser = do
    exercise <- exerciseParser
    space1 
    first <- repsWeightParser
    rest <- many (string ", " *> repsWeightParser)
    let allRepsWeight = first : rest
    return [Set exercise reps w | (reps, w) <- allRepsWeight]

workoutParser :: Parser Workout
workoutParser = do 
    sets <- some (setLineParser <* optional eol)
    eof 
    return $ Workout (concat sets)


