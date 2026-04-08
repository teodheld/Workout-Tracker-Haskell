module Workout.Parser where 

import Workout.Domain
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Void

type Parser = Parsec Void String

exerciseParser :: Parser Exercise
exerciseParser =
        (Squat <$ string "Squat")
    <|> (Bench <$ string "Bench")
    <|> (Deadlift <$ string "Deadlift")

setParser :: Parser Set
setParser = do
    exercise <- exerciseParser
    space1 
    reps <- L.decimal
    char 'x'
    weight <- L.decimal
    return (Set exercise reps weight)

workoutParser :: Parser Workout
workoutParser = Workout <$> some (setParser <* optional eol)


