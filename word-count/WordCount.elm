module WordCount exposing (wordCount)

import Dict exposing (Dict, update)
import String exposing (toLower)
import Regex exposing (Regex, replace, regex)

inc : Maybe Int -> Maybe Int
inc = Maybe.withDefault 0 >> (+) 1 >> Just

nonAlphaNum : Regex
nonAlphaNum = regex "[!@#$%^&*():;\"',.]+"

wordCount : String -> Dict String Int
wordCount sentence =
    sentence
    |> toLower
    |> replace Regex.All nonAlphaNum (\_ -> "")
    |> String.words
    |> List.foldl (\w cnt -> update w inc cnt) Dict.empty
    
