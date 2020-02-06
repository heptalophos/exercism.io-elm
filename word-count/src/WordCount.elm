module WordCount exposing (wordCount)

import Dict exposing (Dict, 
                      update)
import String exposing (toLower)
import Regex exposing (Match, 
                       replace, 
                       fromString)


wordCount : String -> Dict String Int
wordCount sentence =
    sentence
    |> toLower 
    |> replaceRegex nonAlphaNum (\_ -> " ")
    |> replaceRegex boundaryApostroph (\_ -> " ")
    |> String.words
    |> List.foldl (\w cnt -> update w inc cnt) 
                  Dict.empty


nonAlphaNum : String
nonAlphaNum = "[!@#$%^&*():;\",.\n]"


boundaryApostroph : String
boundaryApostroph = "\\W+[']|[']\\W+|^[']|[']$"


-- Auxiliary 

inc : Maybe Int -> Maybe Int
inc = Maybe.withDefault 0 >> (+) 1 >> Just


replaceRegex : String -> (Match -> String) -> String -> String
replaceRegex myRegex replacer string =
    case Regex.fromString myRegex of
        Nothing ->
            string
        Just regex ->
            Regex.replace regex replacer string