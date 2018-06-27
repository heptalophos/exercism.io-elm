module Anagram exposing (detect)

isAnagram : String -> String -> Bool
isAnagram word possible = (String.toLower word /= String.toLower possible) 
                        && (sorted (String.toLower word) == sorted (String.toLower possible))

sorted : String -> List Char
sorted text = text
              |> String.toList
              |> List.sort

detect : String -> List String -> List String
detect word candidates =
    List.filter (\x -> isAnagram word x) candidates
