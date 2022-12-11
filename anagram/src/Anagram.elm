module Anagram exposing ( detect )


sorted : String -> List Char
sorted text = 
    text |> String.toList |> List.sort
    

isAnagram : String -> String -> Bool
isAnagram word possible = 
    let
        toLower = String.toLower
    in
        (/=) (toLower word) (toLower possible) 
        && 
        (==) (sorted (toLower word)) (sorted (toLower possible))


detect : String -> List String -> List String
detect word candidates =
    List.filter (\x -> isAnagram word x) candidates