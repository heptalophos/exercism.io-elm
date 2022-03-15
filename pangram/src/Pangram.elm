module Pangram exposing (isPangram)

import String

isPangram : String -> Bool
isPangram sentence = 
    let 
        normal = 
            sentence |> String.toLower
        alphaList =
            "abcdefghijklmnopqrstuvwxyz" |> String.split ""
    in
        List.all (\letter -> String.contains letter normal) alphaList
