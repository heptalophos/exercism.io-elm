module Pangram exposing (isPangram)

import String

isPangram : String -> Bool
isPangram sentence = 
    let 
        normal = 
            sentence |> String.toLower
        alphabet =
            "abcdefghijklmnopqrstuvwxyz" |> String.split ""
    in
        List.all (\letter -> String.contains letter normal) alphabet
