module Pangram exposing (isPangram)

import String

isPangram : String -> Bool
isPangram sentence = 
    List.all (\letter -> String.contains letter 
                                         (sentence 
                                          |> String.toLower)) 
                                         ("abcdefghijklmnopqrstuvwxyz" 
                                          |> String.split "")
