module Etl exposing (transform)

import Dict exposing (Dict)


transform : Dict Int (List String) -> Dict String Int
transform input =
    let 
        pairs : (Int, List String) -> List (String, Int)
        pairs (score, letters) = List.map(\letter -> (String.toLower letter, score)) letters
    in 
        Dict.toList input 
        |> List.concatMap pairs 
        |> Dict.fromList
