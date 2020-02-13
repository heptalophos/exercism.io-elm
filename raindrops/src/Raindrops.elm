module Raindrops exposing (raindrops)


raindrops : Int -> String
raindrops number = 
    List.foldr (\(x, y) acc -> 
                    if modBy x number == 0 
                    then y::acc 
                    else acc) 
                    [] 
                    [(3, "Pling"), 
                     (5, "Plang"), 
                     (7, "Plong")]
    |> (\drops -> 
            if (List.isEmpty drops |> not) 
            then String.concat drops
            else String.fromInt number)