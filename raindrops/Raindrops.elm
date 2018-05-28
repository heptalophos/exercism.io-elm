module Raindrops exposing (raindrops)

import String


raindrops : Int -> String
raindrops number = List.foldr (\(x, y) acc -> 
                        if number % x == 0 then y::acc else acc) [] [(3, "Pling"), (5, "Plang"), (7, "Plong")]
                  |> (\pln_gs -> if List.isEmpty pln_gs then toString number else String.concat pln_gs)

