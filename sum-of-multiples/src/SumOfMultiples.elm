module SumOfMultiples exposing (sumOfMultiples)

import Set exposing (Set)

sumOfMultiples : List Int -> Int -> Int
sumOfMultiples divisors limit =
    List.concatMap (\d -> List.range 1 ((limit - 1) // d)
                          |> List.map ((*) d))
                   divisors 
    |> Set.fromList
    |> Set.foldl (+) 0 
