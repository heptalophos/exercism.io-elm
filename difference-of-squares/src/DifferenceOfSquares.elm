module DifferenceOfSquares exposing ( difference
                                    , squareOfSum
                                    , sumOfSquares
                                    )


squareOfSum : Int -> Int
squareOfSum n = 
    (List.range 1 n |> List.foldl (+)  0) ^ 2 


sumOfSquares : Int -> Int
sumOfSquares n = 
    List.range 1 n |> List.map (\x -> x ^ 2) |> List.foldl (+)  0


difference : Int -> Int
difference n = 
    squareOfSum n - sumOfSquares n
