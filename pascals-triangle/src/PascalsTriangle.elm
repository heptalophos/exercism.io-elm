module PascalsTriangle exposing (rows)

import List exposing (map, range)


rows : Int -> List (List (Int))
rows n =
    let
        row x = 
            range 0 x |> map (binomial x)
    in
        range 0 (n - 1) |> map row
    let
        triangle =
            [map (binomial (x - 1)) range 0 (x - 1) | x <- range 1]   
    in
        flip List.take triangle n
    -- for a triangle lazy Seq which in haskell is very easy 
    -- mapping binomial to the range using a list comprehension
    -- [map (binomial (x - 1)) [0..(x - 1)] | x <- [1..]]


-- row : Int -> List Int
-- row n = 
--     range 0 n |> map (binomial n)


binomial : Int -> Int -> Int
binomial n k =
    case (n, k) of
        (_, 0) -> 1
        (0, _) -> 0 
        (_, _) -> (binomial (n - 1) (k - 1)) * n // k 
