module PascalsTriangle exposing (rows)

import List exposing (map, range)


rows : Int -> List (List (Int))
rows n = 
    range 0 (n - 1) |> map(\x -> range 0 x |> map (binomial x))
    -- let -- Oops, doesn't work in elm 0.19.1 as 
           -- list comprehensions have been removed
    --     flip : (a -> b -> c) -> b -> a -> c
    --     flip f y x = f x y
    --     triangle : List (List (Int -> Int))
    --     triangle =
    --       [map (binomial (x - 1)) [0..(x - 1)] | x <- [1..]]   
    -- in
    --     flip List.take triangle n


binomial : Int -> Int -> Int
binomial n k =
    case (n, k) of
        (_, 0) -> 1
        (0, _) -> 0 
        (_, _) -> (binomial (n - 1) (k - 1)) * n // k 
