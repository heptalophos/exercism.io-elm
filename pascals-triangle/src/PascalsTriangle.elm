module PascalsTriangle exposing (rows)
import List exposing (map)


rows : Int -> List (List (Int))
rows n =
    List.range 1 n
        |> List.map row
    -- flip List.take triangle n


row : Int -> List Int
row n = 
    List.range 0 n |> List.map (binomial n)


binomial : Int -> Int -> Int
binomial n k = 
    case (n, k) of
        (_, 0) -> 
            1
        (0, _) -> 
            0 
        (_, _) -> 
            (binomial (n - 1) (k - 1)) * (n // k) 


-- triangle : List (List (Int ))
-- triangle = 
    -- [map (\y -> binomial (y - 1) (List.range 0 (y - 1))]) []
    -- (List.range 1 map(\y -> (map(\x -> binomial (x - 1) y) (List.range 0 (y - 1)))))


flip : (a -> b -> c) -> b -> a -> c
flip f y x = 
        f x y
