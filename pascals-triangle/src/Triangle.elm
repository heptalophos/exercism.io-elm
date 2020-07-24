module Triangle exposing (rows)
import List exposing (map)


rows : Int -> List (List Int)
rows n =
    flip List.take triangle n


binomial : Int -> Int -> Int
-- binomial _ 0 = 
--     1
-- binomial 0 _ = 
--     0
binomial n k = 
    case (n, k) of
        (_, 0) -> 
            1
        (0, _) ->  
            0 
        (_, _) ->
            binomial (n-1) (k-1) * ((//) n k) 


triangle : List (List Int)
triangle = 
    map (\x -> binomial (x - 1) (List.range 0 (x - 1))) []

flip : (a -> b -> c) -> b -> a -> c
flip f y x = f x y