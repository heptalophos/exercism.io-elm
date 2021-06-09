module PythagoreanTriplet exposing (triplets)


type alias Triplet =
    ( Int, Int, Int )


triplets : Int -> List Triplet
triplets n = 
    flip List.concatMap 
         (List.range 1 (n // 2)) 
         (\a ->
            flip List.concatMap 
                 (List.range (a + 1) (n // 2)) 
                 (\b -> 
                    let 
                        c = n - a - b
                    in 
                        if a * a + b * b == c * c
                        then [(a, b, c)]
                        else []))


flip : (a -> b -> c) -> b -> a -> c
flip f a b = f b a