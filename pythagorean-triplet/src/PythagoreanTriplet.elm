module PythagoreanTriplet exposing ( triplets )

type alias Triplet = ( Int, Int, Int )

triplets : Int -> List Triplet
triplets n = 
    flip List.concatMap (List.range 1 (n // 3))
         ( \a ->    
                let 
                    b = (//) (n ^ 2 - 2 * n * a) (2 * n - 2 * a)
                    r = modBy (2 * n - 2 * a) (n ^ 2 - 2 * n * a) 
                    c = n - a - b
                in
                    if r == 0 && a <= b then [(a, b, c)] else [] )

flip : (a -> b -> c) -> b -> a -> c
flip f a b = f b a