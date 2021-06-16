module Triangle exposing (Triangle(..), triangleKind)


import Set


type Triangle
    = Equilateral
    | Isosceles
    | Scalene


triangleKind : number -> number -> number -> Result String Triangle
triangleKind x y z =
        if List.any (\a -> a <= 0) [x, y, z] then
            Result.Err "Invalid lengths"
        else
            if 2 * List.foldl max x [x, y, z] >= 
               List.sum [x, y, z] then
            Result.Err "Violates inequality"
        else 
            case Set.size <| Set.fromList [x, y, z] of
                1 -> Result.Ok Equilateral
                2 -> Result.Ok Isosceles
                3 -> Result.Ok Scalene
                _ -> Result.Err "What?"
