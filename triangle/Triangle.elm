module Triangle exposing (Triangle(..), triangleKind, version)

import Set

type Triangle
    = Degenerate
    | Equilateral
    | Isosceles
    | Scalene


version : Int
version =
    2


triangleKind : comparable -> comparable -> comparable -> Result String Triangle
triangleKind x y z =
        if (x <= 0 || y <= 0 || z <= 0) then
            Result.Err "Invalid side"
        else
        if (x + y < z || x + z < y || y + z < x) then
            Result.Err "Violated Triangle Inequality"
        else 
            if (x + y == z || x + z == y || y + z == x) then
            Result.Ok Degenerate
        else 
            case Set.size(Set.fromList [x, y, z]) of
                1 -> Result.Ok Equilateral
                2 -> Result.Ok Isosceles
                _ -> Result.Ok Scalene