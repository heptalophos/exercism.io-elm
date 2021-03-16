module Transpose exposing (transpose)


transpose : List String -> List String
transpose =
    let
        pad str x y =
            str 
            |> List.repeat (List.length x - List.length y)
            |> (++) y 
        folder l m =
            List.map2 (::) (pad ' ' m l) (pad [] l m)
    in
        List.map String.toList
        >> List.foldr folder []
        >> List.map String.fromList