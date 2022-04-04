module Transpose exposing (transpose)


transpose : List String -> List String
transpose =
    let
        pad str x y =
            str 
            |> List.repeat (List.length x - List.length y)
            |> (++) y 
        folder r rs =
            List.map2 (::) (pad ' ' rs r) (pad [] r rs)
    in
        List.map String.toList
        >> List.foldr folder []
        >> List.map String.fromList