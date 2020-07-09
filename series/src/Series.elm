module Series exposing (slices)


slices : Int -> String -> Result String (List (List Int))
slices size input =
    let 
        intList = 
            input
            |> String.toList
            |> List.map (String.fromChar >> String.toInt)
            |> List.map (Maybe.withDefault 0)
        sublists list =
            List.range 0 (String.length input - size)
            |> List.map ( \x -> 
                                List.drop x list 
                                |> List.take size)
    in 
        if String.isEmpty input then
            Err "series cannot be empty"
        else if size > String.length input then
            Err "slice length cannot be greater than series length"
        else if size == 0 then
            Err "slice length cannot be zero"
        else if size < 0 then
            Err "slice length cannot be negative"
        else
            Ok (sublists intList)